(in-package :cl-user)
(defpackage dexador.backend.usocket
  (:nicknames :dex.usocket)
  (:use :cl
        :dexador.util)
  #+(or sbcl ccl cmu allegro)
  (:import-from #+sbcl :sb-cltl2
                #+ccl :ccl
                #+cmu :ext
                #+allegro :sys
                :variable-information)
  (:import-from :usocket
                :socket-connect
                :socket-stream)
  (:import-from :fast-http
                :make-http-response
                :make-parser
                :http-status
                :http-headers)
  (:import-from :fast-io
                :make-output-buffer
                :finish-output-buffer
                :with-fast-output
                :fast-write-sequence
                :fast-write-byte)
  (:import-from :quri
                :uri-host
                :uri-port
                :uri-path
                :uri-query
                :url-encode-params)
  (:import-from :alexandria
                :copy-stream)
  (:export :request))
(in-package :dexador.backend.usocket)

(defun write-header (stream name value)
  (flet ((write-ascii-string (string stream)
           (loop for char of-type character across string
                 do (write-byte (char-code char) stream))))
    (if (typep name 'octets)
        (write-sequence name stream)
        (write-ascii-string (string-capitalize name) stream))
    (write-sequence #.(ascii-string-to-octets ": ") stream)
    (if (typep value 'octets)
        (write-sequence value stream)
        (write-ascii-string value stream))
    (write-sequence +crlf+ stream)))

#+(or sbcl ccl cmu allegro)
(define-compiler-macro write-header (&environment env stream name value)
  `(progn
     ,(if (or (and (constantp name)
                   (typep name '(or symbol string)))
              (and (symbolp name)
                   (subtypep (assoc 'type (nth-value 2 (variable-information name env)))
                             '(or symbol string))))
          `(write-sequence ,(ascii-string-to-octets (string-capitalize name)) ,stream)
          `(write-sequence (if (typep ,name '(or string symbol))
                               (ascii-string-to-octets (string-capitalize ,name))
                               ,name)
                           ,stream))
     (write-sequence #.(ascii-string-to-octets ": ") ,stream)
     ,(if (or (and (constantp value)
                   (stringp value))
              (and (symbolp value)
                   (subtypep (assoc 'type (nth-value 2 (variable-information value env)))
                             '(or symbol string))))
          `(write-sequence ,(ascii-string-to-octets (string value)) ,stream)
          `(write-sequence (if (typep ,value '(or string symbol))
                               (ascii-string-to-octets ,value)
                               ,value)
                           ,stream))
     (write-sequence +crlf+ ,stream)))

(defun-speedy read-until-crlf (stream)
  (with-fast-output (buf)
    (tagbody
     read-cr
       (loop for byte of-type (unsigned-byte 8) = (read-byte stream nil nil)
             when byte
             do (fast-write-byte byte buf)
             until (= byte (char-code #\Return)))

     read-lf
       (let ((next-byte (read-byte stream nil nil)))
         (declare (type (unsigned-byte 8) next-byte))
         (cond
           ((null next-byte))
           ((= next-byte (char-code #\Newline))
            (fast-write-byte next-byte buf))
           ((= next-byte (char-code #\Return))
            (fast-write-byte next-byte buf)
            (go read-lf)))))))

(defun write-first-line (method uri version stream)
  (write-sequence (ascii-string-to-octets (string method)) stream)
  (write-byte #.(char-code #\Space) stream)
  (write-sequence (ascii-string-to-octets (format nil "~A~:[~;~:*?~A~]"
                                                  (uri-path uri)
                                                  (uri-query uri)))
                  stream)
  (write-byte #.(char-code #\Space) stream)
  (write-sequence (ecase version
                    (1.1 #.(ascii-string-to-octets "HTTP/1.1"))
                    (1.0 #.(ascii-string-to-octets "HTTP/1.0"))) stream)
  (write-sequence +crlf+ stream))

(defun-careful request (uri &key (method :get) (version 1.1)
                            content
                            keep-alive socket)
  (let* ((uri (quri:uri uri))
         (content (if (consp content)
                      (quri:url-encode-params content)
                      content))
         (socket (or socket
                     (usocket:socket-connect (uri-host uri)
                                             (uri-port uri)
                                             :element-type '(unsigned-byte 8))))
         (stream (usocket:socket-stream socket)))

    (write-first-line method uri version stream)
    (write-header stream :user-agent #.*default-user-agent*)
    (write-header stream :host (uri-host uri))
    (write-header stream :accept "*/*")
    (when (= version 1.1)
      (if keep-alive
          (write-header stream :connection "keep-alive")
          (write-header stream :connection "close")))
    (etypecase content
      (null)
      (string
       (write-header stream :content-type "application/x-www-form-urlencoded")
       (write-header stream :content-length (princ-to-string (length content))))
      (pathname
       (write-header stream :content-type (mimes:mime content))
       (with-open-file (in content)
         (write-header stream :content-length (princ-to-string (file-length in))))))
    (write-sequence +crlf+ stream)
    (force-output stream)

    ;; Sending the content
    (etypecase content
      (null)
      (string (write-string content stream))
      (pathname (with-open-file (in content)
                  (copy-stream in stream))))
    (let* ((http (make-http-response))
           (body (make-output-buffer))
           (finishedp nil)
           (parser (make-parser http
                                :body-callback
                                (lambda (data start end)
                                  (fast-write-sequence data body start end))
                                :finish-callback
                                (lambda ()
                                  (setq finishedp t)))))
      (loop for buf of-type octets  = (read-until-crlf stream)
            do (funcall parser buf)
            until (or finishedp
                      (zerop (length buf))))
      (unless keep-alive
        (usocket:socket-close socket))
      (values (finish-output-buffer body)
              (http-status http)
              (http-headers http)
              (when keep-alive
                socket)))))
