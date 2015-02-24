(in-package :cl-user)
(defpackage dexador.backend.usocket
  (:nicknames :dex.usocket)
  (:use :cl
        :dexador.util)
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
                :copy-stream
                :if-let)
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
(define-compiler-macro write-header (stream name value)
  `(progn
     ,(if (and (constantp name)
               (typep name '(or symbol string)))
          `(write-sequence ,(ascii-string-to-octets (string-capitalize name)) ,stream)
          `(write-sequence (ascii-string-to-octets (string-capitalize ,name)) ,stream))
     (write-sequence #.(ascii-string-to-octets ": ") ,stream)
     ,(if (and (constantp value)
               (stringp value))
          `(write-sequence ,(ascii-string-to-octets (string value)) ,stream)
          `(write-sequence (ascii-string-to-octets ,value) ,stream))
     (write-sequence +crlf+ ,stream)))

(defun-speedy read-until-crlf (stream)
  (with-fast-output (buf)
    (tagbody
     read-cr
       (loop for byte of-type (or (unsigned-byte 8) null) = (read-byte stream nil nil)
             if byte
               do (fast-write-byte byte buf)
             else
               do (go eof)
             until (= byte (char-code #\Return)))

     read-lf
       (let ((next-byte (read-byte stream nil nil)))
         (declare (type (or (unsigned-byte 8) null) next-byte))
         (cond
           ((null next-byte)
            (go eof))
           ((= next-byte (char-code #\Newline))
            (fast-write-byte next-byte buf))
           ((= next-byte (char-code #\Return))
            (fast-write-byte next-byte buf)
            (go read-lf))))
     eof)))

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
                            content headers
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

    (macrolet ((write-header* (name value)
                 (let ((tmp (gensym)))
                   `(if-let ((,tmp (assoc ,name headers :test #'eq)))
                      (write-header stream ,name (princ-to-string (cdr ,tmp)))
                      (write-header stream ,name ,value)))))
      (write-first-line method uri version stream)
      (write-header* :user-agent #.*default-user-agent*)
      (write-header* :host (uri-host uri))
      (write-header* :accept "*/*")
      (when (and (= version 1.1)
                 (not (assoc :connection headers :test #'eq)))
        (if keep-alive
            (write-header stream :connection "keep-alive")
            (write-header stream :connection "close")))
      (etypecase content
        (null)
        (string
         (write-header* :content-type "application/x-www-form-urlencoded")
         (write-header* :content-length (princ-to-string (length content))))
        (pathname
         (write-header* :content-type (mimes:mime content))
         (if-let ((content-length (assoc :content-length headers :test #'eq)))
           (write-header stream :content-length (princ-to-string (cdr content-length)))
           (with-open-file (in content)
             (write-header stream :content-length (princ-to-string (file-length in))))))))

    ;; Custom headers
    (loop for (name . value) in headers
          unless (member name '(:user-agent :host :accept
                                :content-type :content-length) :test #'eq)
            do (write-header stream name value))
    (write-sequence +crlf+ stream)
    (force-output stream)

    ;; Sending the content
    (etypecase content
      (null)
      (string (write-string content stream))
      (pathname (with-open-file (in content)
                  (copy-stream in stream))))
    (force-output stream)

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
