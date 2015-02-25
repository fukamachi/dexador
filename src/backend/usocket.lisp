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
  (:import-from :trivial-mimes
                :mime)
  (:import-from :quri
                :uri-host
                :uri-port
                :url-encode-params)
  (:import-from :alexandria
                :copy-stream
                :if-let)
  (:export :request))
(in-package :dexador.backend.usocket)

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

(defun read-response (stream)
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
    (values http (finish-output-buffer body))))

(defun print-verbose-data (&rest data)
  (format t "~&>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>~%")
  (dolist (d data)
    (map nil (lambda (byte)
               (princ (code-char byte)))
         d))
  (format t "~&>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>~%"))

(defun-careful request (uri &key (method :get) (version 1.1)
                            content headers
                            (timeout *default-timeout*) keep-alive
                            (max-redirects 5)
                            socket verbose)
  (let* ((uri (quri:uri uri))
         (content (if (consp content)
                      (quri:url-encode-params content)
                      content))
         (socket (or socket
                     (usocket:socket-connect (uri-host uri)
                                             (uri-port uri)
                                             :timeout timeout
                                             :element-type '(unsigned-byte 8))))
         (stream (usocket:socket-stream socket))
         (first-line-data
           (with-fast-output (buffer)
             (write-first-line method uri version buffer)))
         (headers-data
           (flet ((write-header* (name value)
                    (let ((header (assoc name headers)))
                      (if header
                          (when (cdr header)
                            (write-header name (cdr header)))
                          (write-header name value)))))
             (with-header-output (buffer)
               (write-header* :user-agent #.*default-user-agent*)
               (write-header* :host (uri-host uri))
               (write-header* :accept "*/*")
               (when (and keep-alive
                          (= version 1.0))
                 (write-header* :connection "keep-alive"))
               (etypecase content
                 (null)
                 (string
                  (write-header* :content-type "application/x-www-form-urlencoded")
                  (write-header* :content-length (length content)))
                 (pathname
                  (write-header* :content-type (mimes:mime content))
                  (if-let ((content-length (assoc :content-length headers :test #'eq)))
                    (write-header :content-length (cdr content-length))
                    (with-open-file (in content)
                      (write-header :content-length (file-length in))))))

               ;; Custom headers
               (loop for (name . value) in headers
                     unless (member name '(:user-agent :host :accept
                                           :connection
                                           :content-type :content-length) :test #'eq)
                       do (write-header name value))
               (fast-write-sequence +crlf+ buffer)))))
    (write-sequence first-line-data stream)
    (write-sequence headers-data stream)
    (force-output stream)
    (when verbose
      (print-verbose-data first-line-data headers-data))

    ;; Sending the content
    (etypecase content
      (null)
      (string (write-string content stream))
      (pathname (with-open-file (in content)
                  (copy-stream in stream))))
    (force-output stream)

    (tagbody
     start-reading
       (multiple-value-bind (http body)
           (read-response stream)
         (let ((status (http-status http))
               (response-headers (http-headers http)))
           (when (and (member status '(301 302 303 307) :test #'=)
                      (member method '(:get :head) :test #'eq)
                      (gethash "location" response-headers))
             (let* ((location-uri (quri:uri (gethash "location" response-headers)))
                    (next-first-line-data
                      (with-fast-output (buffer)
                        (write-first-line method location-uri version buffer))))
               (if (string= (uri-host location-uri)
                            (uri-host uri))
                   (progn
                     (when verbose
                       (print-verbose-data next-first-line-data headers-data))
                     (write-sequence next-first-line-data stream)
                     (write-sequence headers-data stream)
                     (force-output stream)
                     (decf max-redirects)
                     (when (= 0 max-redirects)
                       (error "Request exceeded the limit of redirects"))
                     (go start-reading))
                   (progn
                     (usocket:socket-close socket)
                     (return-from request
                       (request location-uri
                                :method method
                                :version version
                                :content content
                                :headers (nconc `((:host . ,(uri-host location-uri))) headers)
                                :timeout timeout
                                :keep-alive keep-alive
                                :max-redirects (1- max-redirects)
                                :verbose verbose))))))
           (unless keep-alive
             (usocket:socket-close socket))
           (return-from request
             (values body
                     status
                     response-headers
                     (when keep-alive
                       socket))))))))
