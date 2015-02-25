(in-package :cl-user)
(defpackage dexador.backend.posix
  (:nicknames :dex.posix)
  (:use :cl
        :dexador.util)
  (:import-from :wsock
                :socket
                :connect
                :shutdown
                :sockaddr-in
                :+AF-INET+
                :+SOCK-STREAM+
                :+SHUT-RDWR+)
  (:import-from :wsys
                :errno
                :bzero
                #+nil :read
                #+nil :write)
  (:import-from :cffi
                :with-foreign-object
                :with-foreign-slots
                :foreign-type-size
                :with-pointer-to-vector-data)
  (:import-from :static-vectors
                :make-static-vector
                :static-vector-pointer
                :free-static-vector)
  (:import-from :trivial-mimes
                :mime)
  (:import-from :quri
                :uri-host
                :uri-path
                :uri-query
                :url-encode-params
                :render-uri)
  (:import-from :fast-io
                :with-fast-output
                :make-output-buffer
                :finish-output-buffer
                :fast-write-sequence
                :fast-write-byte)
  (:import-from :fast-http
                :make-http-response
                :make-parser
                :http-status
                :http-headers)
  (:import-from :usocket
                :host-to-vector-quad)
  (:import-from :split-sequence
                :split-sequence)
  (:import-from :swap-bytes
                :htonl
                :htons)
  (:import-from :alexandria
                :if-let)
  (:export :request))
(in-package :dexador.backend.posix)

(defun vector-to-integer (vector)
  "Convert a vector to a 32-bit unsigned integer."
  (+ (ash (aref vector 0) 24)
     (ash (aref vector 1) 16)
     (ash (aref vector 2) 8)
     (aref vector 3)))

(defun-careful request (uri &key verbose (method :get) (version 1.1)
                            content headers
                            keep-alive socket)
  (let ((uri (quri:uri uri))
        (content (if (consp content)
                     (quri:url-encode-params content)
                     content))
        (fd (or socket
                (wsock:socket wsock:+AF-INET+ wsock:+SOCK-STREAM+ 0))))
    (declare (type fixnum fd))
    (when (= fd -1)
      (error "Cannot create a socket (Code=~A)" errno))
    (cffi:with-foreign-object (sin '(:struct wsock:sockaddr-in))
      (wsys:bzero sin (cffi:foreign-type-size '(:struct wsock:sockaddr-in)))
      (cffi:with-foreign-slots ((wsock::family wsock::addr wsock::port) sin (:struct wsock:sockaddr-in))
        (setf wsock::family wsock:+AF-INET+
              wsock::addr (htonl (vector-to-integer (host-to-vector-quad (quri:uri-host uri))))
              wsock::port (htons (quri:uri-port uri))))
      (let ((retval (wsock:connect fd sin (cffi:foreign-type-size '(:struct wsock:sockaddr-in)))))
        (declare (type fixnum retval))
        (unless (= retval 0)
          (error "Cannot connect to ~S (Code=~A)"
                 (quri:render-uri uri)
                 errno))))
    (let ((first-line-data (with-fast-output (buffer :static)
                             (write-first-line method uri version buffer)))
          (headers-data (with-header-output (buffer :static)
                          (write-header :user-agent #.*default-user-agent*)
                          (write-header :host (uri-host uri))
                          (write-header :accept "*/*")
                          (when (and keep-alive
                                     (= version 1.0)
                                     (not (assoc :connection headers :test #'eq)))
                            (write-header :connection "keep-alive"))
                          (etypecase content
                            (null)
                            (string
                             (write-header :content-type "application/x-www-form-urlencoded")
                             (write-header :content-length (length content)))
                            (pathname
                             (write-header :content-type (mimes:mime content))
                             (if-let ((content-length (assoc :content-length headers :test #'eq)))
                               (write-header :content-length (cdr content-length))
                               (with-open-file (in content)
                                 (write-header :content-length (file-length in))))))

                          ;; Custom headers
                          (loop for (name . value) in headers
                                unless (member name '(:user-agent :host :accept
                                                      :content-type :content-length) :test #'eq)
                                  do (write-header name value))
                          (fast-write-sequence +crlf+ buffer))))
      (unwind-protect
           (progn
             (when verbose
               (format t "~&>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>~%")
               (map nil (lambda (byte)
                          (princ (code-char byte)))
                    first-line-data)
               (map nil (lambda (byte)
                          (princ (code-char byte)))
                    headers-data)
               (format t "~&>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>~%"))
             (wsys:write fd (static-vector-pointer first-line-data) (length first-line-data))
             (wsys:write fd (static-vector-pointer headers-data) (length headers-data)))
        (free-static-vector first-line-data)
        (free-static-vector headers-data)))

    ;; Sending the content
    (etypecase content
      (null)
      (string (let ((content (ascii-string-to-octets content)))
                (cffi:with-pointer-to-vector-data (content-sap content)
                  (wsys:write fd content-sap (length content)))))
      (pathname
       (let ((buffer (make-static-vector 1024)))
         (unwind-protect
              (with-open-file (in content)
                (loop for n = (read-sequence buffer in)
                      do (wsys:write fd (static-vector-pointer buffer) n)
                      while (= n 1024)))
           (free-static-vector buffer)))))

    (let* ((input-buffer (make-static-vector 1024))
           (body (make-output-buffer))
           (http (make-http-response))
           (parser (make-parser http
                                :body-callback
                                (lambda (data start end)
                                  (fast-write-sequence data body start end)))))
      (declare (type function parser))
      (unwind-protect
           (loop
             (let ((n (wsys:read fd (static-vector-pointer input-buffer) 1024)))
               (declare (dynamic-extent n))
               (case n
                 (-1
                  (error "Error while reading from ~D (Code=~A)"
                         fd
                         errno))
                 (0
                  (unless keep-alive
                    (wsock:shutdown fd wsock:+SHUT-RDWR+))
                  (return))
                 (otherwise
                  (funcall parser input-buffer :end n)))))
        (free-static-vector input-buffer))
      (values (finish-output-buffer body)
              (http-status http)
              (http-headers http)
              (when keep-alive
                fd)))))
