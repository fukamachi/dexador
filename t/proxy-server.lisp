(in-package :cl-user)
(defpackage #:dexador-test.proxy
  (:use #:cl)
  (:import-from #:cl-base64
                #:string-to-base64-string)
  (:export #:start-test-proxy
           #:stop-test-proxy
           #:test-proxy-port
           #:test-proxy-url
           #:test-proxy-requests
           #:with-test-proxy
           #:+proxied-by-header+))
(in-package #:dexador-test.proxy)

;;; A minimal but strict HTTP/1.1 forward proxy for exercising dexador's
;;; proxy support in the test suite, portably (including Windows).
;;;
;;; - Plain http requests must use the RFC 7230 absolute-form request target,
;;;   otherwise the proxy answers 400. Forwarded responses get an
;;;   "X-Proxied-By: <tag>" header injected so tests can tell proxied from
;;;   direct responses.
;;; - CONNECT establishes a blind tunnel (for https through the proxy).
;;; - Optional Basic proxy authentication (407 challenge on failure).

(alexandria:define-constant +proxied-by-header+ "x-proxied-by"
  :test #'equal
  :documentation "Response header the test proxy injects into forwarded responses.")

(defstruct (test-proxy (:constructor %make-test-proxy))
  socket
  thread
  port
  tag
  basic-auth
  (requests nil)
  (lock (bt2:make-lock :name "test proxy lock")))

(defun record-request (proxy line)
  (bt2:with-lock-held ((test-proxy-lock proxy))
    (push line (test-proxy-requests proxy))))

(defun test-proxy-url (proxy &key userinfo)
  "Proxy URL, optionally with USERINFO credentials like \"user:pass\"."
  (format nil "http://~@[~A@~]127.0.0.1:~D" userinfo (test-proxy-port proxy)))

(defun crlf (stream)
  (write-byte 13 stream)
  (write-byte 10 stream))

(defun write-ascii-line (stream string)
  (loop for char across string
        do (write-byte (char-code char) stream))
  (crlf stream))

(defun read-ascii-line (stream)
  "Read one CRLF-terminated header line from a binary stream, or NIL at EOF."
  (let ((out (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
    (loop for byte = (read-byte stream nil nil)
          do (cond ((null byte) (return-from read-ascii-line nil))
                   ((= byte 10) (return))
                   ((/= byte 13) (vector-push-extend (code-char byte) out))))
    (coerce out 'simple-string)))

(defun read-head (stream)
  "Read a request/response head. Returns (VALUES first-line headers-alist)."
  (let ((first-line (read-ascii-line stream)))
    (when first-line
      (values first-line
              (loop for line = (read-ascii-line stream)
                    while (and line (plusp (length line)))
                    for colon = (position #\: line)
                    when colon
                      collect (cons (string-downcase (subseq line 0 colon))
                                    (string-trim " " (subseq line (1+ colon)))))))))

(defun send-simple-response (stream status reason &rest extra-headers)
  (write-ascii-line stream (format nil "HTTP/1.1 ~D ~A" status reason))
  (dolist (header extra-headers)
    (write-ascii-line stream header))
  (write-ascii-line stream "Content-Length: 0")
  (write-ascii-line stream "Connection: close")
  (crlf stream)
  (force-output stream))

(defun authorized-p (proxy headers)
  (let ((auth (test-proxy-basic-auth proxy)))
    (or (null auth)
        (equal (cdr (assoc "proxy-authorization" headers :test #'string=))
               (format nil "Basic ~A"
                       (string-to-base64-string
                        (format nil "~A:~A" (car auth) (cdr auth))))))))

(defun copy-until-eof (from to)
  "Relay bytes FROM -> TO until EOF. Blocks for the first byte of each chunk
only, so it also works for interactive CONNECT tunnels."
  (loop with buffer = (make-array 4096 :element-type '(unsigned-byte 8))
        for first = (read-byte from nil nil)
        while first
        do (setf (aref buffer 0) first)
           (let ((end 1))
             (loop while (and (< end (length buffer)) (listen from))
                   for byte = (read-byte from nil nil)
                   while byte
                   do (setf (aref buffer end) byte)
                      (incf end))
             (write-sequence buffer to :end end)
             (force-output to))))

(defun forward-request (client method target version headers proxy)
  "Forward an absolute-form request upstream; relay the response to CLIENT
with an X-Proxied-By header injected."
  (let* ((uri (quri:uri target))
         (path (format nil "~A~@[?~A~]" (or (quri:uri-path uri) "/") (quri:uri-query uri))))
    (usocket:with-client-socket (upstream stream (quri:uri-host uri) (quri:uri-port uri)
                                          :element-type '(unsigned-byte 8))
      (declare (ignorable upstream))
      (write-ascii-line stream (format nil "~A ~A ~A" method path version))
      (write-ascii-line stream (format nil "Host: ~A" (quri:uri-authority uri)))
      (write-ascii-line stream "Connection: close")
      ;; RFC 9110 7.6.3: a proxy MUST add Via to forwarded requests.
      (write-ascii-line stream (format nil "Via: 1.1 ~A" (test-proxy-tag proxy)))
      (loop for (name . value) in headers
            unless (member name '("host" "connection" "proxy-connection" "proxy-authorization")
                           :test #'string=)
              do (write-ascii-line stream (format nil "~:(~A~): ~A" name value)))
      (crlf stream)
      ;; Forward a Content-Length body if the client sent one.
      (let ((length (cdr (assoc "content-length" headers :test #'string=))))
        (when (and length (plusp (parse-integer length)))
          (let ((body (make-array (parse-integer length) :element-type '(unsigned-byte 8))))
            (read-sequence body client)
            (write-sequence body stream))))
      (force-output stream)
      ;; Relay the response, injecting the marker header and forcing the
      ;; client connection closed (we serve one request per connection).
      (multiple-value-bind (status-line response-headers) (read-head stream)
        (write-ascii-line client status-line)
        (write-ascii-line client (format nil "X-Proxied-By: ~A" (test-proxy-tag proxy)))
        (loop for (name . value) in response-headers
              unless (string= name "connection")
                do (write-ascii-line client (format nil "~:(~A~): ~A" name value)))
        (write-ascii-line client "Connection: close")
        (crlf client)
        ;; Flush the head now: bodiless responses (HEAD, 204...) never reach
        ;; the copy loop, and usocket's socket-close doesn't flush.
        (force-output client)
        (copy-until-eof stream client)))))

(defun establish-tunnel (client target)
  "Handle CONNECT: open a TCP connection to TARGET (host:port) and relay
bytes in both directions until either side closes."
  (let* ((colon (position #\: target :from-end t))
         (host (subseq target 0 colon))
         (port (parse-integer (subseq target (1+ colon)))))
    (usocket:with-client-socket (upstream stream host port :element-type '(unsigned-byte 8))
      (declare (ignorable upstream))
      (write-ascii-line client "HTTP/1.1 200 Connection Established")
      (crlf client)
      (force-output client)
      (let ((up (bt2:make-thread (lambda ()
                                   (ignore-errors (copy-until-eof client stream)))
                                 :name "test proxy tunnel")))
        (ignore-errors (copy-until-eof stream client))
        (bt2:join-thread up)))))

(defun handle-connection (proxy client)
  (multiple-value-bind (first-line headers) (read-head client)
    (when first-line
      (record-request proxy first-line)
      (destructuring-bind (method target &optional (version "HTTP/1.1"))
          (uiop:split-string first-line :separator " ")
        (cond
          ((not (authorized-p proxy headers))
           (send-simple-response client 407 "Proxy Authentication Required"
                                 "Proxy-Authenticate: Basic realm=\"dexador-test\""))
          ((string= method "CONNECT")
           (establish-tunnel client target))
          ;; A proxy must receive absolute-form targets (RFC 7230 5.3.2).
          ((not (uiop:string-prefix-p "http://" target))
           (send-simple-response client 400 "Bad Request"))
          (t
           (forward-request client method target version headers proxy)))))))

(defun start-test-proxy (&key basic-auth (tag "cl-test-proxy"))
  "Start an HTTP forward proxy on an ephemeral 127.0.0.1 port.
BASIC-AUTH is an optional (\"user\" . \"pass\") cons the proxy requires."
  (let* ((socket (usocket:socket-listen "127.0.0.1" 0 :element-type '(unsigned-byte 8)
                                                      :reuse-address t))
         (proxy (%make-test-proxy :socket socket
                                  :port (usocket:get-local-port socket)
                                  :tag tag
                                  :basic-auth basic-auth)))
    (setf (test-proxy-thread proxy)
          (bt2:make-thread
           (lambda ()
             (loop for client-socket = (handler-case (usocket:socket-accept
                                                      socket :element-type '(unsigned-byte 8))
                                         (error () (return)))
                   while client-socket
                   do (unwind-protect
                          (handler-case
                              (handle-connection proxy (usocket:socket-stream client-socket))
                            (error (e)
                              (warn "test proxy error: ~A" e)))
                        (ignore-errors (usocket:socket-close client-socket)))))
           :name "test proxy acceptor"))
    proxy))

(defun stop-test-proxy (proxy)
  (ignore-errors (usocket:socket-close (test-proxy-socket proxy)))
  (ignore-errors (bt2:join-thread (test-proxy-thread proxy)))
  (values))

(defmacro with-test-proxy ((var &rest args &key basic-auth tag) &body body)
  (declare (ignore basic-auth tag))
  `(let ((,var (start-test-proxy ,@args)))
     (unwind-protect
         (progn ,@body)
       (stop-test-proxy ,var))))
