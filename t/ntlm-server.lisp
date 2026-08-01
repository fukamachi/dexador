(in-package :cl-user)
(defpackage #:dexador-test.ntlm
  (:use #:cl)
  (:import-from #:cl-base64
                #:usb8-array-to-base64-string
                #:base64-string-to-usb8-array)
  (:export #:start-ntlm-server
           #:stop-ntlm-server
           #:ntlm-server-port
           #:ntlm-server-message-types
           #:with-ntlm-server))
(in-package #:dexador-test.ntlm)

;;; A fake NTLM-authenticating HTTP server for exercising the winhttp
;;; backend's single-sign-on challenge loop against a real SSPI client.
;;;
;;; It performs the NTLM handshake structurally without validating any
;;; cryptography: 401 + "WWW-Authenticate: NTLM" on unauthenticated requests,
;;; a static Type 2 challenge in reply to the client's Type 1 token, and
;;; 200 "SSO-OK" for the final Type 3 token. NTLM is connection-oriented, so
;;; all three legs are served over one kept-alive connection.

(defun type2-challenge-message ()
  "The canonical CHALLENGE_MESSAGE (Type 2) test vector from MS-NLMP 4.2.4.3.
Using the spec's own bytes guarantees the client's SSPI accepts the challenge
and advances to a Type 3 message; the handshake is never cryptographically
validated by this test server."
  (coerce
   #(#x4e #x54 #x4c #x4d #x53 #x53 #x50 #x00 #x02 #x00 #x00 #x00 #x0c #x00 #x0c #x00
     #x38 #x00 #x00 #x00 #x33 #x82 #x8a #xe2 #x01 #x23 #x45 #x67 #x89 #xab #xcd #xef
     #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x24 #x00 #x24 #x00 #x44 #x00 #x00 #x00
     #x06 #x00 #x70 #x17 #x00 #x00 #x00 #x0f #x53 #x00 #x65 #x00 #x72 #x00 #x76 #x00
     #x65 #x00 #x72 #x00 #x02 #x00 #x0c #x00 #x44 #x00 #x6f #x00 #x6d #x00 #x61 #x00
     #x69 #x00 #x6e #x00 #x01 #x00 #x0c #x00 #x53 #x00 #x65 #x00 #x72 #x00 #x76 #x00
     #x65 #x00 #x72 #x00 #x00 #x00 #x00 #x00)
   '(vector (unsigned-byte 8))))

(defstruct (ntlm-server (:constructor %make-ntlm-server))
  socket
  thread
  port
  ;; NTLM message types received, in order (expect (1 3) for a handshake).
  (message-types nil))

(defun crlf (stream)
  (write-byte 13 stream)
  (write-byte 10 stream))

(defun write-ascii-line (stream string)
  (loop for char across string
        do (write-byte (char-code char) stream))
  (crlf stream))

(defun read-ascii-line (stream)
  (let ((out (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
    (loop for byte = (read-byte stream nil nil)
          do (cond ((null byte) (return-from read-ascii-line nil))
                   ((= byte 10) (return))
                   ((/= byte 13) (vector-push-extend (code-char byte) out))))
    (coerce out 'simple-string)))

(defun read-request-authorization (stream)
  "Read one request head; return the Authorization header value (or NIL),
or :EOF when the connection is gone."
  (let ((first-line (read-ascii-line stream)))
    (if (null first-line)
        :eof
        (loop with authorization = nil
              for line = (read-ascii-line stream)
              while (and line (plusp (length line)))
              do (let ((colon (position #\: line)))
                   (when (and colon (string-equal "authorization" (subseq line 0 colon)))
                     (setf authorization (string-trim " " (subseq line (1+ colon))))))
              finally (return authorization)))))

(defun respond (stream status reason body &rest extra-headers)
  (write-ascii-line stream (format nil "HTTP/1.1 ~D ~A" status reason))
  (dolist (header extra-headers)
    (write-ascii-line stream header))
  (write-ascii-line stream (format nil "Content-Length: ~D" (length body)))
  (write-ascii-line stream "Connection: keep-alive")
  (crlf stream)
  (loop for char across body
        do (write-byte (char-code char) stream))
  (force-output stream))

(defun ntlm-token-type (authorization)
  "NTLM message type of an \"NTLM <base64>\" Authorization value, or NIL."
  (when (and authorization
             (uiop:string-prefix-p "NTLM " authorization))
    (let ((token (base64-string-to-usb8-array (subseq authorization 5))))
      (when (<= 12 (length token))
        (aref token 8)))))

(defun handle-connection (server stream)
  (loop
    (let ((authorization (read-request-authorization stream)))
      (when (eq authorization :eof)
        (return))
      (let ((message-type (ntlm-token-type authorization)))
        (when message-type
          (setf (ntlm-server-message-types server)
                (append (ntlm-server-message-types server) (list message-type))))
        (case message-type
          ((nil)
           (respond stream 401 "Unauthorized" "" "WWW-Authenticate: NTLM"))
          (1
           (respond stream 401 "Unauthorized" ""
                    (format nil "WWW-Authenticate: NTLM ~A"
                            (usb8-array-to-base64-string (type2-challenge-message)))))
          (3
           (respond stream 200 "OK" "SSO-OK"))
          (t
           (respond stream 400 "Bad Request" "")))))))

(defun start-ntlm-server ()
  (let* ((socket (usocket:socket-listen "127.0.0.1" 0 :element-type '(unsigned-byte 8)
                                                      :reuse-address t))
         (server (%make-ntlm-server :socket socket
                                    :port (usocket:get-local-port socket))))
    (setf (ntlm-server-thread server)
          (bt2:make-thread
           (lambda ()
             (loop for client = (handler-case (usocket:socket-accept
                                               socket :element-type '(unsigned-byte 8))
                                  (error () (return)))
                   while client
                   do (unwind-protect
                          (handler-case (handle-connection server (usocket:socket-stream client))
                            (error (e) (warn "ntlm test server error: ~A" e)))
                        (ignore-errors (usocket:socket-close client)))))
           :name "ntlm test server"))
    server))

(defun stop-ntlm-server (server)
  (ignore-errors (usocket:socket-close (ntlm-server-socket server)))
  (ignore-errors (bt2:join-thread (ntlm-server-thread server)))
  (values))

(defmacro with-ntlm-server ((var) &body body)
  `(let ((,var (start-ntlm-server)))
     (unwind-protect
         (progn ,@body)
       (stop-ntlm-server ,var))))
