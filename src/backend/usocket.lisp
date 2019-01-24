(in-package :cl-user)
(defpackage dexador.backend.usocket
  (:nicknames :dex.usocket)
  (:use :cl
        :dexador.encoding
        :dexador.util)
  (:import-from :dexador.connection-cache
                :steal-connection
                :push-connection)
  (:import-from :dexador.decoding-stream
                :make-decoding-stream)
  (:import-from :dexador.keep-alive-stream
                :make-keep-alive-stream)
  (:import-from :dexador.error
                :http-request-failed
                :http-request-not-found
                :socks5-proxy-request-failed)
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
  (:import-from :chunga
                :chunked-stream-input-chunking-p
                :chunked-stream-output-chunking-p
                :make-chunked-stream)
  (:import-from :trivial-mimes
                :mime)
  (:import-from :cl-cookie
                :merge-cookies
                :parse-set-cookie-header
                :cookie-jar-host-cookies
                :write-cookie-header)
  (:import-from :quri
                :uri-p
                :uri-host
                :uri-port
                :uri-path
                :uri-authority
                :uri-scheme
                :url-encode
                :url-encode-params
                :merge-uris)
  (:import-from :chipz
                :make-decompressing-stream
                :decompress
                :make-dstate)
  (:import-from :cl-base64
                :string-to-base64-string)
  #-dexador-no-ssl
  (:import-from :cl+ssl
                :with-global-context
                :make-context
                :make-ssl-client-stream
                :ensure-initialized
                :ssl-check-verify-p)
  (:import-from :alexandria
                :copy-stream
                :if-let
                :when-let
                :ensure-list)
  (:import-from :uiop)
  (:export :request

           ;; Restarts
           :retry-request
           :ignore-and-continue))
(in-package :dexador.backend.usocket)

(defparameter *ca-bundle*
  (uiop:native-namestring
   (asdf:system-relative-pathname :dexador #P"certs/cacert.pem")))

(defun-speedy read-until-crlf*2 (stream)
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
         (unless next-byte
           (go eof))
         (locally (declare (type (unsigned-byte 8) next-byte))
           (cond
             ((= next-byte (char-code #\Newline))
              (fast-write-byte next-byte buf)
              (go read-cr2))
             ((= next-byte (char-code #\Return))
              (fast-write-byte next-byte buf)
              (go read-lf))
             (T
              (fast-write-byte next-byte buf)
              (go read-cr)))))

     read-cr2
       (let ((next-byte (read-byte stream nil nil)))
         (unless next-byte
           (go eof))
         (locally (declare (type (unsigned-byte 8) next-byte))
           (cond
             ((= next-byte (char-code #\Return))
              (fast-write-byte next-byte buf)
              (go read-lf2))
             (T
              (fast-write-byte next-byte buf)
              (go read-cr)))))

     read-lf2
       (let ((next-byte (read-byte stream nil nil)))
         (unless next-byte
           (go eof))
         (locally (declare (type (unsigned-byte 8) next-byte))
           (cond
             ((= next-byte (char-code #\Newline))
              (fast-write-byte next-byte buf))
             ((= next-byte (char-code #\Return))
              (fast-write-byte next-byte buf)
              (go read-lf))
             (T
              (fast-write-byte next-byte buf)
              (go read-cr)))))

     eof)))

(defvar +empty-body+
  (make-array 0 :element-type '(unsigned-byte 8)))

(defun read-response (stream has-body collect-headers read-body)
  (let* ((http (make-http-response))
         body
         body-data
         (headers-data (and collect-headers
                            (make-output-buffer)))
         (header-finished-p nil)
         (finishedp nil)
         (content-length nil)
         (transfer-encoding-p)
         (parser (make-parser http
                              :header-callback
                              (lambda (headers)
                                (setq header-finished-p t
                                      content-length (gethash "content-length" headers)
                                      transfer-encoding-p (gethash "transfer-encoding" headers))
                                (unless (and has-body
                                             (or content-length
                                                 transfer-encoding-p))
                                  (setq finishedp t)))
                              :body-callback
                              (lambda (data start end)
                                (when body-data
                                  (fast-write-sequence data body-data start end)))
                              :finish-callback
                              (lambda ()
                                (setq finishedp t)))))
    (let ((buf (read-until-crlf*2 stream)))
      (declare (type octets buf))
      (when collect-headers
        (fast-write-sequence buf headers-data))
      (funcall parser buf))
    (unless header-finished-p
      (error "maybe invalid header"))
    (cond
      ((not read-body)
       (setq body stream))
      ((not has-body)
       (setq body +empty-body+))
      ((and content-length (not transfer-encoding-p))
       (let ((buf (make-array content-length :element-type '(unsigned-byte 8))))
         (read-sequence buf stream)
         (setq body buf)))
      ((let ((status (http-status http)))
         (or (= status 100)    ;; Continue
             (= status 101)    ;; Switching Protocols
             (= status 204)    ;; No Content
             (= status 304))) ;; Not Modified
       (setq body +empty-body+))
      (T
       (setq body-data (make-output-buffer))
       (loop for buf of-type octets = (read-until-crlf*2 stream)
             do (funcall parser buf)
             until (or finishedp
                       (zerop (length buf)))
             finally
                (setq body (finish-output-buffer body-data)))))
    (values http
            body
            (and collect-headers
                 (finish-output-buffer headers-data))
            transfer-encoding-p)))

(defun print-verbose-data (direction &rest data)
  (flet ((boundary-line ()
           (let ((char (ecase direction
                         (:incoming #\<)
                         (:outgoing #\>))))
             (fresh-line)
             (dotimes (i 50)
               (write-char char))
             (fresh-line))))
    (boundary-line)
    (dolist (d data)
      (map nil (lambda (byte)
                 (princ (code-char byte)))
           d))
    (boundary-line)))

(defun decompress-body (content-encoding body)
  (unless content-encoding
    (return-from decompress-body body))

  (cond
    ((string= content-encoding "gzip")
     (if (streamp body)
         (chipz:make-decompressing-stream :gzip body)
         (chipz:decompress nil (chipz:make-dstate :gzip) body)))
    ((string= content-encoding "deflate")
     (if (streamp body)
         (chipz:make-decompressing-stream :zlib body)
         (chipz:decompress nil (chipz:make-dstate :zlib) body)))
    (T body)))

(defun decode-body (content-type body)
  (let ((charset (and content-type
                      (detect-charset content-type body)))
        (babel-encodings:*suppress-character-coding-errors* t))
    (if charset
        (handler-case
            (if (streamp body)
                (make-decoding-stream body :encoding charset)
                (babel:octets-to-string body :encoding charset))
          (babel:character-decoding-error (e)
            (warn (format nil "Failed to decode the body to ~S due to the following error (falling back to binary):~%  ~A"
                          charset
                          e))
            (return-from decode-body body)))
        body)))

(defun convert-body (body content-encoding content-type content-length chunkedp force-binary keep-alive-p)
  (when (and (streamp body)
             keep-alive-p)
    (cond
      (chunkedp
       (setf body
             (make-keep-alive-stream body :chunked t)))
      (content-length
       (setf body
              (make-keep-alive-stream body :end content-length)))))
  (let ((body (decompress-body content-encoding
                               (if (and (streamp body)
                                        chunkedp)
                                   (let ((chunked-stream (chunga:make-chunked-stream body)))
                                     (setf (chunga:chunked-stream-input-chunking-p chunked-stream) t)
                                     chunked-stream)
                                   body))))
    (if force-binary
        body
        (decode-body content-type body))))

(defun content-disposition (key val)
  (if (pathnamep val)
      (let* ((filename (file-namestring val))
             (utf8-filename-p (find-if (lambda (char)
                                         (< 127 (char-code char)))
                                       filename)))
        (format nil "Content-Disposition: form-data; name=\"~A\"; ~:[filename=\"~A\"~;filename*=UTF-8''~A~]~C~C"
                key
                utf8-filename-p
                (if utf8-filename-p
                    (url-encode filename :encoding :utf-8)
                    filename)
                #\Return #\Newline))
      (format nil "Content-Disposition: form-data; name=\"~A\"~C~C"
              key
              #\Return #\Newline)))

(defun-speedy multipart-content-length (content boundary)
  (declare (type simple-string boundary))
  (let ((boundary-length (length boundary)))
    (+ (loop for (key . val) in content
             sum (+ 2 boundary-length 2
                    (length (the simple-string (content-disposition key val)))
                    (if (pathnamep val)
                        (+ #.(length "Content-Type: ") (length (the simple-string (mimes:mime val))) 2)
                        0)
                    2
                    (typecase val
                      (pathname (with-open-file (in val)
                                  (file-length in)))
                      (string (length (the octets (babel:string-to-octets val))))
                      (symbol (length (the octets (babel:string-to-octets (princ-to-string val)))))
                      (otherwise (length (princ-to-string val))))
                    2))
       2 boundary-length 2 2)))

(defun write-multipart-content (content boundary stream)
  (let ((boundary (ascii-string-to-octets boundary)))
    (labels ((boundary-line (&optional endp)
               (write-sequence (ascii-string-to-octets "--") stream)
               (write-sequence boundary stream)
               (when endp
                 (write-sequence (ascii-string-to-octets "--") stream))
               (crlf))
             (crlf () (write-sequence +crlf+ stream)))
      (loop for (key . val) in content
            do (boundary-line)
               (write-sequence (ascii-string-to-octets (content-disposition key val)) stream)
               (when (pathnamep val)
                 (write-sequence
                  (ascii-string-to-octets
                   (format nil "Content-Type: ~A~C~C"
                           (mimes:mime val)
                           #\Return #\Newline))
                  stream))
               (crlf)
               (typecase val
                 (pathname (let ((buf (make-array 1024 :element-type '(unsigned-byte 8))))
                             (with-open-file (in val :element-type '(unsigned-byte 8))
                               (loop for n of-type fixnum = (read-sequence buf in)
                                     until (zerop n)
                                     do (write-sequence buf stream :end n)))))
                 (string (write-sequence (babel:string-to-octets val) stream))
                 (otherwise (write-sequence (babel:string-to-octets (princ-to-string val)) stream)))
               (crlf)
            finally
               (boundary-line t)))))

(defmacro with-restarts (&body body)
  `(restart-case (progn ,@body)
     (retry-request ()
       :report "Retry the same request."
       (setf use-connection-pool nil
             reusing-stream-p nil
             stream (make-new-connection uri))
       (go retry))
     (ignore-and-continue ()
       :report "Ignore the error and continue.")))

(defun build-cookie-headers (uri cookie-jar)
  (with-header-output (buffer)
    (let ((cookies (cookie-jar-host-cookies cookie-jar (uri-host uri) (or (uri-path uri) "/")
                                            :securep (string= (uri-scheme uri) "https"))))
      (when cookies
        (fast-write-sequence (ascii-string-to-octets "Cookie: ") buffer)
        (fast-write-sequence
         (ascii-string-to-octets (write-cookie-header cookies))
         buffer)
        (fast-write-sequence +crlf+ buffer)))))

(defun make-connect-stream (uri version stream &optional proxy-auth)
  (let ((header (with-fast-output (buffer)
                  (write-connect-header uri version buffer proxy-auth))))
    (write-sequence header stream)
    (force-output stream)
    (read-until-crlf*2 stream)
    stream))

(defun make-proxy-authorization (uri)
  (let ((proxy-auth (quri:uri-userinfo uri)))
    (when proxy-auth
      (format nil "Basic ~A"
              (string-to-base64-string proxy-auth)))))

(defconstant +socks5-version+ 5)
(defconstant +socks5-reserved+ 0)
(defconstant +socks5-no-auth+ 0)
(defconstant +socks5-connect+ 1)
(defconstant +socks5-domainname+ 3)
(defconstant +socks5-succeeded+ 0)
(defconstant +socks5-ipv4+ 1)
(defconstant +socks5-ipv6+ 4)

(defun ensure-socks5-connected (input output uri http-method)
  (labels ((fail (condition &key reason)
             (error (make-condition condition
                                    :body nil :status nil :headers nil
                                    :uri uri
                                    :method http-method
                                    :reason reason)))
           (exact (n reason)
             (unless (eql n (read-byte input nil 'eof))
               (fail 'dexador.error:socks5-proxy-request-failed :reason reason)))
           (drop (n reason)
             (dotimes (i n)
               (when (eq (read-byte input nil 'eof) 'eof)
                 (fail 'dexador.error:socks5-proxy-request-failed :reason reason)))))
    ;; Send Version + Auth Method
    ;; Currently, only supports no-auth method.
    (write-byte +socks5-version+ output)
    (write-byte 1 output)
    (write-byte +socks5-no-auth+ output)
    (finish-output output)

    ;; Receive Auth Method
    (exact +socks5-version+ "Unexpected version")
    (exact +socks5-no-auth+ "Unsupported auth method")

    ;; Send domainname Request
    (let* ((host (babel:string-to-octets (uri-host uri)))
           (hostlen (length host))
           (port (uri-port uri)))
      (unless (<= 1 hostlen 255)
        (fail 'dexador.error:socks5-proxy-request-failed :reason "domainname too long"))
      (unless (<= 1 port 65535)
        (fail 'dexador.error:socks5-proxy-request-failed :reason "Invalid port"))
      (write-byte +socks5-version+ output)
      (write-byte +socks5-connect+ output)
      (write-byte +socks5-reserved+ output)
      (write-byte +socks5-domainname+ output)
      (write-byte hostlen output)
      (write-sequence host output)
      (write-byte (ldb (byte 8 8) port) output)
      (write-byte (ldb (byte 8 0) port) output)
      (finish-output output)

      ;; Receive reply
      (exact +socks5-version+ "Unexpected version")
      (exact +socks5-succeeded+ "Unexpected result code")
      (drop 1 "Should be reserved byte")
      (let ((atyp (read-byte input nil 'eof)))
        (cond
          ((eql atyp +socks5-ipv4+)
           (drop 6 "Should be IPv4 address and port"))
          ((eql atyp +socks5-ipv6+)
           (drop 18 "Should be IPv6 address and port"))
          ((eql atyp +socks5-domainname+)
           (let ((n (read-byte input nil 'eof)))
             (when (eq n 'eof)
               (fail 'dexador.error:socks5-proxy-request-failed :reason "Invalid domainname length"))
             (drop n "Should be domainname and port")))
          (t
           (fail 'dexador.error:socks5-proxy-request-failed :reason "Unknown address")))))))

(defun-careful request (uri &rest args
                            &key (method :get) (version 1.1)
                            content headers
                            basic-auth
                            cookie-jar
                            (timeout *default-timeout*) (keep-alive t) (use-connection-pool t)
                            (max-redirects 5)
                            ssl-key-file ssl-cert-file ssl-key-password
                            stream (verbose *verbose*)
                            force-binary
                            want-stream
                            proxy
                            (insecure *not-verify-ssl*)
                            ca-path
                            &aux
                            (proxy-uri (and proxy (quri:uri proxy))))
  (declare (ignorable ssl-key-file ssl-cert-file ssl-key-password
                      timeout ca-path)
           (type single-float version)
           (type fixnum max-redirects))
  (labels ((make-new-connection (uri)
             (restart-case
                 (let* ((con-uri (quri:uri (or proxy uri)))
                        (stream
                          (usocket:socket-stream
                           (usocket:socket-connect (uri-host con-uri)
                                                   (uri-port con-uri)
                                                   #-(or ecl clisp allegro) :timeout #-(or ecl clisp allegro) timeout
                                                                    :element-type '(unsigned-byte 8))))
                        (scheme (uri-scheme uri)))
                   (declare (type string scheme))
                   (when (socks5-proxy-p proxy-uri)
                     (ensure-socks5-connected stream stream uri method))
                   (if (string= scheme "https")
                       #+dexador-no-ssl
                       (error "SSL not supported. Remove :dexador-no-ssl from *features* to enable SSL.")
                       #-dexador-no-ssl
                       (progn
                         (cl+ssl:ensure-initialized)
                         (let ((ctx (cl+ssl:make-context :verify-mode
                                                         (if insecure
                                                             cl+ssl:+ssl-verify-none+
                                                             cl+ssl:+ssl-verify-peer+)
                                                         :verify-location
                                                         (cond
                                                           (ca-path (uiop:native-namestring ca-path))
                                                           ((probe-file *ca-bundle*) *ca-bundle*)
                                                           ;; In executable environment, perhaps *ca-bundle* doesn't exist.
                                                           (t :default)))))
                           (cl+ssl:with-global-context (ctx :auto-free-p t)
                             (cl+ssl:make-ssl-client-stream (if (http-proxy-p proxy-uri)
                                                                (make-connect-stream uri version stream (make-proxy-authorization con-uri))
                                                                stream)
                                                            :hostname (uri-host uri)
                                                            :verify (not insecure)
                                                            :certificate ssl-cert-file
                                                            :key ssl-key-file
                                                            :password ssl-key-password))))
                       stream))
               (retry-request ()
                 :report "Retry the same request."
                 (return-from request
                   (apply #'request uri :use-connection-pool nil args)))))
           (http-proxy-p (uri)
             (and uri
                  (let ((scheme (uri-scheme uri)))
                    (and (stringp scheme)
                         (or (string= scheme "http")
                             (string= scheme "https"))))))
           (socks5-proxy-p (uri)
             (and uri
                  (let ((scheme (uri-scheme uri)))
                    (and (stringp scheme)
                         (string= scheme "socks5")))))
           (connection-keep-alive-p (connection-header)
             (and keep-alive
                  (or (and (= (the single-float version) 1.0)
                           (equalp connection-header "keep-alive"))
                      (not (equalp connection-header "close")))))
           (finalize-connection (stream connection-header uri)
             (if (or want-stream
                     (connection-keep-alive-p connection-header))
                 (push-connection (format nil "~A://~A"
                                          (uri-scheme uri)
                                          (uri-authority uri)) stream)
                 (ignore-errors (close stream)))))
    (let* ((uri (quri:uri uri))
           (proxy (when (http-proxy-p proxy-uri) proxy))
           (content-type
             (cdr (find :content-type headers :key #'car :test #'eq)))
           (multipart-p (and (not content-type)
                             (consp content)
                             (find-if #'pathnamep content :key #'cdr)))
           (form-urlencoded-p (and (not content-type)
                                   (consp content)
                                   (not multipart-p)))
           (boundary (and multipart-p
                          (make-random-string 12)))
           (content (if form-urlencoded-p
                        (quri:url-encode-params content)
                        content))
           (stream (or stream
                       (and use-connection-pool
                            (steal-connection (format nil "~A://~A"
                                                      (uri-scheme uri)
                                                      (uri-authority uri))))))
           (reusing-stream-p (not (null stream)))
           (stream (or stream
                       (make-new-connection uri)))
           (content-length
             (assoc :content-length headers :test #'string-equal))
           (transfer-encoding
             (assoc :transfer-encoding headers :test #'string-equal))
           (chunkedp (or (and transfer-encoding
                              (equalp (cdr transfer-encoding) "chunked"))
                         (and content-length
                              (null (cdr content-length)))))
           (first-line-data
             (with-fast-output (buffer)
               (write-first-line method uri proxy version buffer)))
           (headers-data
             (flet ((write-header* (name value)
                      (let ((header (assoc name headers :test #'string-equal)))
                        (if header
                            (when (cdr header)
                              (write-header name (cdr header)))
                            (write-header name value)))
                      (values)))
               (with-header-output (buffer)
                 (write-header* :user-agent #.*default-user-agent*)
                 (write-header* :host (uri-authority uri))
                 (write-header* :accept "*/*")
                 (cond
                   ((and keep-alive
                         (= (the single-float version) 1.0))
                    (write-header* :connection "keep-alive"))
                   ((and (not keep-alive)
                         (= (the single-float version) 1.1))
                    (write-header* :connection "close")))
                 (when basic-auth
                   (write-header* :authorization
                                  (format nil "Basic ~A"
                                          (string-to-base64-string
                                           (format nil "~A:~A"
                                                   (car basic-auth)
                                                   (cdr basic-auth))))))
                 (when proxy
                   (let ((scheme (quri:uri-scheme uri)))
                     (when (string= scheme "http")
                       (let* ((uri (quri:uri proxy))
                              (proxy-authorization (make-proxy-authorization uri)))
                         (when proxy-authorization
                           (write-header* :proxy-authorization proxy-authorization))))))
                 (cond
                   (multipart-p
                    (write-header* :content-type (format nil "multipart/form-data; boundary=~A" boundary))
                    (unless chunkedp
                      (write-header* :content-length
                                     (multipart-content-length content boundary))))
                   (form-urlencoded-p
                    (write-header* :content-type "application/x-www-form-urlencoded")
                    (unless chunkedp
                      (write-header* :content-length (length (the string content)))))
                   (t
                    (etypecase content
                      (null
                       (unless chunkedp
                         (write-header* :content-length 0)))
                      (string
                       (write-header* :content-type (or content-type "text/plain"))
                       (unless chunkedp
                         (write-header* :content-length (length (the (simple-array (unsigned-byte 8) *) (babel:string-to-octets content))))))
                      ((array (unsigned-byte 8) *)
                       (write-header* :content-type (or content-type "text/plain"))
                       (unless chunkedp
                         (write-header* :content-length (length content))))
                      (pathname
                       (write-header* :content-type (or content-type (mimes:mime content)))
                       (unless chunkedp
                         (if-let ((content-length (assoc :content-length headers :test #'string-equal)))
                           (write-header :content-length (cdr content-length))
                           (with-open-file (in content)
                             (write-header :content-length (file-length in)))))))))

                 ;; Transfer-Encoding: chunked
                 (when (and chunkedp
                            (not transfer-encoding))
                   (write-header* :transfer-encoding "chunked"))

                 ;; Custom headers
                 (loop for (name . value) in headers
                       unless (member name '(:user-agent :host :accept
                                             :connection
                                             :content-type :content-length) :test #'string-equal)
                         do (write-header name value)))))
           (cookie-headers (and cookie-jar
                                (build-cookie-headers uri cookie-jar))))
      (macrolet ((with-retrying (&body body)
                   `(if reusing-stream-p
                        (handler-bind ((error
                                         (lambda (e)
                                           (declare (ignore e))
                                           (ignore-errors (close stream))
                                           (when reusing-stream-p
                                             (setf use-connection-pool nil
                                                   reusing-stream-p nil
                                                   stream (make-new-connection uri))
                                             (go retry)))))
                          ,@body)
                        (restart-case
                            (progn ,@body)
                          (retry-request ()
                            :report "Retry the same request."
                            (return-from request
                              (apply #'request uri :use-connection-pool nil args)))))))
        (tagbody
         retry
           (with-retrying
             (write-sequence first-line-data stream)
             (write-sequence headers-data stream)
             (when cookie-headers
               (write-sequence cookie-headers stream))
             (write-sequence +crlf+ stream)
             (force-output stream))

           ;; Sending the content
           (when content
             (let ((stream (if chunkedp
                               (chunga:make-chunked-stream stream)
                               stream)))
               (when chunkedp
                 (setf (chunga:chunked-stream-output-chunking-p stream) t))
               (with-retrying
                 (etypecase content
                   (string
                    (write-sequence (babel:string-to-octets content) stream))
                   ((array (unsigned-byte 8) *)
                    (write-sequence content stream))
                   (pathname (with-open-file (in content :element-type '(unsigned-byte 8))
                               (copy-stream in stream)))
                   (cons
                    (write-multipart-content content boundary stream)))
                 (when chunkedp
                   (setf (chunga:chunked-stream-output-chunking-p stream) nil))
                 (finish-output stream))))

         start-reading
           (multiple-value-bind (http body response-headers-data transfer-encoding-p)
               (with-retrying
                   (read-response stream (not (eq method :head)) verbose (not want-stream)))
             (let ((status (http-status http))
                   (response-headers (http-headers http)))
               (when (= status 0)
                 (unless reusing-stream-p
                   ;; There's nothing we can do.
                   (with-restarts
                     (http-request-failed status
                                          :body body
                                          :headers headers
                                          :uri uri
                                          :method method)))
                 (setf use-connection-pool nil
                       reusing-stream-p nil
                       stream (make-new-connection uri))
                 (go retry))
               (when verbose
                 (print-verbose-data :outgoing first-line-data headers-data cookie-headers +crlf+)
                 (print-verbose-data :incoming response-headers-data))
               (when cookie-jar
                 (when-let (set-cookies (append (gethash "set-cookie" response-headers)
                                                (ensure-list (gethash "set-cookie2" response-headers))))
                   (merge-cookies cookie-jar
                                  (remove nil (mapcar (lambda (cookie)
                                                        (declare (type string cookie))
                                                        (unless (= (length cookie) 0)
                                                          (parse-set-cookie-header cookie
                                                                                   (uri-host uri)
                                                                                   (uri-path uri))))
                                                      set-cookies)))))
               (when (and (member status '(301 302 303 307) :test #'=)
                          (gethash "location" response-headers)
                          (/= max-redirects 0))
                 ;; Need to read the response body
                 (when (and want-stream
                            (not (eq method :head)))
                   (let ((content-length (gethash "content-length" response-headers)))
                     (cond
                       ((integerp content-length)
                        (dotimes (i content-length)
                          (loop until (read-byte body nil nil))))
                       (transfer-encoding-p
                        (read-until-crlf*2 body)))))

                 (let ((location-uri (quri:uri (gethash "location" response-headers))))
                   (if (and (or (null (uri-host location-uri))
                                (and (string= (uri-scheme location-uri)
                                              (uri-scheme uri))
                                     (string= (uri-host location-uri)
                                              (uri-host uri))
                                     (eql (uri-port location-uri)
                                          (uri-port uri))))
                            (or (= status 307)
                                (member method '(:get :head) :test #'eq)))
                       (progn
                         (setq uri (merge-uris location-uri uri))
                         (setq first-line-data
                               (with-fast-output (buffer)
                                 (write-first-line method uri proxy version buffer)))
                         (when cookie-jar
                           ;; Rebuild cookie-headers.
                           (setq cookie-headers (build-cookie-headers uri cookie-jar)))
                         (decf max-redirects)
                         (if (equalp (gethash "connection" response-headers) "close")
                             (progn
                               (finalize-connection stream (gethash "connection" response-headers) uri)
                               (setq use-connection-pool nil
                                     reusing-stream-p nil
                                     stream (make-new-connection uri)))
                             (setq reusing-stream-p t))
                         (go retry))
                       (progn
                         (setf location-uri (quri:merge-uris location-uri uri))
                         (finalize-connection stream (gethash "connection" response-headers) uri)
                         (setf (getf args :headers)
                               (nconc `((:host . ,(uri-host location-uri))) headers))
                         (setf (getf args :max-redirects)
                               (1- max-redirects))
                         ;; Redirect as GET if it's 301, 302, 303
                         (unless (or (= status 307)
                                     (member method '(:get :head) :test #'eq))
                           (setf (getf args :method) :get))
                         (return-from request
                           (apply #'request location-uri args))))))
               (unwind-protect
                    (let ((body (convert-body body
                                              (gethash "content-encoding" response-headers)
                                              (gethash "content-type" response-headers)
                                              (gethash "content-length" response-headers)
                                              transfer-encoding-p
                                              force-binary
                                              (connection-keep-alive-p
                                               (gethash "connection" response-headers)))))
                      ;; Raise an error when the HTTP response status code is 4xx or 50x.
                      (when (<= 400 status)
                        (with-restarts
                          (http-request-failed status
                                               :body body
                                               :headers response-headers
                                               :uri uri
                                               :method method)))
                      (return-from request
                        (values body
                                status
                                response-headers
                                uri
                                (when (and keep-alive
                                           (not (equalp (gethash "connection" response-headers) "close")))
                                  stream))))
                 (finalize-connection stream (gethash "connection" response-headers) uri)))))))))
