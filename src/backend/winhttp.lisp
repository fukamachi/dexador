(defpackage #:dexador.backend.winhttp
  (:nicknames :dex.winhttp)
  (:use #:cl
        #:dexador.restarts
        #:dexador.util
        #:winhttp)
  (:import-from #:dexador.body
                #:decode-body
                #:write-multipart-content
                #:decompress-body)
  (:import-from #:dexador.error
                #:http-request-failed)
  (:import-from #:winhttp
                #:set-ignore-certificates
                #:set-timeouts)
  (:import-from #:fast-io
                #:fast-output-stream
                #:with-fast-output
                #:fast-write-sequence
                #:finish-output-stream)
  (:import-from #:babel)
  (:import-from #:flexi-streams)
  (:import-from #:cl-cookie
                #:cookie-jar-host-cookies
                #:write-cookie-header
                #:parse-set-cookie-header
                #:merge-cookies)
  (:import-from #:alexandria
                #:read-file-into-byte-vector
                #:ensure-list
                #:when-let)
  (:import-from #:split-sequence
                #:split-sequence)
  (:import-from #:cl-base64
                #:string-to-base64-string)
  (:export :request))
(in-package #:dexador.backend.winhttp)

(defconstant +WINHTTP_OPTION_DISABLE_FEATURE+ 63)
(defconstant +WINHTTP_DISABLE_COOKIES+    #x00000001)
(defconstant +WINHTTP_DISABLE_REDIRECTS+  #x00000002)
(defconstant +WINHTTP_DISABLE_KEEP_ALIVE+ #x00000008)

(defconstant +WINHTTP_OPTION_PROXY+ 38)
(defconstant +WINHTTP_OPTION_AUTOLOGON_POLICY+ 77)
;; Autologon security levels (WINHTTP_OPTION_AUTOLOGON_POLICY).
(defconstant +WINHTTP_AUTOLOGON_SECURITY_LEVEL_MEDIUM+ 0)
(defconstant +WINHTTP_AUTOLOGON_SECURITY_LEVEL_LOW+ 1)
(defconstant +WINHTTP_AUTOLOGON_SECURITY_LEVEL_HIGH+ 2)
(defconstant +WINHTTP_ACCESS_TYPE_NO_PROXY+ 1)
(defconstant +WINHTTP_ACCESS_TYPE_NAMED_PROXY+ 3)
;; Windows 8.1+: resolve the proxy per request from WinINet (registry) settings,
;; PAC scripts and WPAD, falling back to direct when nothing is configured.
(defconstant +WINHTTP_ACCESS_TYPE_AUTOMATIC_PROXY+ 4)

;; WinHttpQueryAuthSchemes / WinHttpSetCredentials scheme bitflags.
(defconstant +WINHTTP_AUTH_SCHEME_BASIC+ 1)
(defconstant +WINHTTP_AUTH_SCHEME_NTLM+ 2)
(defconstant +WINHTTP_AUTH_SCHEME_DIGEST+ 8)
(defconstant +WINHTTP_AUTH_SCHEME_NEGOTIATE+ 16)
(defconstant +ERROR_INVALID_PARAMETER+ 87)

(cffi:defcstruct winhttp-proxy-info
  (access-type :uint32)
  (proxy :pointer)
  (bypass :pointer))

(cffi:defcfun (%query-auth-schemes "WinHttpQueryAuthSchemes" :convention :stdcall) :boolean
  (hreq :pointer)
  (supported-schemes :pointer)
  (first-scheme :pointer)
  (auth-target :pointer))

(defun open-session (user-agent access-type)
  (winhttp::with-wide-string (u user-agent)
    (let ((h (winhttp::%http-open u access-type (cffi:null-pointer) (cffi:null-pointer) 0)))
      (when (cffi:null-pointer-p h)
        (winhttp::get-last-error))
      h)))

(defun open-http-session (user-agent system-proxy-p)
  (if system-proxy-p
      (handler-case (open-session user-agent +WINHTTP_ACCESS_TYPE_AUTOMATIC_PROXY+)
        (winhttp::win-error (e)
          ;; AUTOMATIC_PROXY requires Windows 8.1.
          (if (= (winhttp::win-error-code e) +ERROR_INVALID_PARAMETER+)
              (open-session user-agent +WINHTTP_ACCESS_TYPE_NO_PROXY+)
              (error e))))
      (open-session user-agent +WINHTTP_ACCESS_TYPE_NO_PROXY+)))

(defmacro with-http-session ((var user-agent system-proxy-p) &body body)
  `(let ((,var (open-http-session ,user-agent ,system-proxy-p)))
     (unwind-protect (progn ,@body)
       (winhttp::close-handle ,var))))

(defun autologon-policy-value ()
  "DWORD for WINHTTP_OPTION_AUTOLOGON_POLICY."
  (if (not *use-default-credentials*)
      +WINHTTP_AUTOLOGON_SECURITY_LEVEL_HIGH+
      (ecase *winhttp-autologon-policy*
        (:medium +WINHTTP_AUTOLOGON_SECURITY_LEVEL_MEDIUM+)
        (:low +WINHTTP_AUTOLOGON_SECURITY_LEVEL_LOW+)
        (:high +WINHTTP_AUTOLOGON_SECURITY_LEVEL_HIGH+))))

(defun set-request-credentials (req target scheme user pass)
  "WinHttpSetCredentials wrapper. A NULL USER answers Negotiate/NTLM challenges
with the logged-on user's credentials (single sign-on)."
  (flet ((%set (u p)
           (unless (winhttp::%set-credentials req
                                              (ecase target (:server 0) (:proxy 1))
                                              (ecase scheme
                                                (:basic +WINHTTP_AUTH_SCHEME_BASIC+)
                                                (:ntlm +WINHTTP_AUTH_SCHEME_NTLM+)
                                                (:digest +WINHTTP_AUTH_SCHEME_DIGEST+)
                                                (:negotiate +WINHTTP_AUTH_SCHEME_NEGOTIATE+))
                                              u p (cffi:null-pointer))
             (winhttp::get-last-error))))
    (if user
        (winhttp::with-wide-string (u user)
          (winhttp::with-wide-string (p (or pass ""))
            (%set u p)))
        (%set (cffi:null-pointer) (cffi:null-pointer)))))

(defun query-auth-schemes (req)
  "Call WinHttpQueryAuthSchemes. Returns (VALUES supported-bitmask preferred-scheme
auth-target) or NIL when the call fails (e.g. no challenge pending)."
  (cffi:with-foreign-objects ((supported :uint32)
                              (first :uint32)
                              (target :uint32))
    (when (%query-auth-schemes req supported first target)
      (values (cffi:mem-ref supported :uint32)
              (cffi:mem-ref first :uint32)
              (cffi:mem-ref target :uint32)))))

(defun select-auth-scheme-from-mask (supported)
  "Return the strongest scheme in SUPPORTED."
  (loop for (flag key) in `((,+WINHTTP_AUTH_SCHEME_NEGOTIATE+ :negotiate)
                            (,+WINHTTP_AUTH_SCHEME_NTLM+ :ntlm)
                            (,+WINHTTP_AUTH_SCHEME_DIGEST+ :digest)
                            (,+WINHTTP_AUTH_SCHEME_BASIC+ :basic))
        when (plusp (logand supported flag))
          return key))

(defun select-auth-scheme-from-header (challenge)
  "Fallback when WinHttpQueryAuthSchemes is unavailable: parse a
WWW-Authenticate / Proxy-Authenticate header value."
  (when challenge
    ;; Splitting on commas also cuts inside Digest parameter lists
    ;; (qop=\"auth,auth-int\"); the stray pieces simply match no scheme name.
    (let ((tokens (loop for part in (split-sequence #\, challenge)
                        for trimmed = (string-trim " " part)
                        collect (subseq trimmed 0 (or (position #\Space trimmed)
                                                      (length trimmed))))))
      (loop for (name key) in '(("Negotiate" :negotiate) ("NTLM" :ntlm)
                                ("Digest" :digest) ("Basic" :basic))
            when (member name tokens :test #'string-equal)
              return key))))

(defun userinfo-credentials (uri)
  (let ((userinfo (and uri (quri:uri-userinfo uri))))
    (when userinfo
      (let ((colon (position #\: userinfo)))
        (if colon
            (cons (subseq userinfo 0 colon) (subseq userinfo (1+ colon)))
            (cons userinfo ""))))))

(defun drain-response-body (req)
  "Consume any remaining response body so the request handle can be resent."
  (let ((buffer (make-array 4096 :element-type '(unsigned-byte 8))))
    (loop for bytes = (read-data req buffer)
          until (zerop bytes))))

(defun answer-auth-challenge (req status response-headers uri proxy-uri basic-auth)
  "Set credentials on REQ for a 401/407 challenge; true when the request
should be resent. Uses WinHttpQueryAuthSchemes when available (required for
Digest and the documented SetCredentials flow), falling back to header parsing.
Explicit credentials (URL userinfo / BASIC-AUTH) are used with whatever scheme
the peer selected; Negotiate/NTLM challenges without explicit credentials fall
back to single sign-on when *USE-DEFAULT-CREDENTIALS* is true."
  (multiple-value-bind (supported preferred queried-target)
      (query-auth-schemes req)
    (declare (ignore preferred))
    (multiple-value-bind (target creds scheme)
        (if supported
            (let* ((target (if (= queried-target 1) :proxy :server))
                   (creds (if (eq target :proxy)
                              (userinfo-credentials proxy-uri)
                              (or (userinfo-credentials uri) basic-auth)))
                   (scheme (select-auth-scheme-from-mask supported)))
              (values target creds scheme))
            ;; QueryAuthSchemes failed: fall back to status + header parsing.
            (let* ((target (if (eql status 407) :proxy :server))
                   (challenge (gethash (if (eq target :proxy)
                                           "proxy-authenticate"
                                           "www-authenticate")
                                       response-headers))
                   (creds (if (eq target :proxy)
                              (userinfo-credentials proxy-uri)
                              (or (userinfo-credentials uri) basic-auth)))
                   (scheme (select-auth-scheme-from-header challenge)))
              (values target creds scheme)))
      (when scheme
        (cond
          (creds
           (set-request-credentials req target scheme (car creds) (cdr creds))
           t)
          ((and *use-default-credentials* (member scheme '(:negotiate :ntlm)))
           (set-request-credentials req target scheme nil nil)
           t))))))

(defun set-request-proxy (req proxy-uri)
  "Route REQ through PROXY-URI by setting WINHTTP_OPTION_PROXY on the request handle."
  (winhttp::with-wide-string (p (format-host-port (quri:uri-host proxy-uri)
                                                  (quri:uri-port proxy-uri)))
    (cffi:with-foreign-object (info '(:struct winhttp-proxy-info))
      (setf (cffi:foreign-slot-value info '(:struct winhttp-proxy-info) 'access-type)
            +WINHTTP_ACCESS_TYPE_NAMED_PROXY+
            (cffi:foreign-slot-value info '(:struct winhttp-proxy-info) 'proxy) p
            (cffi:foreign-slot-value info '(:struct winhttp-proxy-info) 'bypass) (cffi:null-pointer))
      (unless (winhttp::%set-option req +WINHTTP_OPTION_PROXY+ info
                                    (cffi:foreign-type-size '(:struct winhttp-proxy-info)))
        (winhttp::get-last-error)))))

(defun set-option (req var value &optional (type :uint32))
  (cffi:with-foreign-object (buf type)
    (setf (cffi:mem-aref buf type) value)
    (let ((ret (winhttp::%set-option req
                                     var
                                     buf
                                     (cffi:foreign-type-size type))))
      (unless ret
        (winhttp::get-last-error)))))

(defun query-headers* (req)
  (loop with hash = (make-hash-table :test 'equal)
        for (name-camelcased value) in (query-headers req)
        for name = (string-downcase name-camelcased)
        if (gethash name hash)
          do (setf (gethash name hash)
                   (format nil "~A, ~A" (gethash name hash) value))
        else
          do (setf (gethash name hash) value)
        finally (return hash)))

(defun convert-content (content multipart-p form-urlencoded-p preferred-content-type)
  (etypecase content
    (cons
     (cond (multipart-p
            (let ((boundary (make-random-string 12)))
              (values
               (let ((stream (make-instance 'fast-output-stream)))
                 (write-multipart-content content boundary stream)
                 (finish-output-stream stream))
               (format nil "~A; boundary=~A"
                       (or preferred-content-type "multipart/form-data")
                       boundary))))
           (form-urlencoded-p
            (values
             (babel:string-to-octets (quri:url-encode-params content))
             "application/x-www-form-urlencoded"))
           (t
            (error "Can't convert a CONS content"))))
    (string
     (values (babel:string-to-octets content)
             (or preferred-content-type
                 "text/plain")))
    (pathname
     (values (read-file-into-byte-vector content)
             (or preferred-content-type
                 (mimes:mime content))))
    ((array (unsigned-byte 8) (*))
     (values content
             (or preferred-content-type
                 "application/octet-stream")))
    (null
     (values (make-array 0 :element-type '(unsigned-byte 8))
             preferred-content-type))))

;; TODO: Try asynchronous
(defun request (uri &rest args
                            &key (method :get) (version 1.1)
                            content headers
                            basic-auth bearer-auth
                            cookie-jar
                            (connect-timeout *default-connect-timeout*) (read-timeout *default-read-timeout*)
                            (keep-alive t) (use-connection-pool t)
                            (max-redirects 5)
                            ssl-key-file ssl-cert-file ssl-key-password
                            stream (verbose *verbose*)
                            force-binary force-string
                            want-stream
                            ((:proxy proxy-arg) *default-proxy*)
                            (insecure *not-verify-ssl*)
                            ca-path)
  (declare (ignore version use-connection-pool
                   ssl-key-file ssl-cert-file ssl-key-password
                   stream verbose
                   ca-path))
  (let* ((uri (quri:uri uri))
         (proxy-url (resolve-proxy uri proxy-arg))
         (proxy-uri (and proxy-url (quri:uri proxy-url)))
         ;; No explicit proxy: let Windows resolve one (registry, PAC, WPAD)
         ;; unless disabled or the host is bypassed via *no-proxy*.
         (system-proxy-p (and (null proxy-uri)
                              *use-system-proxy*
                              (not (host-bypassed-p (quri:uri-host uri) *no-proxy*))))
         (content-type
           (find :content-type headers :key #'car :test #'string-equal))
         (multipart-p (or (and content-type
                               (string= (cdr content-type) "multipart/" :end1 10))
                          (and (null (cdr content-type))
                               (consp content)
                               (find-if #'pathnamep content :key #'cdr))))
         (form-urlencoded-p (or (string= (cdr content-type) "application/x-www-form-urlencoded")
                                (and (null (cdr content-type))
                                     (consp content)
                                     (not multipart-p))))
         (user-agent
           (cdr (find :user-agent headers :key #'car :test #'string-equal))))
    (multiple-value-bind (content detected-content-type)
        (convert-content content multipart-p form-urlencoded-p (cdr content-type))
      (when detected-content-type
        (if content-type
            (setf (cdr (assoc :content-type headers :test #'string-equal)) detected-content-type)
            (setf headers (append `(("Content-Type" . ,detected-content-type)) headers))))

      (when cookie-jar
        (let ((cookies
                (cookie-jar-host-cookies cookie-jar (quri:uri-host uri) (or (quri:uri-path uri) "/")
                                         :securep (string= (quri:uri-scheme uri) "https"))))
          (when cookies
            (setf headers
                  (append headers
                          `(("Cookie" . ,(write-cookie-header cookies))))))))
      (when (and proxy-uri (string-equal (quri:uri-scheme proxy-uri) "socks5"))
        (error "SOCKS5 proxies are not supported by the WinHTTP backend: ~A~%~
                Load dexador-usocket and set dex:*dexador-backend* to :usocket instead."
               proxy-url))
      (with-http-session (session (or user-agent *default-user-agent*) system-proxy-p)
        (with-connect (conn session (quri:uri-host uri) (quri:uri-port uri))
          (with-request (req conn :verb method
                                  :url (format nil "~@[~A~]~@[?~A~]"
                                               (quri:uri-path uri)
                                               (quri:uri-query uri))
                                  :https-p (equalp (quri:uri-scheme uri) "https"))
            (cond
              ((quri:uri-userinfo uri)
               (destructuring-bind (user pass) (split-sequence #\: (quri:uri-userinfo uri))
                 (set-credentials req user pass)))
	      ((and basic-auth bearer-auth)
	       (error "You should only use one Authorization header."))
	      (bearer-auth
	       (setf headers
		     (append headers
			     (list (cons "Authorization" (concatenate 'string "Bearer " bearer-auth))))))
              (basic-auth
               (set-credentials req (car basic-auth) (cdr basic-auth))))

            (when proxy-uri
              (set-request-proxy req proxy-uri)
              (let ((userinfo (quri:uri-userinfo proxy-uri)))
                (when userinfo
                  (destructuring-bind (user &optional (pass "")) (split-sequence #\: userinfo)
                    (set-credentials req user pass :basic :proxy))
                  ;; For plain HTTP the request goes to the proxy directly, so we can
                  ;; authenticate proactively instead of waiting for a 407 challenge.
                  (unless (or (equalp (quri:uri-scheme uri) "https")
                              (find "proxy-authorization" headers :key #'car :test #'string-equal))
                    (setf headers
                          (append headers
                                  (list (cons "Proxy-Authorization"
                                              (concatenate 'string "Basic "
                                                           (string-to-base64-string userinfo))))))))))

            ;; TODO: SSL arguments
            (set-option req
                        +WINHTTP_OPTION_DISABLE_FEATURE+
                        (logior +WINHTTP_DISABLE_COOKIES+
                                +WINHTTP_DISABLE_REDIRECTS+
                                (if keep-alive 0 +WINHTTP_DISABLE_KEEP_ALIVE+)))

            ;; Control single sign-on. Default *winhttp-autologon-policy* is
            ;; :MEDIUM (intranet only). :LOW would send credentials to any host.
            (set-option req +WINHTTP_OPTION_AUTOLOGON_POLICY+
                        (autologon-policy-value))

            (dolist (header headers)
              (add-request-headers req
                                   (format nil "~:(~A~): ~A" (car header) (cdr header))))

            (when (and (equalp (quri:uri-scheme uri) "https")
                       insecure)
              (set-ignore-certificates req))

            (when connect-timeout
              (set-timeouts req
                            :connect (* 1000 connect-timeout)
                            :recv (* 1000 read-timeout)))

            (send-request req content)

            (receive-response req)

            ;; Auth challenge loop: reuse the same request handle so that
            ;; connection-oriented handshakes (NTLM, Negotiate) keep their
            ;; connection and SSPI context. Credentials are set once per
            ;; target; a further 401/407 is a handshake continuation and is
            ;; only resent -- calling WinHttpSetCredentials again mid-
            ;; handshake would reset the SSPI context back to the first leg.
            (loop with answered = ()
                  repeat 3
                  for status = (query-status-code req)
                  while (member status '(401 407))
                  do (let ((target (if (eql status 407) :proxy :server))
                           (response-headers (query-headers* req)))
                       (unless (or (member target answered)
                                   (answer-auth-challenge req status response-headers
                                                          uri proxy-uri basic-auth))
                         (loop-finish))
                       (pushnew target answered)
                       (drain-response-body req)
                       (send-request req content)
                       (receive-response req)))

            (let ((status (query-status-code req))
                  (response-headers (query-headers* req)))
              (when cookie-jar
                (when-let (set-cookies (append (ensure-list (gethash "set-cookie" response-headers))
                                               (ensure-list (gethash "set-cookie2" response-headers))))
                  (merge-cookies cookie-jar
                                 (remove nil (mapcar (lambda (cookie)
                                                       (declare (type string cookie))
                                                       (unless (= (length cookie) 0)
                                                         (parse-set-cookie-header cookie
                                                                                  (quri:uri-host uri)
                                                                                  (quri:uri-path uri))))
                                                     set-cookies)))))

              ;; Redirect
              (when (and (member status '(301 302 303 307 308))
                         (gethash "location" response-headers)
                         (/= max-redirects 0))
                (let* ((location-uri (quri:uri (gethash "location" response-headers)))
                       (same-server-p (or (null (quri:uri-host location-uri))
                                          (and (string= (quri:uri-scheme location-uri)
                                                        (quri:uri-scheme uri))
                                               (string-equal (quri:uri-host location-uri)
                                                             (quri:uri-host uri))
                                               (eql (quri:uri-port location-uri)
                                                    (quri:uri-port uri)))))
                       (method
                         (if (or (= status 307) (= status 308)
                                 (member method '(:get :head) :test #'eq))
                             method
                             :get)))
                  (setf (quri:uri-userinfo location-uri) nil)
                  ;; TODO: slurp the body
                  (unless same-server-p
                    (remf args :basic-auth)
                    (remf args :bearer-auth)
                    ; Do not forward credentials to a different host — prevents leaking auth tokens to redirect targets.
                    (setf (getf args :headers)
                          (remove-if (lambda (h)
                                       (let ((name (car h)))
                                         (or (string-equal name :authorization)
                                             (string-equal name "proxy-authorization")
                                             (string-equal name "cookie"))))
                                     (getf args :headers))))
                  (return-from request
                               (apply #'request (quri:merge-uris location-uri uri)
                                      :max-redirects (1- max-redirects)
                                      :method method
                                      args))))

              (let ((body (with-fast-output (body :vector)
                            (loop with buffer = (make-array 1024 :element-type '(unsigned-byte 8))
                                  for bytes = (read-data req buffer)
                                  until (zerop bytes)
                                  do (fast-write-sequence buffer body 0 bytes)))))
                (when (gethash "content-encoding" response-headers)
                  (setf body
                        (decompress-body
                          (gethash "content-encoding" response-headers)
                          body)))

                (let ((body (if force-binary
                                body
                                (decode-body (gethash "content-type" response-headers) body
                                             :default-charset (if force-string
                                                                  babel:*default-character-encoding*
                                                                  nil)))))
                  ;; Raise an error when the HTTP response status is 4xx or 5xx.
                  (when (<= 400 status)
                    (restart-case
                        (http-request-failed status
                                             :body body
                                             :headers response-headers
                                             :uri uri
                                             :method method)
                      (retry-request ()
                        :report "Retry the same request."
                        (return-from request
                          (apply #'request uri args)))
                      (retry-insecure ()
                        :report "Retry the same request without checking for SSL certificate validity."
                        (return-from request
                          (apply #'request uri :insecure t args)))
                      (ignore-and-continue ()
                        :report "Ignore the error and continue.")))

                  ;; TODO: This obviously isn't streaming.
                  ;;   Wrapping 'req' object by gray streams would be better,
                  ;;   but freeing it could be a problem for the next.
                  (when want-stream
                    (setf body
                          (etypecase body
                            (string (make-string-input-stream body))
                            (vector (flex:make-in-memory-input-stream body)))))

                  (values body
                          status
                          response-headers
                          uri))))))))))
