;;;; dexador.backend.dotcl -- an HTTP transport for dexador on dotcl
;;;; (Common Lisp on .NET/CLR).
;;;;
;;;; This backend delegates the whole HTTP exchange to System.Net.Http.HttpClient
;;;; through dotcl's `dotnet:' interop -- no sockets, no cl+ssl/OpenSSL, no cffi.
;;;; It is selected on dotcl regardless of the host OS (see dexador.lisp's
;;;; *dexador-backend*), so dexador works the same on Windows/Linux/macOS without
;;;; a native winhttp / usocket+ssl transport.
;;;;
;;;; HttpClient handles TLS and transfer/content decompression itself and REQUEST
;;;; follows redirects, so this file only converts request/response between
;;;; dexador's contract and the CLR objects.  `request' returns the same values the other backends do:
;;;;   (values body status response-headers uri).

(defpackage #:dexador.backend.dotcl
  (:nicknames :dex.dotcl)
  (:use #:cl
        #:dexador.util
        #:dexador.restarts)
  (:import-from #:dexador.body
                #:decode-body
                #:write-multipart-content)
  (:import-from #:fast-io
                #:fast-output-stream
                #:finish-output-stream)
  (:import-from #:dexador.error
                #:http-request-failed)
  (:import-from #:cl-cookie
                #:cookie-jar-host-cookies
                #:write-cookie-header
                #:parse-set-cookie-header
                #:merge-cookies)
  (:import-from #:alexandria
                #:when-let
                #:ensure-list)
  (:export #:request
           #:clear-client-cache))
(in-package #:dexador.backend.dotcl)

;;; ------------------------------------------------------------------
;;; byte[] <-> CL octet-vector

(defun netbytes->octets (netbytes)
  "Copy a .NET byte[] (a dotnet object) into a fresh (unsigned-byte 8) vector.
Reads it through an in-memory binary stream so the copy happens in the runtime
rather than element-by-element across the interop boundary."
  (let* ((len (dotnet:invoke netbytes "get_Length"))
         (vec (make-array len :element-type '(unsigned-byte 8)))
         (ms (dotnet:new "System.IO.MemoryStream" netbytes))
         (in (dotnet:to-stream ms :binary t)))
    (read-sequence vec in)
    (dotnet:invoke ms "Dispose")
    vec))

;;; ------------------------------------------------------------------
;;; Response header parsing
;;;
;;; HttpHeaders.ToString() emits one "Name: value" line per header (multiple
;;; values comma-joined), which is enough to reconstruct dexador's header hash
;;; without marshalling IEnumerable<KeyValuePair<string,IEnumerable<string>>>.

(defun %split-lines (string)
  (loop with start = 0
        for nl = (position #\Newline string :start start)
        for line = (string-right-trim '(#\Return #\Space #\Tab)
                                       (subseq string start (or nl (length string))))
        unless (zerop (length line)) collect line
        while nl do (setf start (1+ nl))))

(defun %merge-header-lines (string hash)
  (dolist (line (%split-lines string) hash)
    (let ((colon (position #\: line)))
      (when colon
        (let ((name (string-downcase (string-trim '(#\Space #\Tab)
                                                   (subseq line 0 colon))))
              (value (string-trim '(#\Space #\Tab) (subseq line (1+ colon)))))
          (if (gethash name hash)
              (setf (gethash name hash)
                    (format nil "~A, ~A" (gethash name hash) value))
              (setf (gethash name hash) value)))))))

(defun response-headers-hash (response)
  "Build dexador's response-header hash (lowercased name -> value string) from
both the message headers and the content headers of RESPONSE."
  (let ((hash (make-hash-table :test 'equal)))
    (%merge-header-lines
     (dotnet:invoke (dotnet:invoke response "get_Headers") "ToString") hash)
    (let ((content (dotnet:invoke response "get_Content")))
      (unless (dotnet-null-p content)
        (%merge-header-lines
         (dotnet:invoke (dotnet:invoke content "get_Headers") "ToString") hash)))
    ;; Set-Cookie lines must stay separate: a cookie's Expires contains a comma.
    (let ((headers (dotnet:invoke response "get_Headers")))
      (when (dotnet:invoke headers "Contains" "Set-Cookie")
        (setf (gethash "set-cookie" hash)
              (%split-lines (dotnet:static "System.String" "Join" (string #\Newline)
                                           (dotnet:invoke headers "GetValues" "Set-Cookie"))))))
    hash))

(defun dotnet-null-p (obj)
  ;; dotnet:invoke marshals a .NET null result to Lisp NIL.
  (null obj))

;;; ------------------------------------------------------------------
;;; Request content

(defun convert-content (content multipart-p form-urlencoded-p preferred-content-type)
  "Return (values octets content-type) for CONTENT, as the winhttp backend does."
  (etypecase content
    (null
     (values nil preferred-content-type))
    (string
     (values (babel:string-to-octets content)
             (or preferred-content-type "text/plain")))
    ((array (unsigned-byte 8) (*))
     (values content
             (or preferred-content-type "application/octet-stream")))
    (pathname
     (values (alexandria:read-file-into-byte-vector content)
             (or preferred-content-type (mimes:mime content))))
    (cons
     (cond (multipart-p
            (let ((boundary (make-random-string 12))
                  (stream (make-instance 'fast-output-stream)))
              (write-multipart-content content boundary stream)
              (values (finish-output-stream stream)
                      (format nil "~A; boundary=~A"
                              (or preferred-content-type "multipart/form-data")
                              boundary))))
           (form-urlencoded-p
            (values (babel:string-to-octets (quri:url-encode-params content))
                    "application/x-www-form-urlencoded"))
           (t
            (error "Can't convert a CONS content"))))))

(defun octets->netbytes (octets)
  "OCTETS as a value dotcl passes to .NET as a byte[]: a
(simple-array (unsigned-byte 8) (*)) is marshalled to byte[] as it is, so
only another kind of vector is copied. No per-byte argument list is built."
  (coerce octets '(simple-array (unsigned-byte 8) (*))))

;;; ------------------------------------------------------------------
;;; Header wiring
;;;
;;; HttpClient partitions headers into request headers and content headers and
;;; rejects the wrong one, so try the request-header collection first and fall
;;; back to the content-header collection.

(defun add-header (req-headers content-headers name value)
  (let ((name (string name))
        (value (if (stringp value) value (princ-to-string value))))
    (unless (dotnet:invoke req-headers "TryAddWithoutValidation" name value)
      (when content-headers
        (dotnet:invoke content-headers "TryAddWithoutValidation" name value)))))

;;; ------------------------------------------------------------------
;;; HttpClient pooling
;;;
;;; Connection reuse is delegated to .NET's own connection pool: each HttpClient
;;; (really its HttpClientHandler / SocketsHttpHandler) keeps a pool of live
;;; keep-alive connections per origin.  Reusing one HttpClient across requests
;;; therefore reuses TCP+TLS connections; newing one per request (as the first
;;; cut did) throws that away.
;;;
;;; Cached clients are keyed only by the handler-config dimensions that must
;;; differ between them -- certificate policy and proxy.  Cookies are always off,
;;; automatic redirects are always off and decompression is always "All", so they
;;; are not part of the key.  Per-request timeouts are NOT a handler dimension: HttpClient.Timeout is
;;; per-client, so we leave the shared client's timeout Infinite and enforce the
;;; per-call timeout with a CancellationTokenSource instead -- that lets a single
;;; client serve calls with different timeouts.
;;;
;;; Shared clients are never disposed (disposing tears down the pool).  Only the
;;; fresh, per-request clients made for :use-connection-pool NIL / :keep-alive
;;; NIL are disposed after the body is read.

(defvar *client-cache* (make-hash-table :test 'equal)
  "config-key -> shared HttpClient. See CLIENT-CACHE-KEY.")

(defun make-web-proxy (proxy-uri)
  (let* ((uri (quri:uri proxy-uri))
         (userinfo (quri:uri-userinfo uri))
         (web-proxy (dotnet:new "System.Net.WebProxy"
                                (format nil "~A://~A:~A"
                                        (quri:uri-scheme uri)
                                        (quri:uri-host uri)
                                        (quri:uri-port uri)))))
    (when userinfo
      (let ((colon (position #\: userinfo)))
        (dotnet:invoke web-proxy "set_Credentials"
                       (dotnet:new "System.Net.NetworkCredential"
                                   (subseq userinfo 0 colon)
                                   (if colon (subseq userinfo (1+ colon)) "")))))
    web-proxy))

(defun make-http-client (insecure proxy)
  "PROXY is NIL (the .NET default), :NONE (no handler proxy) or a proxy URI string."
  (let ((handler (dotnet:new "System.Net.Http.HttpClientHandler")))
    (dotnet:invoke handler "set_UseCookies" nil)
    (dotnet:invoke handler "set_AutomaticDecompression" "All")
    ;; REQUEST follows redirects itself.
    (dotnet:invoke handler "set_AllowAutoRedirect" nil)
    (cond ((null proxy))
          ((eq proxy :none)
           (dotnet:invoke handler "set_UseProxy" nil))
          (t
           (dotnet:invoke handler "set_UseProxy" t)
           (dotnet:invoke handler "set_Proxy" (make-web-proxy proxy))))
    (when insecure
      ;; Accept any server certificate (used when :insecure / *not-verify-ssl*).
      (dotnet:invoke handler "set_ServerCertificateCustomValidationCallback"
                     (dotnet:static "System.Net.Http.HttpClientHandler"
                                    "get_DangerousAcceptAnyServerCertificateValidator")))
    (let ((client (dotnet:new "System.Net.Http.HttpClient" handler)))
      ;; Timeout is enforced per request with a CancellationToken; disable the
      ;; client-wide timeout so one shared client can serve varying timeouts.
      ;; TimeSpan.FromMilliseconds(-1) is Timeout.InfiniteTimeSpan (a static
      ;; readonly field, so reachable as a method rather than a property getter).
      (dotnet:invoke client "set_Timeout"
                     (dotnet:static "System.TimeSpan" "FromMilliseconds" -1.0d0))
      client)))

(defun client-cache-key (insecure proxy)
  (list (and insecure t) proxy))

(defun shared-client (insecure proxy)
  "The cached HttpClient for this config, creating it on first use.  Returns the
same object (EQ) for equal configs, so repeated same-config calls reuse it."
  (let ((key (client-cache-key insecure proxy)))
    (or (gethash key *client-cache*)
        (setf (gethash key *client-cache*)
              (make-http-client insecure proxy)))))

(defun clear-client-cache ()
  "Dispose and drop every cached shared HttpClient.  Bound to dexador's
CLEAR-CONNECTION-POOL below."
  (maphash (lambda (key client)
             (declare (ignore key))
             (ignore-errors (dotnet:invoke client "Dispose")))
           *client-cache*)
  (clrhash *client-cache*))

;;; ------------------------------------------------------------------
;;; Proxy, redirect and body helpers

(defun usable-proxy-uri (proxy)
  (let ((uri (and proxy (quri:uri proxy))))
    (and uri
         (member (quri:uri-scheme uri) '("http" "https" "socks5") :test #'equal)
         uri)))

(defun proxy-via-origin-form-p (proxy-uri uri)
  "True for a plain http URI behind an http(s) proxy.  As in the usocket backend,
such a request goes to the proxy in origin form with the target's Host header."
  (and (string= (quri:uri-scheme uri) "http")
       (not (string= (quri:uri-scheme proxy-uri) "socks5"))))

(defun origin-form-uri (proxy-uri uri)
  (quri:make-uri :scheme (quri:uri-scheme proxy-uri)
                 :host (quri:uri-host proxy-uri)
                 :port (quri:uri-port proxy-uri)
                 :path (or (quri:uri-path uri) "/")
                 :query (quri:uri-query uri)))

(defun host-header-value (uri)
  (let ((port (quri:uri-port uri)))
    (if (and port (/= port (if (string= (quri:uri-scheme uri) "https") 443 80)))
        (format nil "~A:~A" (quri:uri-host uri) port)
        (quri:uri-host uri))))

(defun strip-userinfo (uri)
  (let ((copy (quri:uri (quri:render-uri uri))))
    (setf (quri:uri-userinfo copy) nil)
    copy))

(defun same-server-p (location uri)
  (or (null (quri:uri-host location))
      (and (string= (quri:uri-scheme location) (quri:uri-scheme uri))
           (string-equal (quri:uri-host location) (quri:uri-host uri))
           (eql (quri:uri-port location) (quri:uri-port uri)))))

(defun remove-headers (names headers)
  (remove-if (lambda (h) (member (car h) names :test #'string-equal)) headers))

;; A handler-bind handler for a .NET exception runs after the stack has unwound,
;; so restarts established around the call are gone.  Catching and signalling it
;; again here keeps them available.
(defmacro resignal-errors (&body body)
  `(handler-case (progn ,@body)
     (error (e) (error e))))

(defun convert-body (netbytes content-type force-binary force-string want-stream)
  (let ((default-charset (if force-string babel:*default-character-encoding* nil)))
    (if want-stream
        (let ((stream (dotnet:to-stream (dotnet:new "System.IO.MemoryStream" netbytes)
                                        :binary t)))
          (if force-binary
              stream
              (decode-body content-type stream :default-charset default-charset)))
        (let ((octets (netbytes->octets netbytes)))
          (if force-binary
              octets
              (decode-body content-type octets :default-charset default-charset))))))

;;; ------------------------------------------------------------------
;;; request

(defun request (uri &rest args
                    &key (method :get) (version 1.1)
                         content headers
                         basic-auth bearer-auth
                         cookie-jar
                         (connect-timeout *default-connect-timeout*)
                         (read-timeout *default-read-timeout*)
                         (keep-alive t) (use-connection-pool t)
                         (max-redirects 5)
                         ssl-key-file ssl-cert-file ssl-key-password
                         stream (verbose *verbose*)
                         force-binary force-string
                         want-stream
                         (proxy *default-proxy*)
                         (insecure *not-verify-ssl*)
                         ca-path)
  (declare (ignore version
                   ssl-key-file ssl-cert-file ssl-key-password
                   stream verbose ca-path args))
  (let* ((uri (quri:uri uri))
         (userinfo (quri:uri-userinfo uri))
         (proxy-uri (usable-proxy-uri proxy))
         (content-type-cell (find :content-type headers
                                  :key #'car :test #'string-equal))
         (preferred-content-type (cdr content-type-cell))
         (multipart-p (or (and preferred-content-type
                               (>= (length preferred-content-type) 10)
                               (string-equal "multipart/" preferred-content-type :end2 10))
                          (and (null preferred-content-type)
                               (consp content)
                               (find-if #'pathnamep content :key #'cdr))))
         (form-urlencoded-p (and (consp content)
                                 (not multipart-p)
                                 (or (null preferred-content-type)
                                     (string-equal preferred-content-type
                                                   "application/x-www-form-urlencoded"))))
         ;; Connection reuse: the shared cached client (and .NET's connection pool
         ;; under it) is used whenever :use-connection-pool and :keep-alive are both
         ;; true (the defaults).  Otherwise a throwaway client is made, told to close
         ;; the connection, and disposed when REQUEST returns.
         (fresh (or (not use-connection-pool) (not keep-alive)))
         (fresh-clients '())
         (redirects-left (or max-redirects 0))
         (timeout-ms (* 1000 (+ (or connect-timeout 0) (or read-timeout 0)))))
    (multiple-value-bind (body-octets detected-content-type)
        (convert-content content multipart-p form-urlencoded-p preferred-content-type)
      (when detected-content-type
        (cond ((null content-type-cell)
               (setf headers (append headers `(("Content-Type" . ,detected-content-type)))))
              (multipart-p
               (setf headers (substitute (cons (car content-type-cell) detected-content-type)
                                         content-type-cell headers)))))
      (labels
          ((client-for (target)
             ;; Plain http through an http proxy is sent to the proxy directly,
             ;; so the handler must not proxy it a second time.
             (let ((handler-proxy (cond ((null proxy-uri) nil)
                                        ((proxy-via-origin-form-p proxy-uri target) :none)
                                        (t (quri:render-uri proxy-uri)))))
               (if fresh
                   (let ((client (make-http-client insecure handler-proxy)))
                     (push client fresh-clients)
                     client)
                   (shared-client insecure handler-proxy))))

           (build-request (target request-headers)
             (let* ((via-origin-form (and proxy-uri (proxy-via-origin-form-p proxy-uri target)))
                    (req (dotnet:new "System.Net.Http.HttpRequestMessage"
                                     (dotnet:new "System.Net.Http.HttpMethod"
                                                 (string-upcase (string method)))
                                     (quri:render-uri (if via-origin-form
                                                          (origin-form-uri proxy-uri target)
                                                          target))))
                    (req-headers (dotnet:invoke req "get_Headers"))
                    (content-headers nil)
                    (user-agent (cdr (find :user-agent request-headers
                                           :key #'car :test #'string-equal))))
               (when body-octets
                 (let ((content-obj (dotnet:new "System.Net.Http.ByteArrayContent"
                                                (octets->netbytes body-octets))))
                   (setf content-headers (dotnet:invoke content-obj "get_Headers"))
                   (dotnet:invoke req "set_Content" content-obj)))
               (when fresh
                 (add-header req-headers content-headers "Connection" "close"))
               (when via-origin-form
                 (dotnet:invoke req-headers "set_Host" (host-header-value target))
                 (when-let (proxy-userinfo (quri:uri-userinfo proxy-uri))
                   (add-header req-headers content-headers "Proxy-Authorization"
                               (concatenate 'string "Basic "
                                            (cl-base64:string-to-base64-string proxy-userinfo)))))
               (add-header req-headers content-headers
                           "User-Agent" (or user-agent *default-user-agent*))
               (cond
                 (userinfo
                  (add-header req-headers content-headers "Authorization"
                              (concatenate 'string "Basic "
                                           (cl-base64:string-to-base64-string userinfo))))
                 ((and basic-auth bearer-auth)
                  (error "You should only use one Authorization header."))
                 (bearer-auth
                  (add-header req-headers content-headers "Authorization"
                              (concatenate 'string "Bearer " bearer-auth)))
                 (basic-auth
                  (add-header req-headers content-headers "Authorization"
                              (concatenate 'string "Basic "
                                           (cl-base64:string-to-base64-string
                                            (format nil "~A:~A"
                                                    (car basic-auth) (cdr basic-auth)))))))
               (dolist (h request-headers)
                 (unless (string-equal (car h) :user-agent)
                   (add-header req-headers content-headers (car h) (cdr h))))
               req))

           (cookie-headers-for (target)
             (when cookie-jar
               (let ((cookies (cookie-jar-host-cookies
                               cookie-jar (quri:uri-host target) (or (quri:uri-path target) "/")
                               :securep (string= (quri:uri-scheme target) "https"))))
                 (when cookies
                   `(("Cookie" . ,(write-cookie-header cookies)))))))

           (store-cookies (response-headers target)
             (when cookie-jar
               (when-let (set-cookies (append (ensure-list (gethash "set-cookie" response-headers))
                                              (ensure-list (gethash "set-cookie2" response-headers))))
                 (merge-cookies cookie-jar
                                (remove nil (mapcar
                                             (lambda (cookie)
                                               (unless (zerop (length cookie))
                                                 (parse-set-cookie-header
                                                  cookie (quri:uri-host target)
                                                  (quri:uri-path target))))
                                             set-cookies))))))

           (follow-redirect (location target status)
             (let ((same-server (same-server-p location target)))
               (setf (quri:uri-userinfo location) nil)
               (setf uri (quri:merge-uris location target))
               (decf redirects-left)
               (unless same-server
                 (setf userinfo nil
                       basic-auth nil
                       bearer-auth nil
                       headers (remove-headers '(:authorization :proxy-authorization :cookie)
                                               headers)))
               (unless (or (= status 307) (= status 308)
                           (member method '(:get :head)))
                 (setf method :get
                       body-octets nil
                       headers (remove-headers '(:content-type :content-length) headers)))))

           (attempt ()
             ;; One exchange with the current URI.  Returns after setting up the
             ;; next URI when the response is a redirect, and otherwise returns
             ;; from REQUEST.
             (let* ((target (strip-userinfo uri))
                    (client (client-for target))
                    (cts (dotnet:new "System.Threading.CancellationTokenSource")))
               (when (plusp timeout-ms)
                 ;; Per-request timeout, independent of the client-wide (Infinite) one.
                 (dotnet:invoke cts "CancelAfter" timeout-ms))
               (unwind-protect
                    (let* ((response (resignal-errors
                                       (dotnet:await
                                        (dotnet:invoke client "SendAsync"
                                                       (build-request target
                                                                      (append headers
                                                                              (cookie-headers-for target)))
                                                       (dotnet:invoke cts "get_Token")))))
                           (status (dotnet:static "System.Convert" "ToInt32"
                                                  (dotnet:invoke response "get_StatusCode")))
                           (response-headers (response-headers-hash response))
                           (location (gethash "location" response-headers)))
                      (store-cookies response-headers target)
                      (when (and (member status '(301 302 303 307 308))
                                 location
                                 (/= redirects-left 0))
                        (ignore-errors (dotnet:invoke response "Dispose"))
                        (follow-redirect (quri:uri location) target status)
                        (return-from attempt))
                      (let* ((content-obj (dotnet:invoke response "get_Content"))
                             (netbytes (resignal-errors
                                         (dotnet:await
                                          (dotnet:invoke content-obj "ReadAsByteArrayAsync"))))
                             (body (convert-body netbytes (gethash "content-type" response-headers)
                                                 force-binary force-string want-stream)))
                        (when (<= 400 status)
                          (restart-case
                              (http-request-failed status
                                                   :body body
                                                   :headers response-headers
                                                   :uri target
                                                   :method method)
                            (ignore-and-continue ())))
                        (return-from request
                          (values body status response-headers target))))
                 (ignore-errors (dotnet:invoke cts "Dispose"))))))
        (unwind-protect
             (loop
               (restart-case (attempt)
                 (retry-request ()
                   :report "Retry the same request.")))
          ;; Never dispose the shared client (that kills the pool).
          (dolist (client fresh-clients)
            (ignore-errors (dotnet:invoke client "Dispose"))))))))

;;; ------------------------------------------------------------------
;;; Give dexador's public CLEAR-CONNECTION-POOL a dotcl meaning: dispose our
;;; cached HttpClients too, so users have one uniform "drop pooled connections"
;;; entry point.  Wrapped once (the defvar guard survives file reloads).

(defvar *clear-connection-pool-hooked* nil)

(unless *clear-connection-pool-hooked*
  (let ((orig (fdefinition 'dexador.connection-cache:clear-connection-pool)))
    (setf (fdefinition 'dexador.connection-cache:clear-connection-pool)
          (lambda (&rest args)
            (clear-client-cache)
            (apply orig args)))
    (setf *clear-connection-pool-hooked* t)))
