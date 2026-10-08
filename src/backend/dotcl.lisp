;;;; dexador.backend.dotcl -- an HTTP transport for dexador on dotcl
;;;; (Common Lisp on .NET/CLR).
;;;;
;;;; This backend delegates the whole HTTP exchange to System.Net.Http.HttpClient
;;;; through dotcl's `dotnet:' interop -- no sockets, no cl+ssl/OpenSSL, no cffi.
;;;; It is selected on dotcl regardless of the host OS (see dexador.lisp's
;;;; *dexador-backend*), so dexador works the same on Windows/Linux/macOS without
;;;; a native winhttp / usocket+ssl transport.
;;;;
;;;; HttpClient handles TLS, redirects and transfer/content decompression itself,
;;;; so this file only converts request/response between dexador's contract and
;;;; the CLR objects.  `request' returns the same values the other backends do:
;;;;   (values body status response-headers uri).

(defpackage #:dexador.backend.dotcl
  (:nicknames :dex.dotcl)
  (:use #:cl
        #:dexador.util)
  (:import-from #:dexador.body
                #:decode-body)
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
    hash))

(defun dotnet-null-p (obj)
  ;; dotnet:invoke marshals a .NET null result to Lisp NIL.
  (null obj))

;;; ------------------------------------------------------------------
;;; Request content

(defun convert-content (content multipart-p form-urlencoded-p preferred-content-type)
  "Return (values octets content-type) for CONTENT, mirroring the other backends
for the common shapes.  Multipart is not yet supported by this backend."
  (declare (ignore multipart-p))
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
     (if form-urlencoded-p
         (values (babel:string-to-octets (quri:url-encode-params content))
                 "application/x-www-form-urlencoded")
         (error "The dotcl backend does not support multipart content yet.")))))

(defun octets->netbytes (octets)
  "Marshal a CL octet vector to a fresh .NET byte[]."
  (apply #'dotnet:new-array "System.Byte" (coerce octets 'list)))

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
;;; differ between them -- certificate policy and redirect policy.  Cookies are
;;; always off and decompression is always "All", so they are not part of the
;;; key.  Per-request timeouts are NOT a handler dimension: HttpClient.Timeout is
;;; per-client, so we leave the shared client's timeout Infinite and enforce the
;;; per-call timeout with a CancellationTokenSource instead -- that lets a single
;;; client serve calls with different timeouts.
;;;
;;; Shared clients are never disposed (disposing tears down the pool).  Only the
;;; fresh, per-request clients made for :use-connection-pool NIL / :keep-alive
;;; NIL are disposed after the body is read.

(defvar *client-cache* (make-hash-table :test 'equal)
  "config-key -> shared HttpClient. See CLIENT-CACHE-KEY.")

(defun make-http-client (insecure max-redirects)
  (let ((handler (dotnet:new "System.Net.Http.HttpClientHandler")))
    (dotnet:invoke handler "set_UseCookies" nil)
    (dotnet:invoke handler "set_AutomaticDecompression" "All")
    (if (and max-redirects (plusp max-redirects))
        (progn
          (dotnet:invoke handler "set_AllowAutoRedirect" t)
          (dotnet:invoke handler "set_MaxAutomaticRedirections" max-redirects))
        (dotnet:invoke handler "set_AllowAutoRedirect" nil))
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

(defun client-cache-key (insecure max-redirects)
  (list (and insecure t)
        (if (and max-redirects (plusp max-redirects)) max-redirects 0)))

(defun shared-client (insecure max-redirects)
  "The cached HttpClient for this config, creating it on first use.  Returns the
same object (EQ) for equal configs, so repeated same-config calls reuse it."
  (let ((key (client-cache-key insecure max-redirects)))
    (or (gethash key *client-cache*)
        (setf (gethash key *client-cache*)
              (make-http-client insecure max-redirects)))))

(defun clear-client-cache ()
  "Dispose and drop every cached shared HttpClient.  Bound to dexador's
CLEAR-CONNECTION-POOL below."
  (maphash (lambda (key client)
             (declare (ignore key))
             (ignore-errors (dotnet:invoke client "Dispose")))
           *client-cache*)
  (clrhash *client-cache*))

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
                         proxy
                         (insecure *not-verify-ssl*)
                         ca-path)
  (declare (ignore version
                   ssl-key-file ssl-cert-file ssl-key-password
                   stream verbose proxy ca-path args))
  (let* ((uri (quri:uri uri))
         (content-type-cell (find :content-type headers
                                  :key #'car :test #'string-equal))
         (preferred-content-type (cdr content-type-cell))
         (form-urlencoded-p (and (consp content)
                                 (or (null preferred-content-type)
                                     (string-equal preferred-content-type
                                                   "application/x-www-form-urlencoded"))))
         (user-agent (cdr (find :user-agent headers
                                :key #'car :test #'string-equal))))
    (multiple-value-bind (body-octets detected-content-type)
        (convert-content content nil form-urlencoded-p preferred-content-type)
      (when (and detected-content-type (null content-type-cell))
        (setf headers (append headers `(("Content-Type" . ,detected-content-type)))))

      ;; Cookie jar -> Cookie header
      (when cookie-jar
        (let ((cookies (cookie-jar-host-cookies
                        cookie-jar (quri:uri-host uri) (or (quri:uri-path uri) "/")
                        :securep (string= (quri:uri-scheme uri) "https"))))
          (when cookies
            (setf headers (append headers
                                  `(("Cookie" . ,(write-cookie-header cookies))))))))

      ;; Connection reuse: the shared cached client (and .NET's connection pool
      ;; under it) is used whenever :use-connection-pool and :keep-alive are both
      ;; true (the defaults).  Otherwise a throwaway client is made, told to close
      ;; the connection, and disposed after the body is read.
      (let* ((fresh (or (not use-connection-pool) (not keep-alive)))
             (client (if fresh
                         (make-http-client insecure max-redirects)
                         (shared-client insecure max-redirects)))
             (cts (dotnet:new "System.Threading.CancellationTokenSource"))
             (timeout-ms (* 1000 (+ (or connect-timeout 0) (or read-timeout 0)))))
        (when (plusp timeout-ms)
          ;; Per-request timeout, independent of the client-wide (Infinite) one.
          (dotnet:invoke cts "CancelAfter" timeout-ms))
        (unwind-protect
             (let* ((method-obj (dotnet:new "System.Net.Http.HttpMethod"
                                            (string-upcase (string method))))
                    (req (dotnet:new "System.Net.Http.HttpRequestMessage"
                                     method-obj (quri:render-uri uri)))
                    (req-headers (dotnet:invoke req "get_Headers"))
                    (content-obj nil)
                    (content-headers nil))
               ;; Body
               (when body-octets
                 (setf content-obj (dotnet:new "System.Net.Http.ByteArrayContent"
                                               (octets->netbytes body-octets)))
                 (setf content-headers (dotnet:invoke content-obj "get_Headers"))
                 (dotnet:invoke req "set_Content" content-obj))
               (when fresh
                 (add-header req-headers content-headers "Connection" "close"))
               ;; User-Agent
               (add-header req-headers content-headers
                           "User-Agent" (or user-agent *default-user-agent*))
               ;; Authorization
               (cond
                 ((quri:uri-userinfo uri)
                  (add-header req-headers content-headers "Authorization"
                              (concatenate 'string "Basic "
                                           (cl-base64:string-to-base64-string
                                            (quri:uri-userinfo uri)))))
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
               ;; Remaining user headers (Content-Type included; skip User-Agent,
               ;; already added).
               (dolist (h headers)
                 (unless (string-equal (car h) :user-agent)
                   (add-header req-headers content-headers (car h) (cdr h))))

               ;; Send (per-request timeout via the cancellation token)
               (let* ((response (dotnet:await
                                 (dotnet:invoke client "SendAsync" req
                                                (dotnet:invoke cts "get_Token"))))
                      (status (dotnet:static "System.Convert" "ToInt32"
                                             (dotnet:invoke response "get_StatusCode")))
                      (response-headers (response-headers-hash response))
                      (final-uri (let ((rm (dotnet:invoke response "get_RequestMessage")))
                                   (if (dotnet-null-p rm)
                                       uri
                                       (quri:uri (dotnet:invoke
                                                  (dotnet:invoke rm "get_RequestUri")
                                                  "get_AbsoluteUri"))))))
                 ;; Cookie jar update
                 (when cookie-jar
                   (when-let (set-cookies (ensure-list (gethash "set-cookie" response-headers)))
                     (merge-cookies cookie-jar
                                    (remove nil (mapcar
                                                 (lambda (cookie)
                                                   (unless (zerop (length cookie))
                                                     (parse-set-cookie-header
                                                      cookie (quri:uri-host uri)
                                                      (quri:uri-path uri))))
                                                 set-cookies)))))
                 ;; Body bytes
                 (let* ((content-obj (dotnet:invoke response "get_Content"))
                        (netbytes (dotnet:await (dotnet:invoke content-obj "ReadAsByteArrayAsync")))
                        (octets (netbytes->octets netbytes))
                        (body (if force-binary
                                  octets
                                  (decode-body (gethash "content-type" response-headers) octets
                                               :default-charset (if force-string
                                                                    babel:*default-character-encoding*
                                                                    nil)))))
                   (when (<= 400 status)
                     (http-request-failed status
                                          :body body
                                          :headers response-headers
                                          :uri final-uri
                                          :method method))
                   (when want-stream
                     (setf body
                           (etypecase body
                             (string (make-string-input-stream body))
                             (vector (dotnet:to-stream
                                      (dotnet:new "System.IO.MemoryStream" netbytes)
                                      :binary t)))))
                   (values body status response-headers final-uri))))
          ;; Cleanup: never dispose the shared client (that kills the pool);
          ;; dispose only the throwaway one and always the token source.
          (ignore-errors (dotnet:invoke cts "Dispose"))
          (when fresh (ignore-errors (dotnet:invoke client "Dispose"))))))))

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
