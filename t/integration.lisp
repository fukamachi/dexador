(in-package :cl-user)
(defpackage dexador-integration-test
  (:use :cl
        :rove)
  (:import-from :clack.test
                :*clack-test-port*
                :localhost))
(in-package :dexador-integration-test)

;;; Integration tests that route requests through a real external proxy.
;;;
;;; The proxies are started outside the suite (see .github/workflows/ci.yml,
;;; which runs mitmproxy). Any RFC-compliant HTTP proxy works: traversal is
;;; detected through the Via header that proxies MUST add to forwarded
;;; requests (RFC 9110 section 7.6.3), echoed back by the test origin.
;;; mitmproxy needs .github/mitmproxy-addon.py to add Via.
;;;
;;; Expected environment:
;;;   DEXADOR_TEST_PROXY        e.g. http://127.0.0.1:3128 (no auth)
;;;   DEXADOR_TEST_AUTH_PROXY   e.g. http://127.0.0.1:3129 (Basic dexador:test)
;;;   DEXADOR_TEST_SOCKS5_PROXY e.g. socks5://127.0.0.1:3130
;;; Tests are skipped when none of these are set.
;;;
;;; HTTPS-through-proxy tests additionally need DEXADOR_TEST_PROXY_CA: the
;;; path to the proxy's self-signed CA certificate (mitmproxy's
;;; ~/.mitmproxy/mitmproxy-ca-cert.pem). The proxy terminates TLS with certs
;;; signed by that CA and forwards to the plain-HTTP origin (the addon
;;; downgrades the upstream hop; run mitmproxy with upstream_cert=false and
;;; connection_strategy=lazy). On Windows the CA must also be imported into
;;; the certificate store for the winhttp backend:
;;;   certutil -addstore Root %USERPROFILE%\.mitmproxy\mitmproxy-ca-cert.cer

(defun random-port ()
  ;; IGNORE-ERRORS: binding can fail with errors clack.test doesn't handle,
  ;; e.g. WSAEACCES when the port is in a Windows excluded port range.
  (loop for port from (+ 50000 (random 1000)) upto 60000
        if (ignore-errors (clack.test::port-available-p port))
          return port))

(defmacro testing-app ((desc) app &body body)
  ;; Disable clack.test's own port picking; see RANDOM-PORT above.
  `(let* ((clack.test:*random-port* nil)
          (*clack-test-port* (random-port))
          (clack.test:*clack-test-access-port* *clack-test-port*))
     (clack.test:testing-app ,desc ,app
       (let ((dex:*use-connection-pool* nil))
         (dex:clear-connection-pool)
         ,@body))))

(defmacro with-proxy-capable-backend (() &body body)
  "Use the usocket backend on Windows: these tests reach the origin via
127.0.0.1 and WinHTTP never proxies loopback addresses. The winhttp
backend has its own tests below against a routable origin address."
  #-windows `(progn ,@body)
  #+windows `(let ((dex:*dexador-backend* :usocket))
               ,@body))

(defun env-url (name)
  (let ((value (uiop:getenv name)))
    (and value (plusp (length value)) value)))

(defun url-with-userinfo (url userinfo)
  (let ((uri (quri:uri url)))
    (setf (quri:uri-userinfo uri) userinfo)
    (quri:render-uri uri)))

(defparameter *echo-via-app*
  (lambda (env)
    ;; Body is "<request-uri>|<via header or empty>", so the client can see
    ;; both what path arrived and whether a proxy forwarded the request.
    (let ((body (format nil "~A|~A"
                        (getf env :request-uri)
                        (or (gethash "via" (getf env :headers)) ""))))
      `(200 (:content-length ,(length body)) (,body)))))

(defun get-via (uri &rest args)
  "GET through dexador; return (VALUES status path via-header)."
  (multiple-value-bind (body code) (apply #'dex:get uri args)
    (let ((bar (position #\| body)))
      (values code (subseq body 0 bar) (subseq body (1+ bar))))))

(deftest external-proxy-integration
  (let ((proxy-url (env-url "DEXADOR_TEST_PROXY"))
        (auth-proxy-url (env-url "DEXADOR_TEST_AUTH_PROXY"))
        (socks5-url (env-url "DEXADOR_TEST_SOCKS5_PROXY")))
    (if (not (or proxy-url auth-proxy-url socks5-url))
        (skip "Set DEXADOR_TEST_PROXY (and friends) to run external proxy integration tests")
        (with-proxy-capable-backend ()
          (testing-app ("external proxy integration") *echo-via-app*
            (testing "direct request carries no Via header"
              (multiple-value-bind (code path via) (get-via (localhost "/direct"))
                (ok (eql code 200))
                (ok (equal path "/direct"))
                (ok (zerop (length via)))))
            (when proxy-url
              (testing "plain forward proxy adds Via to the forwarded request"
                (multiple-value-bind (code path via)
                    (get-via (localhost "/via-proxy") :proxy proxy-url)
                  (ok (eql code 200))
                  (ok (equal path "/via-proxy"))
                  (ok (plusp (length via))))))
            (when auth-proxy-url
              (testing "auth proxy without credentials -> 407"
                (handler-case
                    (progn (dex:get (localhost "/secret") :proxy auth-proxy-url)
                           (fail "Expected HTTP-REQUEST-PROXY-AUTHENTICATION-REQUIRED"))
                  (dex:http-request-proxy-authentication-required (e)
                    (ok (eql (dex:response-status e) 407)))))
              (testing "auth proxy with credentials"
                (multiple-value-bind (code path via)
                    (get-via (localhost "/secret")
                             :proxy (url-with-userinfo auth-proxy-url "dexador:test"))
                  (ok (eql code 200))
                  (ok (equal path "/secret"))
                  (ok (plusp (length via))))))
            (when socks5-url
              ;; A SOCKS5 proxy is layer-4 and never touches HTTP headers, so
              ;; only reachability is asserted. (mitmproxy in socks5 mode is
              ;; HTTP-aware and does add Via, but a pure SOCKS proxy won't.)
              (testing "socks5 proxy"
                (multiple-value-bind (code path via)
                    (get-via (localhost "/via-socks5") :proxy socks5-url)
                  (declare (ignore via))
                  (ok (eql code 200))
                  (ok (equal path "/via-socks5"))))))))))

(deftest https-proxy-integration
  (let ((proxy-url (env-url "DEXADOR_TEST_PROXY"))
        (auth-proxy-url (env-url "DEXADOR_TEST_AUTH_PROXY"))
        (ca-file (env-url "DEXADOR_TEST_PROXY_CA")))
    (cond
      ((not ca-file)
       (skip "Set DEXADOR_TEST_PROXY_CA (the proxy's CA certificate) to run HTTPS proxy tests"))
      ((not (or proxy-url auth-proxy-url))
       (skip "Set DEXADOR_TEST_PROXY / DEXADOR_TEST_AUTH_PROXY to run HTTPS proxy tests"))
      ((member :dexador-no-ssl *features*)
       (skip "SSL support is disabled (:dexador-no-ssl)"))
      (t
       (with-proxy-capable-backend ()
         (testing-app ("https through proxy (usocket)") *echo-via-app*
           ;; "localhost" (not 127.0.0.1) so the MITM certificate carries a
           ;; DNS SAN and standard hostname verification applies.
           (flet ((https-url (path)
                    (format nil "https://localhost:~D~A" *clack-test-port* path)))
             (when proxy-url
               (testing "CONNECT tunnel with verified certificate"
                 (multiple-value-bind (code path via)
                     (get-via (https-url "/https-proxy") :proxy proxy-url :ca-path ca-file)
                   (ok (eql code 200))
                   (ok (equal path "/https-proxy"))
                   (ok (plusp (length via))))))
             (when auth-proxy-url
               (testing "authenticated CONNECT"
                 (multiple-value-bind (code path via)
                     (get-via (https-url "/https-auth")
                              :proxy (url-with-userinfo auth-proxy-url "dexador:test")
                              :ca-path ca-file)
                   (ok (eql code 200))
                   (ok (equal path "/https-auth"))
                   (ok (plusp (length via)))))))))))))

;;; WinHTTP backend proxy tests (Windows only).
;;;
;;; WinHTTP never proxies requests to loopback addresses, so the test origin
;;; must be reached through a routable address of this machine, given via
;;; DEXADOR_TEST_ORIGIN_HOST (e.g. the runner's LAN IP). The origin is bound
;;; to all interfaces for these tests.

#+windows
(progn

(defun origin-host ()
  (env-url "DEXADOR_TEST_ORIGIN_HOST"))

(defun origin (path)
  (format nil "http://~A:~D~A" (origin-host) *clack-test-port* path))

(defmacro testing-app-all-interfaces ((desc) app &body body)
  `(let ((clack.test:*clackup-additional-args* '(:address "0.0.0.0")))
     (testing-app (,desc) ,app ,@body)))

(deftest winhttp-explicit-proxy
  (let ((proxy-url (env-url "DEXADOR_TEST_PROXY"))
        (auth-proxy-url (env-url "DEXADOR_TEST_AUTH_PROXY")))
    (cond
      ((not (origin-host))
       (skip "Set DEXADOR_TEST_ORIGIN_HOST (a non-loopback address of this machine) to test the winhttp backend"))
      ((not (or proxy-url auth-proxy-url))
       (skip "Set DEXADOR_TEST_PROXY / DEXADOR_TEST_AUTH_PROXY to test the winhttp backend"))
      (t
       (let ((dex:*dexador-backend* :winhttp)
             ;; Make the direct case deterministic even if the machine has a system proxy.
             (dex:*use-system-proxy* nil))
         (testing-app-all-interfaces ("winhttp explicit proxy") *echo-via-app*
           (testing "direct request carries no Via header"
             (multiple-value-bind (code path via) (get-via (origin "/direct"))
               (ok (eql code 200))
               (ok (equal path "/direct"))
               (ok (zerop (length via)))))
           (when proxy-url
             (testing "plain forward proxy adds Via to the forwarded request"
               (multiple-value-bind (code path via)
                   (get-via (origin "/via-proxy") :proxy proxy-url)
                 (ok (eql code 200))
                 (ok (equal path "/via-proxy"))
                 (ok (plusp (length via)))))
             (testing "*no-proxy* bypasses the proxy"
               (let ((dex:*no-proxy* (origin-host)))
                 (multiple-value-bind (code path via)
                     (get-via (origin "/bypass") :proxy proxy-url)
                   (ok (eql code 200))
                   (ok (equal path "/bypass"))
                   (ok (zerop (length via)))))))
           (when auth-proxy-url
             (testing "auth proxy without credentials -> 407"
               (handler-case
                   (progn (dex:get (origin "/secret") :proxy auth-proxy-url)
                          (fail "Expected HTTP-REQUEST-PROXY-AUTHENTICATION-REQUIRED"))
                 (dex:http-request-proxy-authentication-required (e)
                   (ok (eql (dex:response-status e) 407)))))
             (testing "auth proxy with credentials"
               (multiple-value-bind (code path via)
                   (get-via (origin "/secret")
                            :proxy (url-with-userinfo auth-proxy-url "dexador:test"))
                 (ok (eql code 200))
                 (ok (equal path "/secret"))
                 (ok (plusp (length via))))))
           (testing "socks5 proxy is rejected"
             (ok (signals (dex:get (origin "/socks") :proxy "socks5://127.0.0.1:1080")
                          'error)))))))))

(deftest winhttp-https-proxy
  (let ((proxy-url (env-url "DEXADOR_TEST_PROXY"))
        (auth-proxy-url (env-url "DEXADOR_TEST_AUTH_PROXY")))
    (cond
      ((not (env-url "DEXADOR_TEST_PROXY_CA"))
       ;; WinHTTP validates against the Windows certificate store; the CA env
       ;; var doubles as the signal that the proxy CA has been imported there.
       (skip "Set DEXADOR_TEST_PROXY_CA (and certutil -addstore Root it) to run winhttp HTTPS proxy tests"))
      ((not (origin-host))
       (skip "Set DEXADOR_TEST_ORIGIN_HOST (a non-loopback address of this machine) to test the winhttp backend"))
      ((not (or proxy-url auth-proxy-url))
       (skip "Set DEXADOR_TEST_PROXY / DEXADOR_TEST_AUTH_PROXY to run HTTPS proxy tests"))
      (t
       (let ((dex:*dexador-backend* :winhttp)
             (dex:*use-system-proxy* nil))
         (testing-app-all-interfaces ("winhttp https through proxy") *echo-via-app*
           ;; The MITM certificate carries the origin IP as a SAN; WinHTTP
           ;; verifies IP SANs against the request host.
           (flet ((https-url (path)
                    (format nil "https://~A:~D~A" (origin-host) *clack-test-port* path)))
             (when proxy-url
               (testing "CONNECT tunnel with verified certificate"
                 (multiple-value-bind (code path via)
                     (get-via (https-url "/https-proxy") :proxy proxy-url)
                   (ok (eql code 200))
                   (ok (equal path "/https-proxy"))
                   (ok (plusp (length via))))))
             (when auth-proxy-url
               (testing "authenticated CONNECT (Basic credentials on 407 challenge)"
                 (multiple-value-bind (code path via)
                     (get-via (https-url "/https-auth")
                              :proxy (url-with-userinfo auth-proxy-url "dexador:test"))
                   (ok (eql code 200))
                   (ok (equal path "/https-auth"))
                   (ok (plusp (length via)))))))))))))

;;; System proxy discovery: these tests rewrite the current user's WinINet
;;; proxy settings in the registry (Internet Options), which is what
;;; *use-system-proxy* discovery reads. Settings are saved and restored, but
;;; the tests still only run when DEXADOR_TEST_MUTATE_SYSTEM_PROXY=1 --
;;; meant for disposable CI runners.

(defparameter +inet-settings-key+
  "HKCU\\Software\\Microsoft\\Windows\\CurrentVersion\\Internet Settings")

(defun reg-set (name type value)
  (uiop:run-program (list "reg" "add" +inet-settings-key+
                          "/v" name "/t" type "/d" (princ-to-string value) "/f")
                    :output nil :error-output nil))

(defun reg-delete (name)
  (uiop:run-program (list "reg" "delete" +inet-settings-key+ "/v" name "/f")
                    :output nil :error-output nil :ignore-error-status t))

(defun reg-query (name)
  "Return (VALUES value type) for NAME under the Internet Settings key, or NIL."
  (let* ((out (uiop:run-program (list "reg" "query" +inet-settings-key+ "/v" name)
                                :output :string :error-output nil :ignore-error-status t))
         ;; "    ProxyServer    REG_SZ    127.0.0.1:3128"
         (line (find-if (lambda (l) (search name l)) (uiop:split-string out :separator '(#\Newline))))
         (parts (and line (remove "" (uiop:split-string (string-trim '(#\Return) line))
                                  :test #'string=))))
    (when (and parts (<= 3 (length parts)))
      (values (format nil "~{~A~^ ~}" (cddr parts)) (second parts)))))

(defun call-with-saved-proxy-registry (fn)
  (let ((saved (loop for name in '("ProxyEnable" "ProxyServer" "AutoConfigURL")
                     collect (multiple-value-bind (value type) (reg-query name)
                               (list name value type)))))
    (unwind-protect (funcall fn)
      (loop for (name value type) in saved
            do (if value
                   (reg-set name type value)
                   (reg-delete name))))))

(defun make-pac-app (proxy-hostport)
  "Like *ECHO-VIA-APP* but also serves a PAC script routing everything through PROXY-HOSTPORT."
  (lambda (env)
    (if (string= (getf env :path-info) "/proxy.pac")
        (let ((pac (format nil "function FindProxyForURL(url, host) { return \"PROXY ~A\"; }"
                           proxy-hostport)))
          `(200 (:content-type "application/x-ns-proxy-autoconfig"
                 :content-length ,(length pac))
                (,pac)))
        (funcall *echo-via-app* env))))

(deftest winhttp-system-proxy-discovery
  (let ((proxy-url (env-url "DEXADOR_TEST_PROXY")))
    (cond
      ((not (equal (uiop:getenv "DEXADOR_TEST_MUTATE_SYSTEM_PROXY") "1"))
       (skip "Set DEXADOR_TEST_MUTATE_SYSTEM_PROXY=1 to run tests that rewrite the user's registry proxy settings"))
      ((not (and (origin-host) proxy-url))
       (skip "Also requires DEXADOR_TEST_ORIGIN_HOST and DEXADOR_TEST_PROXY"))
      (t
       (let* ((dex:*dexador-backend* :winhttp)
              (proxy-uri (quri:uri proxy-url))
              (proxy-hostport (format nil "~A:~D"
                                      (quri:uri-host proxy-uri) (quri:uri-port proxy-uri))))
         (call-with-saved-proxy-registry
          (lambda ()
            (testing-app-all-interfaces ("winhttp system proxy discovery") (make-pac-app proxy-hostport)
              (testing "static registry proxy (Internet Options) is discovered"
                (reg-set "ProxyEnable" "REG_DWORD" 1)
                (reg-set "ProxyServer" "REG_SZ" proxy-hostport)
                (reg-delete "AutoConfigURL")
                (multiple-value-bind (code path via) (get-via (origin "/registry"))
                  (ok (eql code 200))
                  (ok (equal path "/registry"))
                  (ok (plusp (length via)))))
              (testing "*use-system-proxy* nil connects directly"
                (let ((dex:*use-system-proxy* nil))
                  (multiple-value-bind (code path via) (get-via (origin "/registry-off"))
                    (ok (eql code 200))
                    (ok (equal path "/registry-off"))
                    (ok (zerop (length via))))))
              (testing "*no-proxy* wins over system settings"
                (let ((dex:*no-proxy* (origin-host)))
                  (multiple-value-bind (code path via) (get-via (origin "/registry-bypass"))
                    (ok (eql code 200))
                    (ok (equal path "/registry-bypass"))
                    (ok (zerop (length via))))))
              (testing "PAC script (AutoConfigURL) is evaluated"
                (reg-set "ProxyEnable" "REG_DWORD" 0)
                (reg-delete "ProxyServer")
                (reg-set "AutoConfigURL" "REG_SZ" (origin "/proxy.pac"))
                (multiple-value-bind (code path via) (get-via (origin "/pac"))
                  (ok (eql code 200))
                  (ok (equal path "/pac"))
                  (ok (plusp (length via)))))))))))))

) ; #+windows progn
