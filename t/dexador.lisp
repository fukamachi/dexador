(in-package :cl-user)
(defpackage dexador-test
  (:use :cl
        :rove)
  (:import-from :clack.test
                :*clack-test-port*
                :*clack-test-access-port*
                :port-available-p
                :localhost))
(in-package :dexador-test)

(defun random-port ()
  "Return a port number not in use from 50000 to 60000."
  ;; IGNORE-ERRORS: binding can fail with errors clack.test doesn't handle,
  ;; e.g. WSAEACCES when the port is in a Windows excluded port range (Hyper-V).
  (loop for port from (+ 50000 (random 1000)) upto 60000
        if (ignore-errors (port-available-p port))
          return port))

(defmacro testing-app ((desc &key use-connection-pool) app &body body)
  ;; Disable clack.test's own port picking: its PORT-AVAILABLE-P chokes on
  ;; Windows excluded port ranges, so choose the port with ours instead.
  `(let* ((clack.test:*random-port* nil)
          (*clack-test-port* (random-port))
          (clack.test:*clack-test-access-port* *clack-test-port*))
     (clack.test:testing-app ,desc ,app
       ;; Clack's TESTING-APP sets dex:*use-connection-pool* to NIL,
       ;; but we need to change it in some tests
       (let ((dex:*use-connection-pool* ,use-connection-pool))
         (dex:clear-connection-pool)
         ,@body))))

(defmacro with-proxy-capable-backend (() &body body)
  "Run BODY with a proxy-testable backend. These tests proxy to loopback
origins, which WinHTTP always bypasses, so use usocket on Windows.
The winhttp backend is covered by dexador-integration-test instead."
  #-windows `(progn ,@body)
  #+windows `(let ((dex:*dexador-backend* :usocket))
               ,@body))

(defun proxied-by (headers)
  "Which test proxy served the response, or NIL when it was direct."
  (gethash dexador-test.proxy:+proxied-by-header+ headers))

(deftest normal-case-tests
  (testing-app ("normal case")
      (lambda (env)
        `(200 (:content-length ,(length (getf env :request-uri))) (,(getf env :request-uri))))
    (testing "GET"
      (multiple-value-bind (body code headers)
          (dex:get (localhost "/foo")
                   :headers '((:x-foo . "ppp")))
        (ok (eql code 200))
        (ok (equal body "/foo"))
        (ok (equal (gethash "content-length" headers) "4"))))
    (testing "HEAD"
      (multiple-value-bind (body code)
          (dex:head (localhost "/foo"))
        (ok (eql code 200))
        (ok (equal body ""))))
    (testing "PUT"
      (multiple-value-bind (body code)
          (dex:put (localhost "/foo"))
        (ok (eql code 200))
        (ok (equal body "/foo"))))
    (testing "DELETE"
      (multiple-value-bind (body code)
          (dex:delete (localhost "/foo"))
        (ok (eql code 200))
        (ok (equal body "/foo"))))))

(deftest proxy-http-tests
  (with-proxy-capable-backend ()
    (testing-app ("proxy (http) case")
        (lambda (env)
          (let ((body (format nil "~A~%~A"
                              (gethash "host" (getf env :headers))
                              (getf env :request-uri))))
            `(200 (:content-length ,(length body)) (,body))))
      (dexador-test.proxy:with-test-proxy (proxy)
        (let ((proxy-url (dexador-test.proxy:test-proxy-url proxy))
              (expected (format nil "~A~%/foo"
                                (quri:uri-authority (quri:uri (localhost))))))
          (testing "GET"
            (multiple-value-bind (body code headers)
                (dex:get (localhost "/foo")
                         :headers '((:x-foo . "ppp"))
                         :proxy proxy-url)
              (ok (eql code 200))
              (ok (equal body expected))
              (ok (equal (proxied-by headers) "cl-test-proxy"))))
          (testing "HEAD"
            (multiple-value-bind (body code headers)
                (dex:head (localhost "/foo") :proxy proxy-url)
              (ok (eql code 200))
              (ok (equal body ""))
              (ok (equal (proxied-by headers) "cl-test-proxy"))))
          (testing "PUT"
            (multiple-value-bind (body code headers)
                (dex:put (localhost "/foo") :proxy proxy-url)
              (ok (eql code 200))
              (ok (equal body expected))
              (ok (equal (proxied-by headers) "cl-test-proxy"))))
          (testing "DELETE"
            (multiple-value-bind (body code headers)
                (dex:delete (localhost "/foo") :proxy proxy-url)
              (ok (eql code 200))
              (ok (equal body expected))
              (ok (equal (proxied-by headers) "cl-test-proxy"))))
          (testing "the proxy received absolute-form request targets"
            (ok (every (lambda (line)
                         (search " http://" line))
                       (dexador-test.proxy:test-proxy-requests proxy)))))))))

(deftest proxy-auth-tests
  (with-proxy-capable-backend ()
    (testing-app ("proxy (Basic auth) case")
        (lambda (env)
          (declare (ignore env))
          '(200 (:content-length 2) ("OK")))
      (dexador-test.proxy:with-test-proxy (proxy :basic-auth '("proxyuser" . "proxypass"))
        (testing "no credentials -> 407"
          (handler-case
              (progn (dex:get (localhost "/") :proxy (dexador-test.proxy:test-proxy-url proxy))
                     (fail "Expected HTTP-REQUEST-PROXY-AUTHENTICATION-REQUIRED"))
            (dex:http-request-proxy-authentication-required (e)
              (ok (eql (dex:response-status e) 407)))))
        (testing "wrong credentials -> 407"
          (handler-case
              (progn (dex:get (localhost "/")
                              :proxy (dexador-test.proxy:test-proxy-url proxy :userinfo "proxyuser:wrong"))
                     (fail "Expected HTTP-REQUEST-PROXY-AUTHENTICATION-REQUIRED"))
            (dex:http-request-proxy-authentication-required (e)
              (ok (eql (dex:response-status e) 407)))))
        (testing "user:pass@host proxy URL sends Proxy-Authorization"
          (multiple-value-bind (body code headers)
              (dex:get (localhost "/")
                       :proxy (dexador-test.proxy:test-proxy-url proxy :userinfo "proxyuser:proxypass"))
            (ok (eql code 200))
            (ok (equal body "OK"))
            (ok (equal (proxied-by headers) "cl-test-proxy"))))))))

(deftest proxy-bypass-tests
  (with-proxy-capable-backend ()
    (testing-app ("proxy bypass (*no-proxy*) case")
        (lambda (env)
          (declare (ignore env))
          '(200 (:content-length 2) ("OK")))
      (dexador-test.proxy:with-test-proxy (proxy)
        (let ((dex:*default-proxy* (dexador-test.proxy:test-proxy-url proxy)))
          (testing "*default-proxy* routes through the proxy"
            (multiple-value-bind (body code headers) (dex:get (localhost "/"))
              (declare (ignore body))
              (ok (eql code 200))
              (ok (equal (proxied-by headers) "cl-test-proxy"))))
          (testing "*no-proxy* pattern bypasses the proxy"
            (let ((dex:*no-proxy* "127.0.0.0/8,localhost"))
              (multiple-value-bind (body code headers) (dex:get (localhost "/"))
                (declare (ignore body))
                (ok (eql code 200))
                (ok (null (proxied-by headers)))))))))))

(deftest winhttp-ntlm-auth-tests
  #-windows
  (skip "NTLM authentication tests target the winhttp backend (Windows only)")
  #+windows
  (let ((dex:*dexador-backend* :winhttp))
    (flet ((url (server)
             (format nil "http://127.0.0.1:~D/" (dexador-test.ntlm:ntlm-server-port server))))
      (testing "the strongest authentication scheme is selected"
        (ok (eq (dexador.backend.winhttp::select-auth-scheme-from-mask
                 (logior dexador.backend.winhttp::+WINHTTP_AUTH_SCHEME_BASIC+
                         dexador.backend.winhttp::+WINHTTP_AUTH_SCHEME_NTLM+))
                :ntlm)))
      (testing "autologon defaults to the intranet zone"
        (ok (= (dexador.backend.winhttp::autologon-policy-value)
               dexador.backend.winhttp::+WINHTTP_AUTOLOGON_SECURITY_LEVEL_MEDIUM+)))
      (testing "explicit NTLM credentials complete the challenge/resend handshake"
        (dexador-test.ntlm:with-ntlm-server (server)
          (multiple-value-bind (body code)
              (dex:get (url server) :basic-auth (cons "user" "pass") :force-string t)
            (ok (eql code 200))
            (ok (equal body "SSO-OK"))
            ;; Full handshake over one kept-alive connection: Type 1 then Type 3.
            (ok (equal (dexador-test.ntlm:ntlm-server-message-types server) '(1 3))))))
      (testing "single sign-on engages NTLM with the logged-on credentials"
        (dexador-test.ntlm:with-ntlm-server (server)
          ;; :LOW is required here because the stub listens on an IP address;
          ;; the default :MEDIUM policy only releases credentials to intranet
          ;; zone *names*. Production code should keep :MEDIUM.
          (let ((dex:*winhttp-autologon-policy* :low))
            ;; The loopback logon token can't finish NTLM against a 127.0.0.1 fake,
            ;; but the client must at least offer NTLM (a Type 1) using default
            ;; credentials -- that is the single-sign-on path being exercised.
            (ignore-errors (dex:get (url server)))
            (ok (member 1 (dexador-test.ntlm:ntlm-server-message-types server))))))
      (testing "*use-default-credentials* nil leaves the challenge unanswered"
        (dexador-test.ntlm:with-ntlm-server (server)
          (let ((dex:*use-default-credentials* nil))
            (handler-case
                (progn (dex:get (url server))
                       (fail "Expected HTTP-REQUEST-UNAUTHORIZED"))
              (dex:http-request-unauthorized (e)
                (ok (eql (dex:response-status e) 401)))))
          ;; The handshake never completes: the server never sees a Type 3.
          ;; (WinHTTP still auto-emits a Type 1 for loopback/IP hosts, where the
          ;; autologon HIGH policy is documented not to take effect, so we can't
          ;; assert the server saw nothing at all.)
          (ok (not (member 3 (dexador-test.ntlm:ntlm-server-message-types server)))))))))

(deftest proxy-resolution-tests
  (testing "single environment proxy remains a fallback for both schemes"
    (let ((http-only (dexador.util::make-environment-proxy nil "http://p-http:8080" nil))
          (https-only (dexador.util::make-environment-proxy "http://p-https:8080" nil nil))
          (http-and-all (dexador.util::make-environment-proxy
                         nil "http://p-http:8080" "http://p-all:1080")))
      (ok (equal (dexador.util:resolve-proxy "https://example.com/" http-only)
                 "http://p-http:8080"))
      (ok (equal (dexador.util:resolve-proxy "http://example.com/" https-only)
                 "http://p-https:8080"))
      (ok (equal (dexador.util:resolve-proxy "http://example.com/" http-and-all)
                 "http://p-http:8080"))
      (ok (equal (dexador.util:resolve-proxy "https://example.com/" http-and-all)
                 "http://p-all:1080"))))
  (testing "per-scheme proxy alist"
    (let ((cfg '(("https" . "http://p-https:8080")
                 ("http"  . "http://p-http:8080")
                 ("https://special.example.com" . "http://p-host:9090")
                 ("*"     . "http://p-all:1080"))))
      (ok (equal (dexador.util:resolve-proxy "https://api.example.com/x" cfg) "http://p-https:8080"))
      (ok (equal (dexador.util:resolve-proxy "http://api.example.com/x" cfg) "http://p-http:8080"))
      (ok (equal (dexador.util:resolve-proxy "https://special.example.com/y" cfg) "http://p-host:9090")
          "per-host key wins over per-scheme")
      (ok (equal (dexador.util:resolve-proxy "ftp://files.example.com/z" cfg) "http://p-all:1080")
          "falls back to \"*\"")))
  (testing "IPv6 per-host proxy keys (bare and bracketed)"
    (let ((bare '(("https://::1" . "http://p-bare:1")
                  ("https" . "http://p-https:1")))
          (bracketed '(("https://[::1]" . "http://p-bracket:1")
                       ("https" . "http://p-https:1"))))
      (ok (equal (dexador.util:resolve-proxy "https://[::1]/" bare) "http://p-bare:1")
          "bare scheme://host key matches bracketed URI host")
      (ok (equal (dexador.util:resolve-proxy "https://[::1]/" bracketed) "http://p-bracket:1")
          "bracketed scheme://host key matches")))
  (testing "format-host-port brackets IPv6"
    (ok (equal (dexador.util:format-host-port "example.com" 8080) "example.com:8080"))
    (ok (equal (dexador.util:format-host-port "::1" 8080) "[::1]:8080"))
    (ok (equal (dexador.util:format-host-port "[::1]" 8080) "[::1]:8080")))
  (testing "absolute-form request target omits userinfo"
    (let ((line (babel:octets-to-string
                 (fast-io:with-fast-output (buffer)
                   (dexador.util:write-first-line
                    :get (quri:uri "http://user:pass@example.com/path?q=1#fragment")
                    1.1 buffer t)))))
      (ok (equal line (format nil "GET http://example.com/path?q=1 HTTP/1.1~C~C"
                              #\Return #\Newline)))))
  (testing "string proxy applies to every scheme (backward compatible)"
    (ok (equal (dexador.util:resolve-proxy "https://x.com/" "http://oneproxy:3128") "http://oneproxy:3128"))
    (ok (null (dexador.util:resolve-proxy "https://x.com/" nil)))
    (ok (null (dexador.util:resolve-proxy "https://x.com/" '(("http" . "http://p:1"))))
        "unmatched scheme yields NIL, not an error"))
  (testing "NO_PROXY host matching"
    (ok (dexador.util:host-bypassed-p "example.com" "example.com"))
    (ok (dexador.util:host-bypassed-p "api.example.com" ".example.com"))
    (ok (dexador.util:host-bypassed-p "api.example.com" "example.com"))
    (ng (dexador.util:host-bypassed-p "notexample.com" "example.com") "suffix must align on a dot")
    (ok (dexador.util:host-bypassed-p "anything.net" "*") "\"*\" bypasses all")
    (ok (dexador.util:host-bypassed-p "example.com" "example.com:443") ":port in pattern is ignored")
    (ok (dexador.util:host-bypassed-p "a.foo.org" '("bar.com" ".foo.org")) "list form")
    (ng (dexador.util:host-bypassed-p "a.foo.org" nil)))
  (testing "NO_PROXY IP and CIDR matching"
    (ok (dexador.util:host-bypassed-p "127.0.0.1" "127.0.0.1"))
    (ok (dexador.util:host-bypassed-p "127.0.0.1" "127.0.0.1:8080") ":port on an IP pattern is ignored")
    (ok (dexador.util:host-bypassed-p "10.1.2.3" "10.0.0.0/8"))
    (ok (dexador.util:host-bypassed-p "192.168.1.7" "no-match.com, 192.168.0.0/16"))
    (ng (dexador.util:host-bypassed-p "11.1.2.3" "10.0.0.0/8"))
    (ng (dexador.util:host-bypassed-p "10.1.2.3" "example.com") "IP host never matches a hostname pattern")
    (ng (dexador.util:host-bypassed-p "1.2.3.4" "2.3.4") "IP host requires IP/CIDR match, not a suffix")
    (ok (dexador.util:host-bypassed-p "::1" "::1"))
    (ok (dexador.util:host-bypassed-p "[::1]" "::1") "bracketed URL form of the host")
    (ok (dexador.util:host-bypassed-p "::1" "0:0:0:0:0:0:0:1") "IPv6 compared numerically")
    (ok (dexador.util:host-bypassed-p "fd12:3456::1" "fd00::/8"))
    (ng (dexador.util:host-bypassed-p "2001:db8::1" "fd00::/8"))
    (ng (dexador.util:host-bypassed-p "127.0.0.1" "::1") "no cross-family match"))
  (testing "resolve-proxy honors no-proxy (bypass yields NIL)"
    (let ((dexador.util:*no-proxy* "internal.example.com"))
      (ok (null (dexador.util:resolve-proxy "https://internal.example.com/" "http://p:8080")))
      (ok (equal (dexador.util:resolve-proxy "https://external.com/" "http://p:8080") "http://p:8080")))))

(deftest proxy-socks5-tests
  #+windows
  (skip "SOCKS5 proxy tests are skipped")
  #-windows
  (testing-app ("proxy (socks5) case")
      (flet ((check (uri in out)
               (flexi-streams:with-input-from-sequence (in in)
                 (equalp
                  (flexi-streams:with-output-to-sequence (out :element-type '(unsigned-byte 8))
                    (dexador.backend.usocket::ensure-socks5-connected in out (quri:uri uri) :get))
                  out))))
        (ok (check "http://example.com/"
               #(5 0
                 5 0 0  1  0 0 0 0  0 0)
               #(5 1 0
                 5 1 0  3 11 101 120 97 109 112 108 101 46 99 111 109  0 80)))
        (ok (check "https://example.com/"
               #(5 0
                 5 0 0  1  0 0 0 0  0 0)
               #(5 1 0
                 5 1 0  3 11 101 120 97 109 112 108 101 46 99 111 109  1 187)))
        (ok (check "http://example.com:8080/"
               #(5 0
                 5 0 0  1  0 0 0 0  0 0)
               #(5 1 0
                 5 1 0  3 11 101 120 97 109 112 108 101 46 99 111 109  31 144)))
        (ok (check "https://example.com:8080/"
               #(5 0
                 5 0 0  1  0 0 0 0  0 0)
               #(5 1 0
                 5 1 0  3 11 101 120 97 109 112 108 101 46 99 111 109  31 144)))
        (ok (check "http://example.com/"
                   #(5 0
                     5 0 0  4  0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  0 0)
                   #(5 1 0
                     5 1 0  3 11 101 120 97 109 112 108 101 46 99 111 109  0 80)))
        (ok (check "http://example.com/"
                   #(5 0
                     5 0 0  3  1 0  0 0)
                   #(5 1 0
                     5 1 0  3 11 101 120 97 109 112 108 101 46 99 111 109  0 80)))
        (handler-case
            (check "http://example.com/"
                   #(4)
                   #())
          (dex:socks5-proxy-request-failed ()
            (ok t)))
        (handler-case
            (check "http://example.com/"
                   #(5 255)
                   #())
          (dex:socks5-proxy-request-failed ()
            (ok t))))

      #+needs-Tor-running-on-localhost
      (let ((proxy "socks5://127.0.0.1:9150"))
        (testing "SOCKS5 GET"
          (multiple-value-bind (body code)
              (dex:get "http://duskgytldkxiuqc6.onion/" :proxy proxy)
            (declare (ignore body))
            (ok (eql code 200))))
        (testing "SOCKS5 GET with SSL"
          (multiple-value-bind (body code)
              (dex:get "https://www.facebookcorewwwi.onion/" :proxy proxy)
            (declare (ignore body))
            (ok (eql code 200)))))))

(deftest redirection-tests
  (testing-app ("redirection")
      (lambda (env)
        (let ((id (parse-integer (subseq (getf env :path-info) 1))))
          (cond
            ((= id 3)
             '(200 (:content-length 2) ("OK")))
            ((<= 300 id 399)
             `(,id (:location "/200") ()))
            ((= id 200)
             (let ((method (princ-to-string (getf env :request-method))))
               `(200 (:content-length ,(length method))
                     (,method))))
            (t
             `(302 (:location ,(format nil "/~D" (1+ id))) ())))))
    (testing "redirect"
      (multiple-value-bind (body code headers)
          (dex:get (localhost "/1"))
        (ok (eql code 200))
        (ok (equal body "OK"))
        (ok (equal (gethash "content-length" headers)
                   (princ-to-string 2)))))
    (testing "not enough redirect"
      (multiple-value-bind (body code headers)
          (dex:get (localhost "/1") :max-redirects 0)
        (declare (ignore body))
        (ok (eql code 302))
        (ok (equal (gethash "location" headers) "/2"))))
    (testing "exceed max redirect"
      (multiple-value-bind (body code headers)
          (dex:get (localhost "/4") :max-redirects 7)
        (declare (ignore body))
        (ok (eql code 302))
        (ok (equal (gethash "location" headers) "/12"))))
    (testing "POST redirects as GET"
      (multiple-value-bind (body code headers uri)
          (dex:post (localhost "/301"))
        (declare (ignore headers))
        (ok (eql code 200))
        (ok (equal body "GET"))
        (ok (equal (quri:uri-path uri) "/200"))))
    (testing "POST redirects as POST for 307"
      (multiple-value-bind (body code headers uri)
          (dex:post (localhost "/307"))
        (declare (ignore headers))
        (ok (eql code 200))
        (ok (equal body "POST"))
        (ok (equal (quri:uri-path uri) "/200"))))))

(deftest cross-host-redirect-credential-stripping-tests
  #+windows
  (skip "Cross-host redirect credential tests use usocket backend only")
  #-windows
  ;; Outer server: target that echoes back the Authorization header (or empty string)
  (testing-app ("cross-host redirect target")
      (lambda (env)
        (let ((auth (or (gethash "authorization" (getf env :headers)) "")))
          `(200 (:content-length ,(length auth)) (,auth))))
    (let ((target-port *clack-test-port*))
      ;; Inner server: redirects all requests to the target server (different port = different host)
      (testing-app ("cross-host redirect origin")
          (lambda (env)
            (declare (ignore env))
            `(302 (:location ,(format nil "http://127.0.0.1:~A/" target-port)) ()))
        (let ((dexador.util:*default-proxy* nil))
          (testing "basic-auth is stripped on cross-host redirect"
            (multiple-value-bind (body code)
                (dex:get (localhost "/") :basic-auth '("user" . "pass"))
              (ok (eql code 200))
              (ok (equal body "") "Authorization header must not reach the target")))
          (testing "bearer-auth is stripped on cross-host redirect"
            (multiple-value-bind (body code)
                (dex:get (localhost "/") :bearer-auth "my-secret-token")
              (ok (eql code 200))
              (ok (equal body "") "Authorization header must not reach the target")))
          (testing "keyword :authorization header is stripped on cross-host redirect"
            (multiple-value-bind (body code)
                (dex:get (localhost "/") :headers '((:authorization . "Bearer explicit")))
              (ok (eql code 200))
              (ok (equal body "") "Authorization header must not reach the target")))
          (testing "string Authorization header is stripped on cross-host redirect"
            (multiple-value-bind (body code)
                (dex:get (localhost "/") :headers '(("Authorization" . "Bearer string-key")))
              (ok (eql code 200))
              (ok (equal body "") "Authorization header must not reach the target"))))))))

(deftest same-host-redirect-credential-preservation-tests
  #+windows
  (skip "Same-host redirect credential preservation tests use usocket backend only")
  #-windows
  ;; Single server: /redirect-post returns 301 to /echo-auth; /echo-auth echoes Authorization header
  (testing-app ("same-host redirect credential preservation")
      (lambda (env)
        (cond
          ((string= (getf env :path-info) "/redirect-post")
           '(301 (:location "/echo-auth") ()))
          ((string= (getf env :path-info) "/echo-auth")
           (let ((auth (or (gethash "authorization" (getf env :headers)) "")))
             `(200 (:content-length ,(length auth)) (,auth))))
          (t '(404 () ()))))
    (let ((dexador.util:*default-proxy* nil))
      (testing "basic-auth preserved on same-host POST 301 redirect (method-change case)"
        (multiple-value-bind (body code)
            (dex:post (localhost "/redirect-post") :basic-auth '("user" . "pass"))
          (ok (eql code 200))
          (ok (not (equal body "")) "Authorization header must be preserved on same-host redirect"))))))

(deftest content-disposition-tests
  #+windows
  (skip "Content-Disposition tests are skipped")
  #-windows
  (testing "content-disposition"
    (ok (equal (dexador.backend.usocket::content-disposition "upload" #P"data/plain-file.txt")
               (format nil "Content-Disposition: form-data; name=\"upload\"; filename=\"plain-file.txt\"~C~C"
                       #\Return #\Newline))
        "ASCII file name")
    (ok (equal (dexador.backend.usocket::content-disposition "upload" #P"data/plain file.txt")
               (format nil "Content-Disposition: form-data; name=\"upload\"; filename=\"plain file.txt\"~C~C"
                       #\Return #\Newline))
        "ASCII file name with space")
    #+ecl
    (skip "Skipped because UTF-8 pathname is not allowed on ECL")
    #-ecl
    (ok (equal (dexador.backend.usocket::content-disposition "upload" #P"data/foo-あいうえお.txt")
               (format nil "Content-Disposition: form-data; name=\"upload\"; filename*=UTF-8''foo-%E3%81%82%E3%81%84%E3%81%86%E3%81%88%E3%81%8A.txt~C~C"
                       #\Return #\Newline))
        "UTF-8 file name")
    (ok (equal (dexador.backend.usocket::content-disposition "title" "ignore")
               (format nil "Content-Disposition: form-data; name=\"title\"~C~C"
                       #\Return #\Newline))
        "string value")))

;; SBCL replaces LF with CRLF when reading from a stream on Windows
(defun replace-crlf-to-lf (string)
  (ppcre:regex-replace-all (format nil "~C~C" #\Return #\Newline)
                           string
                           (format nil "~C" #\Newline)))

(deftest post-request-tests
  (testing-app ("POST request")
      (lambda (env)
        (cond
          ((string= (getf env :path-info) "/upload")
           (let ((buf (make-array (getf env :content-length)
                                  :element-type '(unsigned-byte 8))))
             (read-sequence buf (getf env :raw-body))
             `(200 ()
                   (,(replace-crlf-to-lf (babel:octets-to-string buf))))))
          (t
           (let ((req (lack.request:make-request env)))
             `(200 ()
                   (,(with-output-to-string (s)
                       (loop for (k . v) in (lack.request:request-body-parameters req)
                             do (format s "~&~A: ~A~%"
                                        k
                                        (cond
                                          ((and (consp v)
                                                (streamp (car v)))
                                           (let* ((buf (make-array 1024 :element-type '(unsigned-byte 8)))
                                                  (n (read-sequence buf (car v))))
                                             (replace-crlf-to-lf (babel:octets-to-string (subseq buf 0 n)))))
                                          ((consp v)
                                           (car v))
                                          (t v)))))))))))
    (testing "content in alist"
      (multiple-value-bind (body code headers)
          (dex:post (localhost)
                    :content '(("name" . "Eitaro")
                               ("email" . "e.arrows@gmail.com")))
        (declare (ignore headers))
        (ok (eql code 200))
        (ok (equal body (format nil "name: Eitaro~%email: e.arrows@gmail.com~%")))))
    (testing "string content"
      (multiple-value-bind (body code headers)
          (dex:post (localhost "/upload")
                    :content "this is string data")
        (declare (ignore headers))
        (ok (eql code 200))
        (ok (equal body "this is string data"))))
    (testing "octets content"
      (multiple-value-bind (body code headers)
          (dex:post (localhost "/upload")
                    :content (babel:string-to-octets "this is octet data"))
        (declare (ignore headers))
        (ok (eql code 200))
        (ok (equal body "this is octet data"))))
    (testing "multipart"
      (multiple-value-bind (body code)
          (dex:post (localhost)
                    :content `(("title" . "Road to Lisp")
                               ("body" . ,(asdf:system-relative-pathname :dexador #P"t/data/quote.txt"))))
        (ok (eql code 200))
        (ok (equal body
                   (format nil "title: Road to Lisp~%body: \"Within a couple weeks of learning Lisp I found programming in any other language unbearably constraining.\" -- Paul Graham, Road to Lisp~2%")))))
    (testing "multipart-boundary-added"
      (multiple-value-bind (body code)
          (dex:post (localhost)
                    :content `(("title" . "Road to Lisp")
			       ("body" . ,(asdf:system-relative-pathname :dexador #P"t/data/quote.txt"))))
        (ok (eql code 200))
        (ok (equal body
                   (format nil "title: Road to Lisp~%body: \"Within a couple weeks of learning Lisp I found programming in any other language unbearably constraining.\" -- Paul Graham, Road to Lisp~2%")))))
    (testing "multipart-boundary-test-added-even-if-content-type-specified"
      (multiple-value-bind (body code)
          (dex:post (localhost)
		    :headers '((:content-type . "multipart/form-data"))
                    :content `(("title" . "Road to Lisp")
			       ("body" . ,(asdf:system-relative-pathname :dexador #P"t/data/quote.txt"))))
        (ok (eql code 200))
        (ok (equal body
                   (format nil "title: Road to Lisp~%body: \"Within a couple weeks of learning Lisp I found programming in any other language unbearably constraining.\" -- Paul Graham, Road to Lisp~2%")))))
    (testing "upload"
      (multiple-value-bind (body code)
          (dex:post (localhost "/upload")
                    :content (asdf:system-relative-pathname :dexador #P"t/data/quote.txt"))
        (ok (eql code 200))
        (ok (equal body
                   (format nil "\"Within a couple weeks of learning Lisp I found programming in any other language unbearably constraining.\" -- Paul Graham, Road to Lisp~%")))))))

(deftest http-request-failed-tests
  (testing-app ("HTTP request failed")
      (lambda (env)
        (if (string= (getf env :path-info) "/404")
            '(404 (:x-foo 0) ("Not Found"))
            '(500 (:x-bar 1) ("Internal Server Error"))))
    (handler-case
        (progn
          (dex:get (localhost))
          (fail "Must raise an error DEX:HTTP-REQUEST-FAILED"))
      (dex:http-request-failed (e)
        (pass "Raise DEX:HTTP-REQUEST-FAILED error")
        (ok (eql (dex:response-status e) 500)
            "response status is 500")
        (ok (equal (dex:response-body e) "Internal Server Error")
            "response body is \"Internal Server Error\"")
        (ok (equal (gethash "x-bar" (dex:response-headers e))
                   "1"))))
    (handler-case
        (progn
          (dex:get (localhost "/404"))
          (fail "Must raise an error DEX:HTTP-REQUEST-NOT-FOUND"))
      (dex:http-request-not-found (e)
        (pass "Raise DEX:HTTP-REQUEST-FAILED error")
        (ok (eql (dex:response-status e) 404)
            "response status is 404")
        (ok (equal (dex:response-body e) "Not Found")
            "response body is \"Not Found\"")
        (ok (equal (gethash "x-foo" (dex:response-headers e))
                   "0"))))))

(deftest using-cookies-tests
  (testing-app ("Using cookies")
      (lambda (env)
        (list (if (string= (getf env :path-info) "/302")
                  302
                  200)
              ;; mixi.jp
              '(:set-cookie "_auid=a8acafbaef245a806f6a308506dc95c8; domain=localhost; path=/; expires=Mon, 10-Jul-2017 12:32:47 GMT"
                ;; sourceforge
                :set-cookie2 "VISITOR=55a11217d3179d198af1d003; expires=\"Tue, 08-Jul-2025 12:54:47 GMT\"; httponly; Max-Age=315360000; Path=/")
              '("ok")))
    (let ((cookie-jar (cl-cookie:make-cookie-jar)))
      (ok (eql (length (cl-cookie:cookie-jar-cookies cookie-jar)) 0) "0 cookies")
      (dex:head (localhost) :cookie-jar cookie-jar)
      (ok (eql (length (cl-cookie:cookie-jar-cookies cookie-jar)) 2) "2 cookies")
      (dex:head (localhost) :cookie-jar cookie-jar))

    ;; 302
    (let ((cookie-jar (cl-cookie:make-cookie-jar)))
      (ok (eql (length (cl-cookie:cookie-jar-cookies cookie-jar)) 0) "0 cookies")
      (dex:head (localhost "/302") :cookie-jar cookie-jar)
      (ok (eql (length (cl-cookie:cookie-jar-cookies cookie-jar)) 2) "2 cookies")
      (dex:head (localhost "/302") :cookie-jar cookie-jar))))

(deftest verbose-tests
  (testing-app ("verbose")
      (lambda (env)
        (declare (ignore env))
        '(200 () ("ok")))
    (ok (dex:get (localhost) :verbose t))))

(deftest want-stream-tests
  (testing-app ("want-stream")
      (lambda (env)
        (declare (ignore env))
        '(200 (:content-type "text/plain") ("hi")))
    ;; decoding stream
    (let ((body (dex:get (localhost) :want-stream t :keep-alive nil)))
      #+windows
      (ok (typep body 'stream))
      #-windows
      (ok (typep body 'dexador.decoding-stream:decoding-stream)
          "body is a decoding stream")
      (ok (subtypep (stream-element-type body) 'babel:unicode-char)
          "body is a character stream")
      (let ((buf (make-string 2)))
        (read-sequence buf body)
        (ok (equal buf "hi"))))
    ;; binary stream
    (let ((body (dex:get (localhost) :want-stream t :force-binary t :keep-alive nil)))
      (ok (typep body 'stream) "body is a stream")
      (ok (open-stream-p body) "body is open")
      (ok (subtypep (stream-element-type body) '(unsigned-byte 8))
          "body is a octets stream")
      (let ((buf (make-array 2 :element-type '(unsigned-byte 8))))
        (read-sequence buf body)
        (ok (equal (babel:octets-to-string buf) "hi"))))))

(deftest big-body-with-want-stream-tests
  (testing-app ("big body with want-stream")
      (lambda (env)
        (declare (ignore env))
        `(200 (:content-type "application/json; charset=utf-8"
               :content-length 748)
              ("[{\"name\":\"allow-statement-in-has-a\",\"commit\":{\"sha\":\"d58b3c96503786c64eb2dba22980ebb14010bdbf\",\"url\":\"https://api.github.com/repos/fukamachi/datafly/commits/d58b3c96503786c64eb2dba22980ebb14010bdbf\"}},{\"name\":\"fix-has-a\",\"commit\":{\"sha\":\"4bcea61e84402317ab49605918972983a1511e6a\",\"url\":\"https://api.github.com/repos/fukamachi/datafly/commits/4bcea61e84402317ab49605918972983a1511e6a\"}},{\"name\":\"jojo\",\"commit\":{\"sha\":\"d2b753e7fdd0dbeada9721380cf410186a85535b\",\"url\":\"https://api.github.com/repos/fukamachi/datafly/commits/d2b753e7fdd0dbeada9721380cf410186a85535b\"}},{\"name\":\"master\",\"commit\":{\"sha\":\"d2b753e7fdd0dbeada9721380cf410186a85535b\",\"url\":\"https://api.github.com/repos/fukamachi/datafly/commits/d2b753e7fdd0dbeada9721380cf410186a85535b\"}}]")))
    ;; decoding stream
    (let ((body (dex:get (localhost) :want-stream t)))
      #+windows
      (ok (typep body 'stream))
      #-windows
      (ok (typep body 'dexador.decoding-stream:decoding-stream)
          "body is a decoding stream")
      (ok (subtypep (stream-element-type body) 'babel:unicode-char)
          "body is a character stream")
      (let ((buf (make-string 1024)))
        (ok (eql (read-sequence buf body) 748))))))

(deftest redirection-for-want-stream-tests
  (testing-app ("redirection for want-stream")
      (lambda (env)
        (if (string= (getf env :path-info) "/index.html")
            '(200 () ("ok"))
            '(307 (:location "/index.html"
                   :transfer-encoding "chunked") (""))))
    (let ((body (dex:get (localhost) :want-stream t)))
      (ok body))))

(deftest no-body-tests
  (testing-app ("no body")
    (lambda (env)
      (let ((path (getf env :path-info)))
        (if (string= path "/204")
            '(204 () ())
            '(200 () ()))))
  ;; no Content-Length and no Transfer-Encoding
  (multiple-value-bind (body status headers)
      (dex:get (localhost))
    (ok (equal body ""))
    (ok (eql status 200))
    (ok (null (gethash "content-length" headers)))
    (ok (null (gethash "transfer-encoding" headers))))
  ;; 204 No Content
  (multiple-value-bind (body status headers)
      (dex:get (localhost "/204"))
    (ok (eql status 204))
    (ok (equal body ""))
    (ok (null (gethash "content-length" headers)))
    (ok (null (gethash "transfer-encoding" headers))))))

(defvar *json* "{\"name\":\"Eitaro Fukamachi\",\"name_ja\":\"深町英太郎\",\"login\":true}")
(deftest json-tests
  (testing-app ("JSON")
      (lambda (env)
        (declare (ignore env))
        `(200 (:content-type "application/json") (,*json*)))
    (multiple-value-bind (body status)
        (dex:get (localhost))
      (ok (equal body *json*)
          "JSON is returned as a string")
      (ok (eql status 200)))
    (let ((babel:*default-character-encoding* :cp932))
      ;; Test if the JSON encoding
      (multiple-value-bind (body status)
          (dex:get (localhost))
        (ok (equal body *json*)
            "The default encoding is UTF-8 though babel:*default-character-encoding* is different")
        (ok (eql status 200))))))

(deftest keep-alive-tests
  (testing-app ("keep-alive")
      (lambda (env)
        (declare (ignore env))
        '(200 () ("hi")))
    (let ((headers (nth-value 2 (dex:get (localhost)))))
      (ok (or (null (gethash "connection" headers))
              (string-equal (gethash "connection" headers) "keep-alive"))))
    (let ((headers (nth-value 2 (dex:get (localhost) :keep-alive nil))))
      (ok (equalp (gethash "connection" headers) "close")))
    (multiple-value-bind (b status response-headers uri opaque-socket-stream)
        (dex:get (localhost) :keep-alive t :use-connection-pool nil)
      (declare (ignorable b status response-headers uri opaque-socket-stream))
      #+windows
      (ok (null opaque-socket-stream) "no socket stream")
      #-windows
      (ok (open-stream-p opaque-socket-stream) "stream is kept alive")
      (ok (= status 200) "success")
      #-windows
      (multiple-value-bind (b2 status2 response-headers2 uri2 opaque-socket-stream2)
          (dex:get (localhost) :keep-alive t :use-connection-pool nil :stream opaque-socket-stream)
        (declare (ignorable b2 response-headers2 uri2))
        (ok (eql opaque-socket-stream opaque-socket-stream2) "stream is re-used")
        (ok (open-stream-p opaque-socket-stream2) "stream is kept alive")
        (ok (close opaque-socket-stream) "stream can be closed")
        (ok (= status2 200) "success")
        (multiple-value-bind (b3 status3 response-headers3 uri3 opaque-socket-stream3)
            (dex:get (localhost) :keep-alive t :use-connection-pool nil :stream opaque-socket-stream2)
          (declare (ignorable b3 uri3))
          (member (gethash "connection" response-headers3) '(nil "keep-alive") :test #'equalp)
          (ok (= status3 200) "success")
          (ok (not (eql opaque-socket-stream3 opaque-socket-stream2)) "passing in closed stream works"))))))

(defun assert-pool-items-count-is (expected-count)
  (let ((count (dexador.connection-cache::lru-pool-num-elts dexador:*connection-pool*)))
    (ok (= count
           expected-count)
        (format nil "Pool items count should be equal to ~A (real value is ~A)"
                expected-count
                count))))

(defun assert-connection-header-is (headers expected-value)
  (let ((value (gethash "connection" headers)))
    (ok (equalp value expected-value)
        (format nil "\"Connection\" header should be equal to ~S (real value is ~S)"
                expected-value
                value))))


(deftest connection-pool-and-errors
  ;; Here we are checking the situation when server returns 400 error and
  ;; in the second case also requests connection interruption.
  ;; Previously, this second case lead to an error when closed connection
  ;; was returned to the pool.
  #+windows
  (skip "Skipped because connection pool is not used on Windows.")
  #-windows
  (let ((success-count 0)
        (error-count 0)
        (error-and-close-count 0))
    (flet ((assert-success-count (expected)
             (ok (= success-count expected)
                 (format nil "Expected success-count to be ~S (real value is ~S)"
                         expected success-count)))
           (assert-error-count (expected)
             (ok (= error-count expected)
                 (format nil "Expected error-count to be ~S (real value is ~S)"
                         expected error-count)))
           (assert-error-and-close-count (expected)
             (ok (= error-and-close-count expected)
                 (format nil "Expected error-and-close-count to be ~S (real value is ~S)"
                         expected error-and-close-count))))
      (testing-app ("connection-pool-and-errors" :use-connection-pool t)
          (lambda (env)
            (let ((path (getf env :path-info)))
              (cond
                ((string= path "/error-and-close")
                 (incf error-and-close-count)
                 '(400
                   (:connection "close")
                   ("")))
                ((string= path "/error")
                 (incf error-count)
                 '(400
                   nil
                   ("")))
                (t
                 (incf success-count)
                 '(200
                   nil
                   (""))))))
        (testing "Initial pool state"
          (assert-pool-items-count-is 0))

        (testing "Successful requests leave one connect in the pool"
          (loop repeat 10
                do (dex:get (localhost "/")))
          (assert-pool-items-count-is 1)
          (assert-success-count 10)
          (assert-error-count 0)
          (assert-error-and-close-count 0))

        (testing "Repetitive success and error requests should not populate pool with connections"
          (loop repeat 10
                do (dex:get (localhost "/"))
                   (ok (rove:signals (dex:get (localhost "/error"))
                           'dex:http-request-bad-request)))
          (assert-success-count 20)
          (assert-error-count 10)
          (assert-error-and-close-count 0)
          ;; Previously, because of the bug, connections count was 8 (maximum)
          (assert-pool-items-count-is 1))

        (testing "If server requests connection close on error, connection should not be returned to the pool."
          (loop repeat 10
                do (dex:get (localhost "/"))
                   ;; Previously, because of the another bug, other error was signaled,
                   ;; because dexador retried and error with a new connection,
                   ;; but closed it before the second attempt, because server response
                   ;; has "Connection: close" header:
                   (ok (rove:signals (dex:get (localhost "/error-and-close"))
                           'dex:http-request-bad-request)))

          (assert-success-count 30)
          (assert-error-count 10)
          (assert-error-and-close-count 10)
          (assert-pool-items-count-is 0))))))


(deftest deflate-compression-tests
  (testing-app ("deflate compression")
      (lambda (env)
        (declare (ignore env))
        `(200 (:content-encoding "deflate" :content-type "text/plain")
              ,(asdf:system-relative-pathname :dexador #p"t/data/test.zlib")))
    (let ((body (dex:get (localhost))))
      (ok (equal body "Deflate test string.")))))

(deftest gzip-compression-tests
  (testing-app ("gzip compression")
      (lambda (env)
        (declare (ignore env))
        `(200 (:content-encoding "gzip" :content-type "text/plain")
              ,(asdf:system-relative-pathname :dexador #p"t/data/test.gz")))
    (let ((body (dex:get (localhost))))
      (ok (equal body "Gzip test string.")))))

(deftest unread-character-tests
  (ok (eql #\u2602
           (with-open-file (stream (asdf:system-relative-pathname
                                     :dexador #p"t/data/umb.bin")
                                   :element-type '(unsigned-byte 8))
             (let ((decoding-stream
                     (dexador.decoding-stream:make-decoding-stream stream)))
               (peek-char nil decoding-stream)
               (read-char decoding-stream))))))

(deftest keep-alive-stream/decoding-stream
    (with-open-file (stream0 (asdf:system-relative-pathname
                              :dexador #p"t/data/bug139.txt")
                             :element-type '(unsigned-byte 8))
      (let* ((len (file-length stream0))
             (stream1 (dexador.keep-alive-stream:make-keep-alive-stream stream0 :end len
                                                                                :chunked-stream nil))
             (stream2 (dexador.decoding-stream:make-decoding-stream stream1)))
        (ok (= (length
                (loop for c = (read-char stream2 nil :eof)
                      until (eq c :eof)
                      collect c)) len)))))

(deftest connection-cache-test
  (let ((dexador.connection-cache:*connection-pool* (dexador.connection-cache:make-connection-pool 2)))
    ;; Make sure empty cache works
    (ok (null (dexador.connection-cache:steal-connection "some-host")))
    (dexador.connection-cache:clear-connection-pool)
    ;; Make sure push / steal works
    (dexador.connection-cache:push-connection "host1" "host1-socket")
    (ok (string= (dexador.connection-cache:steal-connection "host1") "host1-socket"))
    ;; Make sure steal actually removed the connection
    (ok (null (dexador.connection-cache:steal-connection "host1")))
    ;; Check to make sure multiple elements with the same key work
    (dexador.connection-cache:push-connection "host1" "host1-socket1")
    (dexador.connection-cache:push-connection "host1" "host1-socket2")
    (let ((result1 (dexador.connection-cache:steal-connection "host1"))
          (result2 (dexador.connection-cache:steal-connection "host1")))
      (ok (and (stringp result1) (stringp result2) (not (string= result1 result2)))))
    ;; make sure hash table stays clean
    (ok (zerop (hash-table-count (dexador.connection-cache::lru-pool-hash-table dexador.connection-cache::*connection-pool*))))
    ;; make sure maximum connections is obeyed and least recently used element is evicted
    (dexador.connection-cache:push-connection "host1" "host1-socket1")
    (dexador.connection-cache:push-connection "host2" "host2-socket")
    (dexador.connection-cache:push-connection "host2" "host2-socket")
    (ok (null (dexador.connection-cache:steal-connection "host1")))
    (ok (string= (dexador.connection-cache:steal-connection "host2") "host2-socket"))
    (ok (string= (dexador.connection-cache:steal-connection "host2") "host2-socket"))
    (ok (null (dexador.connection-cache:steal-connection "host2")))
    ;; Make sure clear-connection-pool works and callbacks are called
    (let ((called nil))
      (dexador.connection-cache:push-connection "host1" "host1-socket1" (lambda (s) (declare (ignore s)) (setf called t)))
      (dexador.connection-cache:clear-connection-pool)
      (ok called)
      (setf called nil)
      (dexador.connection-cache:push-connection "host1" "host1-socket" (lambda (s) (declare (ignore s))  (setf called "host1")))
      (dexador.connection-cache:push-connection "host2" "host2-socket" (lambda (s) (declare (ignore s)) (setf called "host2")))
      (dexador.connection-cache:push-connection "host3" "host3-socket" (lambda (s) (declare (ignore s)) (setf called "host3")))
      (ok (string= called "host1"))
      (dexador.connection-cache:push-connection "host4" "host4-socket" (lambda (s) (declare (ignore s)) (setf called "host4")))
      (ok (string= called "host2")))))

(deftest keep-alive-stream-close
    "Issue #150, close on keep alive stream does"
    (with-open-file (stream0 (asdf:system-relative-pathname
                              :dexador #p"t/data/bug139.txt")
                             :element-type '(unsigned-byte 8))
      (let* ((len (file-length stream0))
	     (fake-cache nil)
             (stream (dexador.keep-alive-stream:make-keep-alive-stream
		      stream0 :end len :chunked-stream nil
			      :on-close-or-eof (lambda (underlying-stream abort)
						 (declare (ignorable abort))
						 (when underlying-stream
						   (push underlying-stream fake-cache))))))
	(ok (not (null (open-stream-p stream))))
	(ok (null fake-cache))
	(close stream)
	(ok (open-stream-p (car fake-cache)))
	(ok (null (open-stream-p stream))))))

(deftest multipart-content
  "When not using chunked encoding, multipart-content-length should be correct!"
  (let ((test `(("var" "hello")
		("var2" ,(make-array 15 :element-type '(unsigned-byte 8))
			:content-type "application/octet-stream"))))
    (ok (= (length
	    (flexi-streams:with-output-to-sequence (str)
	      (dexador.body:write-multipart-content test "BLARG" str)))
	   (dexador.body:multipart-content-length test "BLARG")))))

(deftest restarts
  (let ((call 0))
    (testing-app ("ok for every 5 times")
        (lambda (env)
          (cond
            ((equal (getf env :path-info) "/404")
             `(404 () ("not found")))
            ((zerop (mod (incf call) 5))
             `(200 () ("ok")))
            (t `(422 () ("ng")))))
      (testing "retry-request"
        (let ((retry (dex:retry-request 2 :interval 0)))
          (handler-case
              (handler-bind ((dex:http-request-failed retry))
                (dex:get (localhost "/")))
            (dex:http-request-failed ()
              (ok (= call 3)))))
        (handler-bind ((dex:http-request-failed #'dex:retry-request))
          (multiple-value-bind (body code headers)
              (dex:get (localhost "/"))
            (declare (ignore body headers))
            (ok (eql code 200))
            (ok (= call 5)))))
      (testing "ignore-and-continue"
        (handler-bind ((dex:http-request-not-found #'dex:ignore-and-continue))
          (multiple-value-bind (body code headers)
              (dex:get (localhost "/404"))
            (declare (ignore body headers))
            (ok (eql code 404))
            (ok (= call 5)))
          (ok (signals (dex:get (localhost "/"))
                       'dex:http-request-failed)))))))
