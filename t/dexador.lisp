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
  (loop for port from (+ 50000 (random 1000)) upto 60000
        if (clack.test::port-available-p port)
          return port))

(defmacro testing-app ((desc &key use-connection-pool) app &body body)
  `(let ((*clack-test-port* (random-port)))
     (clack.test:testing-app ,desc ,app
       ;; Clack's TESTING-APP sets dex:*use-connection-pool* to NIL,
       ;; but we need to change it in some tests
       (let ((dex:*use-connection-pool* ,use-connection-pool))
         (dex:clear-connection-pool)
         ,@body))))

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
  #+windows
  (skip "Skipped proxy tests on Windows")
  #-windows
  (testing-app ("proxy (http) case")
      ; proxy behavior is same as direct connection if http
      (lambda (env)
        (let ((body (format nil "~A~%~A"
                            (gethash "host" (getf env :headers))
                            (getf env :request-uri))))
          `(200 (:content-length ,(length body)) (,body))))
    (testing "GET"
      (multiple-value-bind (body code)
          (dex:get "http://lisp.org/foo"
                   :headers '((:x-foo . "ppp"))
                   :proxy (localhost))
        (ok (eql code 200))
        (ok (equal body (format nil "lisp.org~%/foo")))))
    (testing "HEAD"
      (multiple-value-bind (body code)
          (dex:head "http://lisp.org/foo"
                    :proxy (localhost))
        (ok (eql code 200))
        (ok (equal body ""))))
    (testing "PUT"
      (multiple-value-bind (body code)
          (dex:put "http://lisp.org/foo"
                   :proxy (localhost))
        (ok (eql code 200))
        (ok (equal body (format nil "lisp.org~%/foo")))))
    (testing "DELETE"
      (multiple-value-bind (body code)
          (dex:delete "http://lisp.org/foo"
                      :proxy (localhost))
        (ok (eql code 200))
        (ok (equal body (format nil "lisp.org~%/foo")))))))

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
              '(:set-cookie "_auid=a8acafbaef245a806f6a308506dc95c8; path=/; expires=Mon, 10-Jul-2017 12:32:47 GMT"
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
    (ok (dex:get (localhost) :verbose t))
    ;; The WinHTTP backend ignores :verbose.
    #-windows
    (flet ((verbose-output (&rest args)
             (with-output-to-string (*standard-output*)
               (apply #'dex:get (localhost) :verbose t args))))
      (testing "basic-auth is redacted"
        (let ((output (verbose-output :basic-auth '("user" . "pass"))))
          (ok (search "Authorization: Basic " output))
          (ng (search "dXNlcjpwYXNz" output))))
      (testing "bearer-auth is redacted"
        (let ((output (verbose-output :bearer-auth "secret-token")))
          (ok (search "Authorization: Bearer " output))
          (ng (search "secret-token" output))))
      (testing "Authorization header given in :headers is redacted"
        (let ((output (verbose-output :headers '(("authorization" . "Token secret-token")))))
          (ok (search "Token " output))
          (ng (search "secret-token" output))))
      (testing "Proxy-Authorization is redacted"
        (let ((output (with-output-to-string (*standard-output*)
                        (dex:get "http://example.com/"
                                 :verbose t
                                 :proxy (format nil "http://proxyuser:proxypass@127.0.0.1:~D"
                                                *clack-test-port*)))))
          (ok (search "Proxy-Authorization: Basic " output))
          (ng (search (base64:string-to-base64-string "proxyuser:proxypass") output))))
      (testing "other headers are kept"
        (let ((output (verbose-output :headers '((:x-foo . "bar")))))
          (ok (search "X-Foo: bar" output)))))))

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
;;;; Request transport ownership, without sockets or TLS.

(define-condition abandoned-request (serious-condition) ())
(define-condition abandoned-stream-close-error (error) ())

(defclass recording-http-stream
    (trivial-gray-streams:fundamental-binary-input-stream
     trivial-gray-streams:fundamental-binary-output-stream
     trivial-gray-streams:fundamental-character-input-stream
     trivial-gray-streams:fundamental-character-output-stream)
  ((input :initarg :input :initform #() :reader recording-input)
   (position :initform 0 :accessor recording-position)
   (output :initform (make-array 0 :element-type '(unsigned-byte 8)
                                 :adjustable t :fill-pointer 0)
           :reader recording-output)
   (reads :initform 0 :accessor recording-reads)
   (listens :initform 0 :accessor recording-listens)
   (closes :initform nil :accessor recording-closes)
   (on-read :initarg :on-read :initform nil :reader recording-on-read)
   (fail-close :initarg :fail-close :initform nil :reader recording-fail-close)))

(defmethod stream-element-type ((stream recording-http-stream))
  '(unsigned-byte 8))
(defmethod trivial-gray-streams:stream-read-byte ((stream recording-http-stream))
  (incf (recording-reads stream))
  (when (recording-on-read stream)
    (funcall (recording-on-read stream)))
  (if (< (recording-position stream) (length (recording-input stream)))
      (prog1 (aref (recording-input stream) (recording-position stream))
        (incf (recording-position stream)))
      :eof))
(defmethod trivial-gray-streams:stream-read-char ((stream recording-http-stream))
  (let ((byte (trivial-gray-streams:stream-read-byte stream)))
    (if (eq byte :eof) :eof (code-char byte))))
(defmethod trivial-gray-streams:stream-listen ((stream recording-http-stream))
  (incf (recording-listens stream))
  (< (recording-position stream) (length (recording-input stream))))
(defmethod trivial-gray-streams:stream-write-byte ((stream recording-http-stream) byte)
  (vector-push-extend byte (recording-output stream))
  byte)
(defmethod trivial-gray-streams:stream-write-char ((stream recording-http-stream) char)
  (trivial-gray-streams:stream-write-byte stream (char-code char))
  char)
(defmethod trivial-gray-streams:stream-write-sequence
    ((stream recording-http-stream) sequence start end &key)
  (loop for index from start below end
        for element = (elt sequence index)
        do (trivial-gray-streams:stream-write-byte
            stream (if (characterp element) (char-code element) element)))
  sequence)
(defmethod trivial-gray-streams:stream-read-sequence
    ((stream recording-http-stream) sequence start end &key)
  (loop for index from start below end
        for byte = (trivial-gray-streams:stream-read-byte stream)
        when (eq byte :eof) return index
        do (setf (elt sequence index)
                 (if (stringp sequence) (code-char byte) byte))
        finally (return end)))

(defmethod trivial-gray-streams:stream-force-output ((stream recording-http-stream)) nil)
(defmethod trivial-gray-streams:stream-finish-output ((stream recording-http-stream)) nil)
(defmethod close ((stream recording-http-stream) &key abort)
  (push abort (recording-closes stream))
  (call-next-method)
  (when (recording-fail-close stream)
    (error 'abandoned-stream-close-error))
  t)

(defun recording-response (&optional (body "abcdef") (status "200 OK"))
  (babel:string-to-octets
   (format nil "HTTP/1.1 ~A~C~CContent-Length: ~D~C~CContent-Type: application/octet-stream~C~CConnection: keep-alive~C~C~C~C~A"
           status #\Return #\Newline (length body) #\Return #\Newline
           #\Return #\Newline #\Return #\Newline #\Return #\Newline body)
   :encoding :utf-8))

(defmacro with-recording-pool (&body body)
  `(let ((dexador.connection-cache:*connection-pool*
           (dexador.connection-cache:make-connection-pool))
         (dexador.connection-cache:*use-connection-pool* t))
     (unwind-protect (progn ,@body)
       (dexador.connection-cache:clear-connection-pool))))

(defun request-from-recording-pool (stream)
  (dexador.connection-cache:push-connection "http://ownership.test" stream #'close)
  (dexador.backend.usocket:request
   "http://ownership.test/" :proxy nil :want-stream t
   :keep-alive t :use-connection-pool t))

(deftest abandoned-request-closes-stream-abortively
  (dolist (keep-alive '(nil t))
    (let* ((original (make-condition 'abandoned-request))
           (stream (make-instance 'recording-http-stream
                                 :on-read (lambda () (error original))))
           (caught (handler-case
                       (dexador.backend.usocket:request
                        "http://ownership.test/" :proxy nil :stream stream
                        :want-stream t :keep-alive keep-alive :use-connection-pool nil)
                     (abandoned-request (condition) condition))))
      (ok (eq caught original) "The original read condition escapes")
      (ok (equal (recording-closes stream) '(t)) "Abandoned transport is abort-closed once")
      (ok (not (open-stream-p stream)) "The abandoned transport is closed"))))

(deftest abandoned-request-preserves-condition-when-close-fails
  (let* ((original (make-condition 'abandoned-request))
         (stream (make-instance 'recording-http-stream :fail-close t
                               :on-read (lambda () (error original))))
         (caught (handler-case
                     (dexador.backend.usocket:request
                      "http://ownership.test/" :proxy nil :stream stream
                      :want-stream t :keep-alive nil :use-connection-pool nil)
                   (serious-condition (condition) condition))))
    (ok (eq caught original) "Cleanup failure must not replace the read condition")
    (ok (equal (recording-closes stream) '(t)) "Failing cleanup was abortive")))

(deftest abandoned-request-preserves-throw-when-close-fails
  (let* ((stream (make-instance 'recording-http-stream :fail-close t
                               :on-read (lambda () (throw 'abandoned-request :escaped))))
         (result (handler-case
                     (catch 'abandoned-request
                       (dexador.backend.usocket:request
                        "http://ownership.test/" :proxy nil :stream stream
                        :want-stream t :keep-alive nil :use-connection-pool nil))
                   (error (condition) condition))))
    (ok (eq result :escaped) "Cleanup failure must not replace THROW")
    (ok (equal (recording-closes stream) '(t)) "Failing cleanup was abortive")))

(deftest pooled-stream-abort-close-does-not-repool
  (with-recording-pool
    (let* ((stream (make-instance 'recording-http-stream :input (recording-response)))
           (body (request-from-recording-pool stream))
           (reads (recording-reads stream))
           (listens (recording-listens stream)))
      (ok (streamp body) "The request handed off a response stream")
      (ok (null (recording-closes stream)) "Request did not dispose the handed-off stream")
      (close body :abort t)
      (ok (= reads (recording-reads stream)) "Abort does not drain unread body bytes")
      (ok (= listens (recording-listens stream)) "Abort does not probe input availability")
      (ok (equal (recording-closes stream) '(t)) "Abort reaches the underlying transport")
      (ok (not (open-stream-p stream)) "Aborted transport is closed")
      (ok (null (dexador.connection-cache:steal-connection "http://ownership.test"))
          "Aborted transport was not returned to the pool"))))

(deftest pooled-stream-normal-close-reuses-transport
  (with-recording-pool
    (let* ((stream (make-instance 'recording-http-stream :input (recording-response)))
           (body (request-from-recording-pool stream))
           (reads (recording-reads stream)))
      (close body)
      (ok (> (recording-reads stream) reads) "Normal close drains remaining body bytes")
      (ok (null (recording-closes stream)) "Normal close leaves transport open")
      (ok (open-stream-p stream))
      (ok (eq stream (dexador.connection-cache:steal-connection "http://ownership.test"))
          "The exact transport is available for reuse")
      (close stream :abort t))))

(deftest pooled-stream-eof-returns-transport-once
  (with-recording-pool
    (let* ((stream (make-instance 'recording-http-stream :input (recording-response)))
           (body (request-from-recording-pool stream)))
      (ok (equal (loop for byte = (read-byte body nil nil) while byte collect byte)
                 '(97 98 99 100 101 102)))
      (ok (eq :eof (read-byte body nil :eof)))
      (close body)
      (ok (null (recording-closes stream)) "EOF leaves the reusable transport open")
      (ok (eq stream (dexador.connection-cache:steal-connection "http://ownership.test")))
      (ok (null (dexador.connection-cache:steal-connection "http://ownership.test"))
          "EOF and subsequent CLOSE do not duplicate pool entries")
      (close stream :abort t))))

(deftest unpooled-response-stream-caller-ownership
  (dolist (keep-alive '(nil t))
    (let* ((stream (make-instance 'recording-http-stream :input (recording-response)))
           (body (dexador.backend.usocket:request
                  "http://ownership.test/" :proxy nil :stream stream :want-stream t
                  :keep-alive keep-alive :use-connection-pool nil)))
      (ok (streamp body))
      (ok (null (recording-closes stream)) "The caller owns the handed-off body")
      (close body :abort t)
      (ok (equal (recording-closes stream) '(t))))))

(deftest error-response-stream-caller-ownership
  (let* ((stream (make-instance 'recording-http-stream
                               :input (recording-response "abcdef" "404 Not Found")))
         (condition (handler-case
                        (dexador.backend.usocket:request
                         "http://ownership.test/" :proxy nil :stream stream :want-stream t
                         :keep-alive nil :use-connection-pool nil)
                      (dexador.error:http-request-failed (condition) condition)))
         (body (dexador.error:response-body condition)))
    (ok (streamp body))
    (ok (null (recording-closes stream)) "HTTP error response body belongs to the caller")
    (ok (= (read-byte body) 97) "The error body is readable after unwind")
    (close body :abort t)
    (ok (equal (recording-closes stream) '(t)))))

;;;; Ownership at redirect and conversion boundaries.

(defun recording-redirect (location &optional (connection "keep-alive"))
  (babel:string-to-octets
   (format nil "HTTP/1.1 302 Found~C~CLocation: ~A~C~CContent-Length: 0~C~CConnection: ~A~C~C~C~C"
           #\Return #\Newline location #\Return #\Newline #\Return #\Newline
           connection #\Return #\Newline #\Return #\Newline)))

(deftest cross-origin-redirect-relinquishes-transferred-transport
  (dolist (target-fails '(nil t))
    (with-recording-pool
      (let* ((original (make-condition 'abandoned-request))
             (old (make-instance 'recording-http-stream
                                 :input (recording-redirect "http://target.test/")))
             (new (make-instance 'recording-http-stream :input (recording-response)
                                 :on-read (when target-fails (lambda () (error original))))))
        (dexador.connection-cache:push-connection "http://ownership.test" old #'close)
        (dexador.connection-cache:push-connection "http://target.test" new #'close)
        (let ((result (handler-case
                          (dexador.backend.usocket:request
                           "http://ownership.test/" :proxy nil :want-stream t
                           :keep-alive t :use-connection-pool t)
                        (abandoned-request (condition) condition))))
          (ok (if target-fails (eq original result) (streamp result))
              "The recursive result or original failure escapes")
          (ok (null (recording-closes old)) "The transferred transport was not closed")
          (ok (open-stream-p old) "The transferred transport is open")
          (ok (eq old (dexador.connection-cache:steal-connection "http://ownership.test"))
              "The old pool owns the original transport")
          (if target-fails
              (ok (equal (recording-closes new) '(t)) "The failed target is disposed once")
              (close result :abort t))
          (close old :abort t))))))

(deftest cross-origin-failed-transfer-retains-ownership
  (with-recording-pool
    (let* ((stream (make-instance 'recording-http-stream
                                  :input (recording-redirect "http://target.test/")
                                  :fail-close t))
           (caught (handler-case
                       (dexador.backend.usocket:request
                        "http://ownership.test/" :proxy nil :stream stream
                        :want-stream t :use-connection-pool nil)
                     (abandoned-stream-close-error (condition) condition))))
      (ok (typep caught 'abandoned-stream-close-error) "Failed disposal escapes")
      (ok (equal (recording-closes stream) '(t nil))
          "A failed ordinary disposal retains ownership for guarded abort")
      (ok (null (dexador.connection-cache:steal-connection "http://ownership.test")))
      (ok (not (open-stream-p stream))))))

(deftest conversion-failure-aborts-owned-transport
  (dolist (mode '(:error :throw))
    (dolist (keep-alive '(nil t))
      (with-recording-pool
        (let* ((bytes (recording-response))
               (threshold (- (length bytes) 6))
               (original (make-condition 'simple-error :format-control "Conversion failed"))
               (stream nil))
          (setf stream
                (make-instance 'recording-http-stream :input bytes
                               :on-read (lambda ()
                                          (when (>= (recording-position stream) threshold)
                                            (if (eq mode :throw)
                                                (throw 'conversion :escaped)
                                                (error original))))))
          (dexador.connection-cache:push-connection "http://ownership.test" stream #'close)
          (let ((caught (handler-case
                            (catch 'conversion
                              (dexador.backend.usocket:request
                               "http://ownership.test/" :proxy nil :want-stream t
                               :force-string t :keep-alive keep-alive :use-connection-pool t))
                          (error (condition) condition))))
            (ok (eq caught (if (eq mode :throw) :escaped original))
                "Conversion preserves the exact ERROR or THROW value")
            (ok (equal (recording-closes stream) '(t)) "Failed conversion aborts once")
            (ok (not (open-stream-p stream)))
            (ok (null (dexador.connection-cache:steal-connection "http://ownership.test")))))))))

(deftest handoff-controls-keep-transport-open
  (dolist (status '("200 OK" "400 Bad Request" "500 Internal Server Error"))
    (with-recording-pool
      (let* ((stream (make-instance 'recording-http-stream
                                    :input (recording-response "abcdef" status)))
             (body (handler-case (request-from-recording-pool stream)
                     (dexador.error:http-request-failed (condition)
                       (dexador.error:response-body condition)))))
        (ok (streamp body))
        (ok (null (recording-closes stream)) "Normal and HTTP-error bodies own live transports")
        (ok (= 97 (read-byte body)))
        (close body :abort t)
        (ok (equal (recording-closes stream) '(t)))))))

(deftest same-origin-method-redirect-preserves-single-owner
  (dolist (outcome '(:success :error :throw :http-error))
    (let* ((redirect (recording-redirect "/target"))
           (original (make-condition 'abandoned-request))
           (stream nil))
      (setf stream
            (make-instance 'recording-http-stream
                           :input (concatenate '(vector (unsigned-byte 8)) redirect
                                               (recording-response "abcdef"
                                                 (if (eq outcome :http-error)
                                                     "500 Internal Server Error" "200 OK")))
                           :on-read (lambda ()
                                      (when (>= (recording-position stream) (length redirect))
                                        (case outcome
                                          (:error (error original))
                                          (:throw (throw 'redirect :escaped)))))))
      (let ((result (handler-case
                        (catch 'redirect
                          (dexador.backend.usocket:request
                           "http://ownership.test/" :proxy nil :method :post :stream stream
                           :want-stream t :keep-alive t :use-connection-pool nil))
                      (abandoned-request (condition) condition)
                      (dexador.error:http-request-failed (condition)
                        (dexador.error:response-body condition)))))
        (cond
          ((member outcome '(:success :http-error))
           (ok (streamp result))
           (ok (null (recording-closes stream)) "The redirected body owns the transport")
           (ok (open-stream-p stream))
           (ok (= 97 (read-byte result)))
           (close result :abort t)
           (ok (equal (recording-closes stream) '(t))))
          (t
           (ok (eq result (if (eq outcome :throw) :escaped original)))
           (ok (equal (recording-closes stream) '(t)) "Recursive failure disposes only once")
           (ok (not (open-stream-p stream)))))))))

(deftest buffered-conversion-failure-discards-transport
  (with-recording-pool
    (let* ((bytes (babel:string-to-octets
                   (format nil "HTTP/1.1 200 OK~C~CContent-Length: 8~C~CContent-Encoding: gzip~C~CConnection: keep-alive~C~C~C~Cnot-gzip"
                           #\Return #\Newline #\Return #\Newline #\Return #\Newline
                           #\Return #\Newline #\Return #\Newline)))
           (stream (make-instance 'recording-http-stream :input bytes)))
      (dexador.connection-cache:push-connection "http://ownership.test" stream #'close)
      (let ((caught (handler-case
                        (dexador.backend.usocket:request
                         "http://ownership.test/" :proxy nil :want-stream nil
                         :keep-alive t :use-connection-pool t)
                      (error (condition) condition))))
        (ok (typep caught 'error) "Buffered decompression fails")
        (ok (equal (recording-closes stream) '(t)) "Buffered conversion failure aborts")
        (ok (not (open-stream-p stream)))
        (ok (null (dexador.connection-cache:steal-connection "http://ownership.test")))))))

(deftest cross-origin-disabled-global-pool-disposes-transport
  (dolist (local-pooling '(nil t))
    (with-recording-pool
      (let* ((dexador.backend.usocket::*default-proxy* nil)
             (stream (make-instance 'recording-http-stream
                        :input (recording-redirect "http://target.test/")
                        :on-read (lambda ()
                                   (setf dexador.connection-cache:*use-connection-pool* nil
                                         ;; Recursive REQUEST parses this default before
                                         ;; acquiring a transport. No socket is opened.
                                         dexador.backend.usocket::*default-proxy*
                                         "http://proxy.test:invalid/")))))
        (when local-pooling
          (dexador.connection-cache:push-connection "http://ownership.test" stream #'close))
        (let ((caught (handler-case
                          (dexador.backend.usocket:request
                           "http://ownership.test/" :want-stream t :keep-alive t
                           :use-connection-pool local-pooling
                           :stream (unless local-pooling stream))
                        (error (condition) condition))))
          (ok (typep caught 'error) "Recursive proxy parsing fails before socket acquisition")
          (ok (equal (recording-closes stream) '(nil))
              "Disabled admission disposes the old transport before releasing ownership")
          (ok (not (open-stream-p stream)))
          (setf dexador.connection-cache:*use-connection-pool* t)
          (ok (null (dexador.connection-cache:steal-connection "http://ownership.test")))
          (when (open-stream-p stream) (close stream :abort t)))))))
