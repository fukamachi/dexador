(in-package :cl-user)
(defpackage dexador-test
  (:use :cl
        :prove)
  (:import-from :clack.test
                :*clack-test-port*
                :port-available-p
                :localhost))
(in-package :dexador-test)

(plan 17)

(defun random-port ()
  "Return a port number not in use from 50000 to 60000."
  (loop for port from (+ 50000 (random 1000)) upto 60000
        if (clack.test::port-available-p port)
          return port))

(defmacro subtest-app (desc app &body body)
  `(let ((*clack-test-port* (random-port)))
     (clack.test:subtest-app ,desc ,app
       (dex:clear-connection-pool)
       ,@body)))

(subtest-app "normal case"
    (lambda (env)
      `(200 (:content-length ,(length (getf env :request-uri))) (,(getf env :request-uri))))
  (subtest "GET"
    (multiple-value-bind (body code headers)
        (dex:get (localhost "/foo")
                 :headers '((:x-foo . "ppp")))
      (is code 200)
      (is body "/foo")
      (is (gethash "content-length" headers) 4)))
  (subtest "HEAD"
    (multiple-value-bind (body code)
        (dex:head (localhost "/foo"))
      (is code 200)
      (is body "")))
  (subtest "PUT"
    (multiple-value-bind (body code)
        (dex:put (localhost "/foo"))
      (is code 200)
      (is body "/foo")))
  (subtest "DELETE"
    (multiple-value-bind (body code)
        (dex:delete (localhost "/foo"))
      (is code 200)
      (is body "/foo"))))

(subtest-app "proxy (http) case"
    ; proxy behavior is same as direct connection if http
    (lambda (env)
      `(200 (:content-length ,(length (getf env :request-uri))) (,(getf env :request-uri))))
  (subtest "GET"
    (multiple-value-bind (body code headers)
        (dex:get (localhost "/foo")
                 :headers '((:x-foo . "ppp"))
                 :proxy (localhost))
      (is code 200)
      (is body (localhost "/foo"))
      (is (gethash "content-length" headers) (length (localhost "/foo")))))
  (subtest "HEAD"
    (multiple-value-bind (body code)
        (dex:head (localhost "/foo")
                  :proxy (localhost))
      (is code 200)
      (is body "")))
  (subtest "PUT"
    (multiple-value-bind (body code)
        (dex:put (localhost "/foo")
                 :proxy (localhost))
      (is code 200)
      (is body (localhost "/foo"))))
  (subtest "DELETE"
    (multiple-value-bind (body code)
        (dex:delete (localhost "/foo")
                    :proxy (localhost))
      (is code 200)
      (is body (localhost "/foo")))))

(subtest-app "redirection"
    (lambda (env)
      (let ((id (parse-integer (subseq (getf env :path-info) 1))))
        (cond
          ((= id 3)
           '(200 (:content-length 2) ("OK")))
          ((<= 300 id 399)
           '(302 (:location "/200") ()))
          ((= id 200)
           (let ((method (princ-to-string (getf env :request-method))))
             `(200 (:content-length ,(length method))
                   (,method))))
          (T
           `(302 (:location ,(format nil "/~D" (1+ id))) ())))))
  (subtest "redirect"
    (multiple-value-bind (body code headers)
        (dex:get (localhost "/1"))
      (is code 200)
      (is body "OK")
      (is (gethash "content-length" headers) 2)))
  (subtest "not enough redirect"
    (multiple-value-bind (body code headers)
        (dex:get (localhost "/1") :max-redirects 0)
      (declare (ignore body))
      (is code 302)
      (is (gethash "location" headers) "/2")))
  (subtest "exceed max redirect"
    (multiple-value-bind (body code headers)
        (dex:get (localhost "/4") :max-redirects 7)
      (declare (ignore body))
      (is code 302)
      (is (gethash "location" headers) "/12")))
  (subtest "Don't redirect POST"
    (multiple-value-bind (body code)
        (dex:post (localhost "/301"))
      (declare (ignore body))
      (is code 302))))

(subtest "content-disposition"
  (is (dexador.backend.usocket::content-disposition "upload" #P"data/plain-file.txt")
      (format nil "Content-Disposition: form-data; name=\"upload\"; filename=\"plain-file.txt\"~C~C"
              #\Return #\Newline)
      "ASCII file name")
  (is (dexador.backend.usocket::content-disposition "upload" #P"data/plain file.txt")
      (format nil "Content-Disposition: form-data; name=\"upload\"; filename=\"plain file.txt\"~C~C"
              #\Return #\Newline)
      "ASCII file name with space")
  #+ecl
  (skip 1 "UTF-8 pathname is not allowed on ECL")
  #-ecl
  (is (dexador.backend.usocket::content-disposition "upload" #P"data/foo-あいうえお.txt")
      (format nil "Content-Disposition: form-data; name=\"upload\"; filename*=UTF-8''foo-%E3%81%82%E3%81%84%E3%81%86%E3%81%88%E3%81%8A.txt~C~C"
              #\Return #\Newline)
      "UTF-8 file name")
  (is (dexador.backend.usocket::content-disposition "title" "ignore")
      (format nil "Content-Disposition: form-data; name=\"title\"~C~C"
              #\Return #\Newline)
      "string value"))

(subtest-app "POST request"
    (lambda (env)
      (cond
        ((string= (getf env :path-info) "/upload")
         (let ((buf (make-array (getf env :content-length)
                                :element-type '(unsigned-byte 8))))
           (read-sequence buf (getf env :raw-body))
           `(200 ()
                 (,(babel:octets-to-string buf)))))
        (T
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
                                           (babel:octets-to-string (subseq buf 0 n))))
                                        ((consp v)
                                         (car v))
                                        (t v)))))))))))
  (subtest "content in alist"
    (multiple-value-bind (body code headers)
        (dex:post (localhost)
                  :content '(("name" . "Eitaro")
                             ("email" . "e.arrows@gmail.com")))
      (declare (ignore headers))
      (is code 200)
      (is body "name: Eitaro
email: e.arrows@gmail.com
")))
  (subtest "string content"
    (multiple-value-bind (body code headers)
        (dex:post (localhost "/upload")
                  :content "this is string data")
      (declare (ignore headers))
      (is code 200)
      (is body "this is string data")))
  (subtest "octets content"
    (multiple-value-bind (body code headers)
        (dex:post (localhost "/upload")
                  :content (babel:string-to-octets "this is octet data"))
      (declare (ignore headers))
      (is code 200)
      (is body "this is octet data")))
  (subtest "multipart"
    (multiple-value-bind (body code)
        (dex:post (localhost)
                  :content `(("title" . "Road to Lisp")
                             ("body" . ,(asdf:system-relative-pathname :dexador #P"t/data/quote.txt"))))
      (is code 200)
      (is body
          "title: Road to Lisp
body: \"Within a couple weeks of learning Lisp I found programming in any other language unbearably constraining.\" -- Paul Graham, Road to Lisp

")))
  (subtest "upload"
    (multiple-value-bind (body code)
        (dex:post (localhost "/upload")
                  :content (asdf:system-relative-pathname :dexador #P"t/data/quote.txt"))
      (is code 200)
      (is body "\"Within a couple weeks of learning Lisp I found programming in any other language unbearably constraining.\" -- Paul Graham, Road to Lisp
"))))

(subtest-app "HTTP request failed"
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
      (is (dex:response-status e) 500
          "response status is 500")
      (is (dex:response-body e) "Internal Server Error"
          "response body is \"Internal Server Error\"")
      (is (gethash "x-bar" (dex:response-headers e)) 1)))
  (handler-case
      (progn
        (dex:get (localhost "/404"))
        (fail "Must raise an error DEX:HTTP-REQUEST-NOT-FOUND"))
    (dex:http-request-not-found (e)
      (pass "Raise DEX:HTTP-REQUEST-FAILED error")
      (is (dex:response-status e) 404
          "response status is 404")
      (is (dex:response-body e) "Not Found"
          "response body is \"Not Found\"")
      (is (gethash "x-foo" (dex:response-headers e)) 0))))

(subtest-app "Using cookies"
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
    (is (length (cl-cookie:cookie-jar-cookies cookie-jar)) 0 "0 cookies")
    (dex:head (localhost) :cookie-jar cookie-jar)
    (is (length (cl-cookie:cookie-jar-cookies cookie-jar)) 2 "2 cookies")
    (dex:head (localhost) :cookie-jar cookie-jar))

  ;; 302
  (let ((cookie-jar (cl-cookie:make-cookie-jar)))
    (is (length (cl-cookie:cookie-jar-cookies cookie-jar)) 0 "0 cookies")
    (dex:head (localhost "/302") :cookie-jar cookie-jar)
    (is (length (cl-cookie:cookie-jar-cookies cookie-jar)) 2 "2 cookies")
    (dex:head (localhost "/302") :cookie-jar cookie-jar)))

(subtest-app "verbose"
    (lambda (env)
      (declare (ignore env))
      '(200 () ("ok")))
  (ok (dex:get (localhost) :verbose t)))

(subtest-app "want-stream"
    (lambda (env)
      (declare (ignore env))
      '(200 (:content-type "text/plain") ("hi")))
  ;; decoding stream
  (let ((body (dex:get (localhost) :want-stream t :keep-alive nil)))
    (is-type body 'dexador.decoding-stream:decoding-stream
             "body is a decoding stream")
    (is (stream-element-type body) 'babel:unicode-char
        "body is a character stream")
    (let ((buf (make-string 2)))
      (read-sequence buf body)
      (is buf "hi")))
  ;; binary stream
  (let ((body (dex:get (localhost) :want-stream t :force-binary t :keep-alive nil)))
    (is-type body 'stream "body is a stream")
    (is (stream-element-type body) '(unsigned-byte 8)
        "body is a octets stream")
    (let ((buf (make-array 2 :element-type '(unsigned-byte 8))))
      (read-sequence buf body)
      (is (babel:octets-to-string buf) "hi"))))

(subtest-app "big body with want-stream"
    (lambda (env)
      (declare (ignore env))
      `(200 (:content-type "application/json; charset=utf-8"
             :content-length 748)
            ("[{\"name\":\"allow-statement-in-has-a\",\"commit\":{\"sha\":\"d58b3c96503786c64eb2dba22980ebb14010bdbf\",\"url\":\"https://api.github.com/repos/fukamachi/datafly/commits/d58b3c96503786c64eb2dba22980ebb14010bdbf\"}},{\"name\":\"fix-has-a\",\"commit\":{\"sha\":\"4bcea61e84402317ab49605918972983a1511e6a\",\"url\":\"https://api.github.com/repos/fukamachi/datafly/commits/4bcea61e84402317ab49605918972983a1511e6a\"}},{\"name\":\"jojo\",\"commit\":{\"sha\":\"d2b753e7fdd0dbeada9721380cf410186a85535b\",\"url\":\"https://api.github.com/repos/fukamachi/datafly/commits/d2b753e7fdd0dbeada9721380cf410186a85535b\"}},{\"name\":\"master\",\"commit\":{\"sha\":\"d2b753e7fdd0dbeada9721380cf410186a85535b\",\"url\":\"https://api.github.com/repos/fukamachi/datafly/commits/d2b753e7fdd0dbeada9721380cf410186a85535b\"}}]")))
  ;; decoding stream
  (let ((body (dex:get (localhost) :want-stream t)))
    (is-type body 'dexador.decoding-stream:decoding-stream
             "body is a decoding stream")
    (is (stream-element-type body) 'babel:unicode-char
        "body is a character stream")
    (let ((buf (make-string 1024)))
      (is (read-sequence buf body) 748))))

(subtest-app "redirection for want-stream"
    (lambda (env)
      (if (string= (getf env :path-info) "/index.html")
          '(200 () ("ok"))
          '(307 (:location "/index.html"
                 :transfer-encoding "chunked") (""))))
  (let ((body (dex:get (localhost) :want-stream t)))
    (ok body)))

(subtest-app "no body"
    (lambda (env)
      (let ((path (getf env :path-info)))
        (if (string= path "/204")
            '(204 () ())
            '(200 () ()))))
  ;; no Content-Length and no Transfer-Encoding
  (multiple-value-bind (body status headers)
      (dex:get (localhost))
    (is body "")
    (is status 200)
    (is (gethash "content-length" headers) nil)
    (is (gethash "transfer-encoding" headers) nil))
  ;; 204 No Content
  (multiple-value-bind (body status headers)
      (dex:get (localhost "/204"))
    (is body "")
    (is status 204)
    (is (gethash "content-length" headers) nil)
    (is (gethash "transfer-encoding" headers) nil)))

(defvar *json* "{\"name\":\"Eitaro Fukamachi\",\"name_ja\":\"深町英太郎\",\"login\":true}")
(subtest-app "JSON"
    (lambda (env)
      (declare (ignore env))
      `(200 (:content-type "application/json") (,*json*)))
  (multiple-value-bind (body status)
      (dex:get (localhost))
    (is body *json*
        "JSON is returned as a string")
    (is status 200))
  (let ((babel:*default-character-encoding* :cp932))
    ;; Test if the JSON encoding
    (multiple-value-bind (body status)
        (dex:get (localhost))
      (is body *json*
          "The default encoding is UTF-8 though babel:*default-character-encoding* is different")
      (is status 200))))

(subtest-app "keep-alive nil"
    (lambda (env)
      (declare (ignore env))
      '(200 () ("hi")))
  (let ((headers (nth-value 2 (dex:get (localhost)))))
    (is (gethash "connection" headers) nil))
  (let ((headers (nth-value 2 (dex:get (localhost) :keep-alive nil))))
    (is (gethash "connection" headers) "close" :test #'equalp)))

(subtest-app "deflate compression"
    (lambda (env)
      (declare (ignore env))
      `(200 (:content-encoding "deflate" :content-type "text/plain")
            ,(asdf:system-relative-pathname :dexador #p"t/data/test.zlib")))
  (let ((body (dex:get (localhost))))
    (is body "Deflate test string." :test #'string=)))

(subtest-app "gzip compression"
    (lambda (env)
      (declare (ignore env))
      `(200 (:content-encoding "gzip" :content-type "text/plain")
            ,(asdf:system-relative-pathname :dexador #p"t/data/test.gz")))
  (let ((body (dex:get (localhost))))
    (is body "Gzip test string." :test #'string=)))

(subtest
 "unread character"
  (is #\u2602
      (with-open-file (stream (asdf:system-relative-pathname
                               :dexador #p"t/data/umb.bin")
                              :element-type '(unsigned-byte 8))
        (let ((decoding-stream
               (dexador.decoding-stream:make-decoding-stream stream)))
          (peek-char nil decoding-stream)
          (read-char decoding-stream)))))

(finalize)
