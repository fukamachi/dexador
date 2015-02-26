(in-package :cl-user)
(defpackage dexador-test
  (:use :cl
        :prove)
  (:import-from :clack.test
                :test-app))
(in-package :dexador-test)

(plan nil)

(test-app
 (lambda (env)
   `(200 (:content-length ,(length (getf env :request-uri))) (,(getf env :request-uri))))
 (lambda ()
   (multiple-value-bind (body code headers)
       (dex:get "http://localhost:4242/foo"
                :headers '((:x-foo . "ppp")))
     (is code 200)
     (is (babel:octets-to-string body) "/foo")
     (is (gethash "content-length" headers) 4))))

(test-app
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
        `(302 (:location ,(format nil "http://localhost:4242/~D" (1+ id))) ())))))
 (lambda ()
   (subtest "redirect"
     (multiple-value-bind (body code headers)
         (dex:get "http://localhost:4242/1")
       (is code 200)
       (is (babel:octets-to-string body) "OK")
       (is (gethash "content-length" headers) 2)))
   (subtest "not enough redirect"
     (multiple-value-bind (body code headers)
         (dex:get "http://localhost:4242/1" :max-redirects 0)
       (declare (ignore body))
       (is code 302)
       (is (gethash "location" headers) "http://localhost:4242/2")))
   (subtest "exceed max redirect"
     (multiple-value-bind (body code headers)
         (dex:get "http://localhost:4242/4" :max-redirects 7)
       (declare (ignore body))
       (is code 302)
       (is (gethash "location" headers) "http://localhost:4242/12")))
   (subtest "Don't redirect POST"
     (multiple-value-bind (body code)
         (dex:post "http://localhost:4242/301")
       (declare (ignore body))
       (is code 302)))))

(finalize)
