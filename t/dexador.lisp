(in-package :cl-user)
(defpackage dexador-test
  (:use :cl
        :prove)
  (:import-from :clack.test
                :subtest-app))
(in-package :dexador-test)

(plan nil)

(subtest-app "normal case"
    (lambda (env)
      `(200 (:content-length ,(length (getf env :request-uri))) (,(getf env :request-uri))))
  (multiple-value-bind (body code headers)
      (dex:get "http://localhost:4242/foo"
               :headers '((:x-foo . "ppp")))
    (is code 200)
    (is body "/foo")
    (is (gethash "content-length" headers) 4)))

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
           `(302 (:location ,(format nil "http://localhost:4242/~D" (1+ id))) ())))))
  (subtest "redirect"
    (multiple-value-bind (body code headers)
        (dex:get "http://localhost:4242/1")
      (is code 200)
      (is body "OK")
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
      (is code 302))))

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
                                      (if (and (consp v)
                                               (streamp (car v)))
                                          (let* ((buf (make-array 1024 :element-type '(unsigned-byte 8)))
                                                 (n (read-sequence buf (car v))))
                                            (babel:octets-to-string (subseq buf 0 n)))
                                          v))))))))))
  (subtest "content in alist"
    (multiple-value-bind (body code headers)
        (dex:post "http://localhost:4242/"
                  :content '(("name" . "Eitaro")
                             ("email" . "e.arrows@gmail.com")))
      (declare (ignore headers))
      (is code 200)
      (is body "name: Eitaro
email: e.arrows@gmail.com
")))
  (subtest "multipart"
    (multiple-value-bind (body code)
        (dex:post "http://localhost:4242/"
                  :content `(("title" . "Road to Lisp")
                             ("body" . ,(asdf:system-relative-pathname :dexador #P"t/data/quote.txt")))
                  :use-connection-pool nil)
      (is code 200)
      (is body
          "title: Road to Lisp
body: \"Within a couple weeks of learning Lisp I found programming in any other language unbearably constraining.\" -- Paul Graham, Road to Lisp

")))
  (subtest "upload"
    (multiple-value-bind (body code)
        (dex:post "http://localhost:4242/upload"
                  :content (asdf:system-relative-pathname :dexador #P"t/data/quote.txt")
                  :use-connection-pool nil)
      (is code 200)
      (is body "\"Within a couple weeks of learning Lisp I found programming in any other language unbearably constraining.\" -- Paul Graham, Road to Lisp
"))))

(finalize)
