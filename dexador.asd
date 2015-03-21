#|
  This file is a part of dexador project.
  Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)
|#

#|
  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage dexador-asd
  (:use :cl :asdf))
(in-package :dexador-asd)

(defsystem dexador
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on (:usocket
               :fast-http
               :quri
               :fast-io
               :babel
               :cl-ppcre
               :cl-cookie
               :trivial-mimes
               :chipz
               :cl-base64
               #-dexador-no-ssl :cl+ssl
               :bordeaux-threads
               :alexandria)
  :components ((:module "src"
                :components
                ((:file "dexador" :depends-on ("backend"))
                 (:file "encoding")
                 (:file "connection-cache")
                 (:file "util")
                 (:module "backend"
                  :depends-on ("encoding" "connection-cache" "util")
                  :components
                  ((:file "usocket"))))))
  :description "Yet another HTTP client for Common Lisp"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op dexador-test))))
