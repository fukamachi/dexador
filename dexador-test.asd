#|
  This file is a part of dexador project.
  Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage dexador-test-asd
  (:use :cl :asdf))
(in-package :dexador-test-asd)

(defsystem dexador-test
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on (:dexador
               :prove
               :lack-request
               :clack-test
               :babel
               :cl-cookie)
  :components ((:module "t"
                :components
                ((:test-file "dexador"))))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
