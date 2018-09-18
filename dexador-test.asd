#|
  This file is a part of dexador project.
  Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)
|#

(defsystem "dexador-test"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :depends-on ("dexador"
               "prove"
               "lack-request"
               "clack-test"
               "babel"
               "cl-cookie")
  :components ((:module "t"
                :components
                ((:test-file "dexador"))))
  :defsystem-depends-on ("prove-asdf")
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
