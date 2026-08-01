#|
  This file is a part of dexador project.
  Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)
|#

(defsystem "dexador-test"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :defsystem-depends-on ("trivial-features")
  :depends-on ("dexador"
               ;; usocket backend on Windows: the unit proxy tests target
               ;; loopback origins, which WinHTTP never proxies.
               (:feature :windows "dexador-usocket")
               "rove"
               "lack-request"
               "clack-test"
               "babel"
               "cl-cookie"
               "usocket"
               "bordeaux-threads"
               "cl-base64"
               "quri")
  :components ((:module "t"
                :components
                ((:file "proxy-server")
                 (:file "ntlm-server")
                 (:file "dexador" :depends-on ("proxy-server" "ntlm-server")))))
  :perform (test-op (op c) (symbol-call '#:rove '#:run c)))
