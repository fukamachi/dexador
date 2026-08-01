(defsystem "dexador-integration-test"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :description "Integration tests against a real external proxy (see .github/workflows/ci.yml)"
  :defsystem-depends-on ("trivial-features")
  :depends-on ("dexador"
               ;; usocket backend on Windows: covers SOCKS5 and the loopback
               ;; origins that WinHTTP never proxies.
               (:feature :windows "dexador-usocket")
               "rove"
               "clack-test"
               "quri")
  :components ((:module "t"
                :components
                ((:file "integration"))))
  :perform (test-op (op c) (symbol-call '#:rove '#:run c)))
