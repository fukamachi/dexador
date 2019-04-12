#|
  This file is a part of dexador project.
  Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)
|#

#|
  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(defsystem "dexador"
  :version "0.9.10"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :defsystem-depends-on ("trivial-features")
  :depends-on ("fast-http"
               "quri"
               "fast-io"
               "babel"
               "trivial-gray-streams"
               "chunga"
               "cl-ppcre"
               "cl-cookie"
               "trivial-mimes"
               "chipz"
               "cl-base64"
               "cl-reexport"
               (:feature (:not :windows) "usocket")
               (:feature :windows "winhttp")
               (:feature (:and (:not :windows) (:not :dexador-no-ssl)) "cl+ssl")
               "bordeaux-threads"
               "alexandria")
  :components ((:module "src"
                :components
                ((:file "dexador" :depends-on ("backend" "error"))
                 (:file "encoding")
                 (:file "connection-cache")
                 (:file "decoding-stream")
                 (:file "keep-alive-stream")
                 (:file "body" :depends-on ("encoding" "decoding-stream" "util"))
                 (:file "error")
                 (:file "util")
                 (:module "backend"
                  :depends-on ("encoding" "connection-cache" "decoding-stream" "keep-alive-stream" "body" "error" "util")
                  :components
                  ((:file "usocket" :if-feature (:not :windows))
                   (:file "winhttp" :if-feature :windows))))))
  :description "Yet another HTTP client for Common Lisp"
  :long-description #.(read-file-string (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "dexador-test"))))
