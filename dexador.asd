#|
  This file is a part of dexador project.
  Copyright (c) 2015 Eitaro Fukamachi (e.arrows@gmail.com)
|#

#|
  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(defsystem "dexador"
  :version "0.9.16"
  :author "Eitaro Fukamachi"
  :license "MIT"
  :defsystem-depends-on ("trivial-features")
  :depends-on ("fast-http"
               "quri"
               "fast-io"
               "babel"
               "trivial-gray-streams"
               "trivial-garbage"
               "chunga"
               "cl-ppcre"
               "cl-cookie"
               "trivial-mimes"
               "chipz"
               "cl-base64"
               (:feature (:not :dotcl) "usocket")
               (:feature (:and :windows (:not :dotcl)) "winhttp")
               (:feature (:and :windows (:not :dotcl)) "flexi-streams")
               (:feature (:and (:not :windows) (:not :dexador-no-ssl) (:not :dotcl)) "cl+ssl")
               "bordeaux-threads"
               "alexandria"
               (:version "uiop" "3.1.1"))
  :components ((:module "src"
                :components
                ((:file "dexador" :depends-on ("backend" "error" "restarts"))
                 (:file "encoding")
                 (:file "connection-cache")
                 (:file "decoding-stream")
                 (:file "keep-alive-stream")
                 (:file "body" :depends-on ("encoding" "decoding-stream" "util"))
                 (:file "error")
                 (:file "restarts")
                 (:file "util")
                 (:module "backend"
                  :depends-on ("encoding" "connection-cache" "decoding-stream" "keep-alive-stream" "body" "error" "restarts" "util")
                  :components
                  ((:file "usocket" :if-feature (:and (:not :windows) (:not :dotcl)))
                   (:file "winhttp" :if-feature (:and :windows (:not :dotcl)))
                   (:file "dotcl" :if-feature :dotcl))))))
  :description "Yet another HTTP client for Common Lisp"
  :in-order-to ((test-op (test-op "dexador-test"))))
