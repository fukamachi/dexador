(in-package :cl-user)
(defpackage dexador
  (:nicknames :dex)
  (:use :cl)
  (:import-from :dexador.backend.usocket
                :http-request)
  (:export :http-request))
(in-package :dexador)
