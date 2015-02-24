(in-package :cl-user)
(defpackage dexador
  (:nicknames :dex)
  (:use :cl)
  #-(and unix (not clisp))
  (:import-from :dexador.backend.usocket
                :request)
  #+(and unix (not clisp))
  (:import-from :dexador.backend.posix
                :request)
  (:export :request))
(in-package :dexador)
