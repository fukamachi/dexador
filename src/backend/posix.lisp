(in-package :cl-user)
(defpackage dexador.backend.posix
  (:use :cl))
(in-package :dexador.backend.posix)

(cffi:defcfun ("connect" connect) :int
  (socket :int)
  (address :pointer)
  (addrlen socklen-t))
