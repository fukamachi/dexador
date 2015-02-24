(in-package :cl-user)
(defpackage dexador
  (:nicknames :dex)
  (:use :cl)
  (:shadow :get
           :delete)
  #-(and unix (not clisp))
  (:import-from :dexador.backend.usocket
                :request)
  #+(and unix (not clisp))
  (:import-from :dexador.backend.posix
                :request)
  (:export :request
           :get
           :post
           :head
           :put
           :delete))
(in-package :dexador)

(defun get (uri &key (protocol :http/1.1) socket keep-alive)
  (request uri :method :get
               :protocol protocol
               :socket socket
               :keep-alive keep-alive))

(defun post (uri &key (protocol :http/1.1) socket keep-alive)
  (request uri :method :post
               :protocol protocol
               :socket socket
               :keep-alive keep-alive))

(defun head (uri &key (protocol :http/1.1) socket)
  (request uri :method :head
               :protocol protocol
               :socket socket
               :keep-alive nil))

(defun put (uri &key (protocol :http/1.1) socket keep-alive)
  (request uri :method :put
               :protocol protocol
               :socket socket
               :keep-alive keep-alive))

(defun delete (uri &key (protocol :http/1.1) socket keep-alive)
  (request uri :method :delete
               :protocol protocol
               :socket socket
               :keep-alive keep-alive))
