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

(defun get (uri &key (version 1.1) headers keep-alive socket)
  (request uri :method :get
               :version version
               :headers headers
               :socket socket
               :keep-alive keep-alive))

(defun post (uri &key (version 1.1) headers content keep-alive socket)
  (request uri :method :post
               :version version
               :headers headers
               :content content
               :socket socket
               :keep-alive keep-alive))

(defun head (uri &key (version 1.1) headers socket)
  (request uri :method :head
               :version version
               :headers headers
               :socket socket
               :keep-alive nil))

(defun put (uri &key (version 1.1) headers content keep-alive socket)
  (request uri :method :put
               :version version
               :headers headers
               :content content
               :socket socket
               :keep-alive keep-alive))

(defun delete (uri &key (version 1.1) headers keep-alive socket)
  (request uri :method :delete
               :version version
               :headers headers
               :socket socket
               :keep-alive keep-alive))
