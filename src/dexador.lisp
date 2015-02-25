(in-package :cl-user)
(defpackage dexador
  (:nicknames :dex)
  (:use :cl)
  (:shadow :get
           :delete)
  (:import-from :dexador.backend.usocket
                :request)
  (:export :request
           :get
           :post
           :head
           :put
           :delete))
(in-package :dexador)

(defun get (uri &key verbose (version 1.1) headers keep-alive socket)
  (request uri :method :get
               :verbose verbose
               :version version
               :headers headers
               :socket socket
               :keep-alive keep-alive))

(defun post (uri &key verbose (version 1.1) headers content keep-alive socket)
  (request uri :method :post
               :verbose verbose
               :version version
               :headers headers
               :content content
               :socket socket
               :keep-alive keep-alive))

(defun head (uri &key verbose (version 1.1) headers socket)
  (request uri :method :head
               :verbose verbose
               :version version
               :headers headers
               :socket socket
               :keep-alive nil))

(defun put (uri &key verbose (version 1.1) headers content keep-alive socket)
  (request uri :method :put
               :verbose verbose
               :version version
               :headers headers
               :content content
               :socket socket
               :keep-alive keep-alive))

(defun delete (uri &key verbose (version 1.1) headers keep-alive socket)
  (request uri :method :delete
               :verbose verbose
               :version version
               :headers headers
               :socket socket
               :keep-alive keep-alive))
