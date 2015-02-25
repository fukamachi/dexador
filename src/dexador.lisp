(in-package :cl-user)
(defpackage dexador
  (:nicknames :dex)
  (:use :cl)
  (:shadow :get
           :delete)
  (:import-from :dexador.backend.usocket
                :request)
  (:import-from :dexador.util
                :*default-timeout*)
  (:export :request
           :get
           :post
           :head
           :put
           :delete
           :*default-timeout*))
(in-package :dexador)

(defun get (uri &key (version 1.1) headers keep-alive (timeout *default-timeout*) socket verbose)
  (request uri :method :get
               :version version
               :headers headers
               :keep-alive keep-alive
               :timeout timeout
               :socket socket
               :verbose verbose))

(defun post (uri &key (version 1.1) headers content keep-alive (timeout *default-timeout*) socket verbose)
  (request uri :method :post
               :version version
               :headers headers
               :content content
               :keep-alive keep-alive
               :timeout timeout
               :socket socket
               :verbose verbose))

(defun head (uri &key (version 1.1) headers (timeout *default-timeout*) socket verbose)
  (request uri :method :head
               :version version
               :headers headers
               :keep-alive nil
               :timeout timeout
               :socket socket
               :verbose verbose))

(defun put (uri &key (version 1.1) headers content keep-alive (timeout *default-timeout*) socket verbose)
  (request uri :method :put
               :version version
               :headers headers
               :content content
               :keep-alive keep-alive
               :timeout timeout
               :socket socket
               :verbose verbose))

(defun delete (uri &key (version 1.1) headers keep-alive (timeout *default-timeout*) socket verbose)
  (request uri :method :delete
               :verbose verbose
               :version version
               :headers headers
               :keep-alive keep-alive
               :timeout timeout
               :socket socket
               :verbose verbose))
