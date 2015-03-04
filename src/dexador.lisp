(in-package :cl-user)
(defpackage dexador
  (:nicknames :dex)
  (:use :cl)
  (:shadow :get
           :delete)
  (:import-from :dexador.backend.usocket
                :request)
  (:import-from :dexador.connection-cache
                :*connection-pool*
                :make-connection-pool
                :*reuse-interval*)
  (:import-from :dexador.util
                :*default-timeout*)
  (:export :request
           :get
           :post
           :head
           :put
           :delete
           :*default-timeout*
           :*connection-pool*
           :make-connection-pool
           :*reuse-interval*))
(in-package :dexador)

(defun get (uri &rest args
            &key version headers keep-alive use-connection-pool timeout max-redirects force-binary
              ssl-key-file ssl-cert-file ssl-key-password stream verbose)
  (declare (ignore version headers keep-alive use-connection-pool timeout max-redirects force-binary ssl-key-file ssl-cert-file ssl-key-password stream verbose))
  (apply #'request uri :method :get args))

(defun post (uri &rest args
             &key version headers content keep-alive use-connection-pool timeout force-binary
               ssl-key-file ssl-cert-file ssl-key-password stream verbose)
  (declare (ignore version headers content keep-alive use-connection-pool timeout force-binary ssl-key-file ssl-cert-file ssl-key-password stream verbose))
  (apply #'request uri :method :post args))

(defun head (uri &rest args
             &key version headers timeout max-redirects force-binary
               ssl-key-file ssl-cert-file ssl-key-password stream verbose)
  (declare (ignore version headers timeout max-redirects force-binary ssl-key-file ssl-cert-file ssl-key-password stream verbose))
  (apply #'request uri :method :head :use-connection-pool nil args))

(defun put (uri &rest args
            &key version headers content keep-alive use-connection-pool timeout force-binary
              ssl-key-file ssl-cert-file ssl-key-password stream verbose)
  (declare (ignore version headers content keep-alive use-connection-pool timeout force-binary ssl-key-file ssl-cert-file ssl-key-password stream verbose))
  (apply #'request uri :method :put args))

(defun delete (uri &rest args
               &key version headers keep-alive use-connection-pool timeout force-binary
                 ssl-key-file ssl-cert-file ssl-key-password stream verbose)
  (declare (ignore version headers keep-alive use-connection-pool timeout force-binary ssl-key-file ssl-cert-file ssl-key-password stream verbose))
  (apply #'request uri :method :delete args))
