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

(defun get (uri &rest args
            &key version headers keep-alive timeout max-redirects force-binary
              ssl-key-file ssl-cert-file ssl-key-password socket verbose)
  (declare (ignore version headers keep-alive timeout max-redirects force-binary ssl-key-file ssl-cert-file ssl-key-password socket verbose))
  (apply #'request uri :method :get args))

(defun post (uri &rest args
             &key version headers content keep-alive timeout force-binary
               ssl-key-file ssl-cert-file ssl-key-password socket verbose)
  (declare (ignore version headers content keep-alive timeout force-binary ssl-key-file ssl-cert-file ssl-key-password socket verbose))
  (apply #'request uri :method :post args))

(defun head (uri &rest args
             &key version headers timeout max-redirects force-binary
               ssl-key-file ssl-cert-file ssl-key-password socket verbose)
  (declare (ignore version headers timeout max-redirects force-binary ssl-key-file ssl-cert-file ssl-key-password socket verbose))
  (apply #'request uri :method :head args))

(defun put (uri &rest args
            &key version headers content keep-alive timeout force-binary
              ssl-key-file ssl-cert-file ssl-key-password socket verbose)
  (declare (ignore version headers content keep-alive timeout force-binary ssl-key-file ssl-cert-file ssl-key-password socket verbose))
  (apply #'request uri :method :put args))

(defun delete (uri &rest args
               &key version headers keep-alive timeout force-binary
                 ssl-key-file ssl-cert-file ssl-key-password socket verbose)
  (declare (ignore version headers keep-alive timeout force-binary ssl-key-file ssl-cert-file ssl-key-password socket verbose))
  (apply #'request uri :method :delete args))
