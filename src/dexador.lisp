(in-package :cl-user)
(defpackage dexador
  (:nicknames :dex)
  (:use :cl)
  (:shadow :get
           :delete)
  (:import-from :dexador.backend.usocket
                :request
                :retry-request
                :ignore-and-continue)
  (:import-from :dexador.connection-cache
                :*connection-pool*
                :*use-connection-pool*
                :make-connection-pool
                :clear-connection-pool)
  (:import-from :dexador.util
                :*default-timeout*)
  (:import-from :alexandria
                :copy-stream)
  (:export :request
           :get
           :post
           :head
           :put
           :delete
           :fetch
           :*default-timeout*
           :*connection-pool*
           :*use-connection-pool*
           :make-connection-pool
           :clear-connection-pool

           ;; Restarts
           :retry-request
           :ignore-and-continue))
(in-package :dexador)

(cl-reexport:reexport-from :dexador.error)

(defun get (uri &rest args
            &key version headers basic-auth cookie-jar keep-alive use-connection-pool timeout max-redirects
              force-binary want-stream
              ssl-key-file ssl-cert-file ssl-key-password stream verbose)
  (declare (ignore version headers basic-auth cookie-jar keep-alive use-connection-pool timeout max-redirects force-binary want-stream ssl-key-file ssl-cert-file ssl-key-password stream verbose))
  (apply #'request uri :method :get args))

(defun post (uri &rest args
             &key version content headers basic-auth cookie-jar keep-alive use-connection-pool timeout
               force-binary want-stream
               ssl-key-file ssl-cert-file ssl-key-password stream verbose)
  (declare (ignore version content headers basic-auth cookie-jar keep-alive use-connection-pool timeout force-binary want-stream ssl-key-file ssl-cert-file ssl-key-password stream verbose))
  (apply #'request uri :method :post args))

(defun head (uri &rest args
             &key version headers basic-auth cookie-jar timeout max-redirects
               ssl-key-file ssl-cert-file ssl-key-password stream verbose)
  (declare (ignore version headers basic-auth cookie-jar timeout max-redirects ssl-key-file ssl-cert-file ssl-key-password stream verbose))
  (apply #'request uri :method :head :use-connection-pool nil args))

(defun put (uri &rest args
            &key version content headers basic-auth cookie-jar keep-alive use-connection-pool timeout
              force-binary want-stream
              ssl-key-file ssl-cert-file ssl-key-password stream verbose)
  (declare (ignore version content headers basic-auth cookie-jar keep-alive use-connection-pool timeout force-binary want-stream ssl-key-file ssl-cert-file ssl-key-password stream verbose))
  (apply #'request uri :method :put args))

(defun delete (uri &rest args
               &key version headers basic-auth cookie-jar keep-alive use-connection-pool timeout
                 force-binary want-stream
                 ssl-key-file ssl-cert-file ssl-key-password stream verbose)
  (declare (ignore version headers basic-auth cookie-jar keep-alive use-connection-pool timeout force-binary want-stream ssl-key-file ssl-cert-file ssl-key-password stream verbose))
  (apply #'request uri :method :delete args))

(defun fetch (uri destination &key (if-exists :error))
  (with-open-file (out destination
                       :direction :output :element-type '(unsigned-byte 8)
                       :if-exists if-exists
                       :if-does-not-exist :create)
    (multiple-value-bind (body status headers)
        (dex:get uri :want-stream t :force-binary t)
      (declare (ignore status))
      (alexandria:copy-stream body out
                              :end (gethash "content-length" headers)))))

(defun ignore-and-continue (e)
  (let ((restart (find-restart 'ignore-and-continue e)))
    (when restart
      (invoke-restart restart))))

(defun retry-request (times &key (interval 3))
  (declare (type (or function integer) interval))
  (etypecase times
    (condition
     (let ((restart (find-restart 'retry-request times)))
       (when restart
         (invoke-restart restart))))
    (integer
     (retry-request-ntimes times :interval interval))))

(defun retry-request-ntimes (n &key (interval 3))
  (declare (type integer n)
           (type (or function integer) interval))
  (let ((retries 0))
    (declare (type integer retries))
    (lambda (e)
      (declare (type condition e))
      (let ((restart (find-restart 'retry-request e)))
        (when restart
          (when (< retries n)
            (incf retries)
            (etypecase interval
              (function (funcall interval retries))
              (integer (sleep interval)))
            (invoke-restart restart)))))))
