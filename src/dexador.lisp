(in-package :cl-user)
(defpackage dexador
  (:nicknames :dex)
  (:use :cl
        #-windows #:dexador.backend.usocket
        #+windows #:dexador.backend.winhttp)
  (:shadow :get
           :delete)
  (:import-from :dexador.connection-cache
                :*connection-pool*
                :*use-connection-pool*
                :make-connection-pool
                :clear-connection-pool)
  (:import-from :dexador.util
                :*default-connect-timeout*
                :*default-read-timeout*
                :*verbose*
                :*not-verify-ssl*)
  (:import-from :alexandria
                :copy-stream)
  (:export :request
           :get
           :post
           :head
           :put
           :patch
           :delete
           :fetch
           :*default-connect-timeout*
           :*default-read-timeout*
           :*verbose*
           :*not-verify-ssl*
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
            &key version headers basic-auth cookie-jar keep-alive use-connection-pool connect-timeout read-timeout max-redirects
              force-binary force-string want-stream
              ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy insecure ca-path)
  (declare (ignore version headers basic-auth cookie-jar keep-alive use-connection-pool connect-timeout read-timeout max-redirects force-binary force-string want-stream ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy insecure ca-path))
  (apply #'request uri :method :get args))

(defun post (uri &rest args
             &key version content headers basic-auth cookie-jar keep-alive use-connection-pool connect-timeout read-timeout
               force-binary force-string want-stream
               ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy insecure ca-path)
  (declare (ignore version content headers basic-auth cookie-jar keep-alive use-connection-pool connect-timeout read-timeout force-binary force-string want-stream ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy insecure ca-path))
  (apply #'request uri :method :post args))

(defun head (uri &rest args
             &key version headers basic-auth cookie-jar connect-timeout read-timeout max-redirects
               ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy insecure ca-path)
  (declare (ignore version headers basic-auth cookie-jar connect-timeout read-timeout max-redirects ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy insecure ca-path))
  (apply #'request uri :method :head :use-connection-pool nil args))

(defun put (uri &rest args
            &key version content headers basic-auth cookie-jar keep-alive use-connection-pool connect-timeout read-timeout
              force-binary force-string want-stream
              ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy insecure ca-path)
  (declare (ignore version content headers basic-auth cookie-jar keep-alive use-connection-pool connect-timeout read-timeout force-binary force-string want-stream ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy insecure ca-path))
  (apply #'request uri :method :put args))

(defun patch (uri &rest args
              &key version content headers basic-auth cookie-jar keep-alive use-connection-pool connect-timeout read-timeout
                force-binary force-string want-stream
                ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy insecure ca-path)
  (declare (ignore version content headers basic-auth cookie-jar keep-alive use-connection-pool connect-timeout read-timeout force-binary force-string want-stream ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy insecure ca-path))
  (apply #'request uri :method :patch args))

(defun delete (uri &rest args
               &key version headers basic-auth cookie-jar keep-alive use-connection-pool connect-timeout read-timeout
                 force-binary force-string want-stream
                 ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy insecure ca-path)
  (declare (ignore version headers basic-auth cookie-jar keep-alive use-connection-pool connect-timeout read-timeout force-binary force-string want-stream ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy insecure ca-path))
  (apply #'request uri :method :delete args))

(defun fetch (uri destination &key (if-exists :error) verbose proxy insecure)
  (unless (and (eql if-exists nil)
               (probe-file destination))
    (with-open-file (out destination
                         :direction :output :element-type '(unsigned-byte 8)
                         :if-exists if-exists
                         :if-does-not-exist :create)
      (let ((body (dex:get uri :want-stream t :force-binary t :verbose verbose :proxy proxy :insecure insecure)))
        (alexandria:copy-stream body out)))))

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
