(in-package :cl-user)
(uiop:define-package dexador
  (:nicknames :dex)
  (:use :cl)
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
                :*default-proxy*
                :*verbose*
                :*not-verify-ssl*)
  (:import-from :alexandria
                :copy-stream
                :remove-from-plist)
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
           :*default-proxy*
           :*verbose*
           :*not-verify-ssl*
           :*connection-pool*
           :*use-connection-pool*
           :make-connection-pool
           :clear-connection-pool

           :*dexador-backend*)
  (:use-reexport :dexador.restarts
                 :dexador.error))
(in-package :dexador)
	   
(defvar *dexador-backend*
  #+windows :winhttp
  #-windows :usocket)

(defun request (uri &rest args
                    &key method version
                         content headers
                         basic-auth bearer-auth
                         cookie-jar
                         connect-timeout read-timeout
                         keep-alive use-connection-pool
                         max-redirects
                         ssl-key-file ssl-cert-file ssl-key-password
                         stream verbose
                         force-binary
                         force-string
                         want-stream
                         proxy
                         insecure
                         ca-path)
  (declare (ignore method version
                   content headers
                   basic-auth bearer-auth
                   cookie-jar
                   connect-timeout read-timeout
                   keep-alive use-connection-pool
                   max-redirects
                   ssl-key-file ssl-cert-file ssl-key-password
                   stream verbose
                   force-binary
                   force-string
                   want-stream
                   proxy
                   insecure
                   ca-path))
  (ecase *dexador-backend*
    (:usocket (apply #'uiop:symbol-call '#:dexador.backend.usocket '#:request uri args))
    (:winhttp (apply #'uiop:symbol-call '#:dexador.backend.winhttp '#:request uri args))))

(defun get (uri &rest args
	    &key (version 1.1) headers basic-auth bearer-auth cookie-jar (keep-alive t)
	      (use-connection-pool dexador.connection-cache:*use-connection-pool*)
	      (connect-timeout dexador.util:*default-connect-timeout*)
	      (read-timeout dexador.util:*default-read-timeout*)
	      (max-redirects 5) force-binary force-string want-stream content
	      ssl-key-file ssl-cert-file ssl-key-password stream
	      (verbose dexador.util:*verbose*) (proxy dexador.util:*default-proxy*)
	      (insecure dexador.util:*not-verify-ssl*) ca-path)
  "Make a GET request to URI and return
    (values body-or-stream status response-headers uri &optional opaque-socket-stream)

  You may pass a real stream in as STREAM if you want us to communicate with the server via it --
  though if any errors occur, we will open a new connection to the server.  If you have a previous
  OPAQUE-SOCKET-STREAM you can pass that in as STREAM as well and we will re-use that connection.

  OPAQUE-SOCKET-STREAM is not returned if USE-CONNECTION-POOL is T, instead we keep track of it and
  re-use it when needed.

  If WANT-STREAM is T, then a STREAM is returned as the first value.  You may read this as needed to
  get the body of the response.  If KEEP-ALIVE and USE-CONNECTION-POOL are T, then the stream will be
  returned to the connection pool when you have read all the data or closed the stream. If KEEP-ALIVE
  is NIL then you are responsible for closing the stream when done.

  If KEEP-ALIVE is T and USE-CONNECTION-POOL is NIL, then the fifth value returned is a stream which
  you can then pass in again using the STREAM option to re-use the active connection.  If you ignore
  the stream, it will get closed during garbage collection.

  If KEEP-ALIVE is T and USE-CONNECTION-POOL is T, then there is no fifth
  value (OPAQUE-SOCKET-STREAM) returned, but the active connection to the host/port may be reused in
  subsequent calls.  This removes the need for the caller to keep track of the active socket-stream
  for subsequent calls.

  While CONTENT is allowed in a GET request the results are ill-defined and not advised."
  (declare (ignorable version headers basic-auth bearer-auth cookie-jar keep-alive use-connection-pool
		      connect-timeout read-timeout max-redirects force-binary force-string
		      want-stream ssl-key-file ssl-cert-file ssl-key-password stream
		      verbose proxy insecure ca-path content))
  (apply #'request uri :method :get args))

(defun post (uri &rest args
             &key (version 1.1) content headers basic-auth bearer-auth cookie-jar (keep-alive t)
	       (use-connection-pool dexador.connection-cache:*use-connection-pool*)
	       (connect-timeout dexador.util:*default-connect-timeout*)
	       (read-timeout dexador.util:*default-read-timeout*)
	       force-binary force-string want-stream ssl-key-file ssl-cert-file
	       ssl-key-password stream (verbose dexador.util:*verbose*)
	       (proxy dexador.util:*default-proxy*)
	       (insecure dexador.util:*not-verify-ssl*) ca-path)
  (declare (ignorable version content headers basic-auth bearer-auth cookie-jar keep-alive
		      use-connection-pool connect-timeout read-timeout force-binary
		      force-string want-stream ssl-key-file ssl-cert-file ssl-key-password
		      stream verbose proxy insecure ca-path))
  (apply #'request uri :method :post args))

(defun head (uri &rest args
             &key (version 1.1) headers basic-auth bearer-auth cookie-jar
	       (connect-timeout dexador.util:*default-connect-timeout*)
	       (read-timeout dexador.util:*default-read-timeout*)
	       (max-redirects 5)
	       ssl-key-file ssl-cert-file ssl-key-password stream
	       (verbose dexador.util:*verbose*)
	       (proxy dexador.util:*default-proxy*)
	       (insecure dexador.util:*not-verify-ssl*) ca-path)
  (declare (ignorable version headers basic-auth bearer-auth cookie-jar connect-timeout read-timeout
		      max-redirects ssl-key-file ssl-cert-file ssl-key-password stream
		      verbose proxy insecure ca-path))
  (apply #'request uri :method :head :use-connection-pool nil args))

(defun put (uri &rest args
            &key (version 1.1) content headers basic-auth bearer-auth cookie-jar (keep-alive t)
	      (use-connection-pool dexador.connection-cache:*use-connection-pool*)
	      (connect-timeout dexador.util:*default-connect-timeout*)
	      (read-timeout dexador.util:*default-read-timeout*)
	      force-binary force-string want-stream ssl-key-file ssl-cert-file
	      ssl-key-password stream
	      (verbose dexador.util:*verbose*)
	      (proxy dexador.util:*default-proxy*)
	      (insecure dexador.util:*not-verify-ssl*) ca-path)
  (declare (ignorable version content headers basic-auth bearer-auth cookie-jar keep-alive
		      use-connection-pool connect-timeout read-timeout force-binary
		      force-string want-stream ssl-key-file ssl-cert-file ssl-key-password
		      stream verbose proxy insecure ca-path))
  (apply #'request uri :method :put args))

(defun patch (uri &rest args
              &key (version 1.1) content headers basic-auth bearer-auth cookie-jar (keep-alive t)
		(use-connection-pool dexador.connection-cache:*use-connection-pool*)
		(connect-timeout dexador.util:*default-connect-timeout*)
		(read-timeout dexador.util:*default-read-timeout*)
		force-binary force-string want-stream ssl-key-file ssl-cert-file
		ssl-key-password stream
		(verbose dexador.util:*verbose*)
		(proxy dexador.util:*default-proxy*)
		(insecure dexador.util:*not-verify-ssl*) ca-path)
  (declare (ignorable version content headers basic-auth bearer-auth cookie-jar keep-alive
		      use-connection-pool connect-timeout read-timeout force-binary
		      force-string want-stream ssl-key-file ssl-cert-file
		      ssl-key-password stream verbose proxy insecure ca-path))
  (apply #'request uri :method :patch args))

(defun delete (uri &rest args
               &key (version 1.1) headers basic-auth bearer-auth cookie-jar (keep-alive t)
		 (use-connection-pool dexador.connection-cache:*use-connection-pool*)
		 (connect-timeout dexador.util:*default-connect-timeout*)
		 (read-timeout dexador.util:*default-read-timeout*)
		 force-binary force-string want-stream content
                 ssl-key-file ssl-cert-file ssl-key-password stream
		 (verbose dexador.util:*verbose*)
		 (proxy dexador.util:*default-proxy*)
		 (insecure dexador.util:*not-verify-ssl*) ca-path)
  (declare (ignorable version headers basic-auth bearer-auth cookie-jar keep-alive use-connection-pool
		      connect-timeout read-timeout force-binary force-string want-stream
		      ssl-key-file ssl-cert-file ssl-key-password stream verbose proxy
		      insecure ca-path content))
  (apply #'request uri :method :delete args))

(defun fetch (uri destination &rest args
              &key (if-exists :error)
                (version 1.1) headers basic-auth bearer-auth cookie-jar (keep-alive t)
		(use-connection-pool dexador.connection-cache:*use-connection-pool*)
		(connect-timeout dexador.util:*default-connect-timeout*)
		(read-timeout dexador.util:*default-read-timeout*)
		(max-redirects 5) ssl-key-file ssl-cert-file
		ssl-key-password stream
		(verbose dexador.util:*verbose*)
		(proxy dexador.util:*default-proxy*)
		(insecure dexador.util:*not-verify-ssl*) ca-path)
  (declare (ignorable version headers basic-auth bearer-auth cookie-jar keep-alive use-connection-pool
		      connect-timeout read-timeout max-redirects ssl-key-file ssl-cert-file
		      ssl-key-password stream verbose proxy insecure ca-path))
  (unless (and (eql if-exists nil)
               (probe-file destination))
    (with-open-file (out destination
                         :direction :output :element-type '(unsigned-byte 8)
                         :if-exists if-exists
                         :if-does-not-exist :create)
      (let ((body (apply #'dex:get uri :want-stream t :force-binary t
                         (remove-from-plist args :if-exists))))
        (alexandria:copy-stream body out)
        ;; Nominally the body gets closed, but if keep-alive is nil we need to explicitly do it.
        (when (open-stream-p body)
          (close body))))))
