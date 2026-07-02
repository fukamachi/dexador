(in-package :cl-user)
(defpackage dexador.util
  (:use :cl)
  (:import-from :fast-io
                :with-fast-output
                :fast-write-byte
                :fast-write-sequence)
  (:import-from :quri
                :uri-path
                :uri-query
                :uri-host
                :uri-port
                :uri-scheme
                :render-uri)
  (:import-from :usocket
                :ipv6-host-to-vector)
  (:export :*default-connect-timeout*
           :*default-read-timeout*
           :*verbose*
           :*default-proxy*
           :*no-proxy*
           :resolve-proxy
           :host-bypassed-p
           :strip-ipv6-brackets
           :*not-verify-ssl*
           :defun-speedy
           :defun-careful
           :octets
           :ascii-string-to-octets
           :+crlf+
           :*default-user-agent*
           :write-first-line
           :write-header
           :with-header-output
           :write-connect-header
           :make-random-string))
(in-package :dexador.util)

(defvar *default-connect-timeout* 10)
(defvar *default-read-timeout* 10)
(defvar *verbose* nil)
(defvar *not-verify-ssl* nil)

(defun getenv-nonempty (&rest names)
  (loop for name in names
        thereis (let ((v (uiop:getenv name)))
                  (and v (plusp (length v)) v))))

(defun environment-proxy ()
  #+windows nil
  #-windows
  (let ((https (getenv-nonempty "https_proxy" "HTTPS_PROXY"))
        (http  (getenv-nonempty "http_proxy" "HTTP_PROXY"))
        (all   (getenv-nonempty "all_proxy" "ALL_PROXY")))
    (remove nil (list (and https (cons "https" https))
                      (and http (cons "http" http))
                      (and all (cons "*" all))))))

(defvar *default-proxy* (environment-proxy)
  "Default proxy: NIL, a proxy URL string used for every scheme, or an alist mapping
\"http\"/\"https\"/\"scheme://host\"/\"*\" (or \"all\") to proxy URLs. Defaults from the
https_proxy / http_proxy environment variables with an all_proxy fallback. Honors
*NO-PROXY*. A proxy URL may carry credentials as user:pass@host and may use the
socks5:// scheme.")

(defvar *no-proxy* (getenv-nonempty "no_proxy" "NO_PROXY")
  "Hosts that bypass the proxy: a comma/space-separated string or a list of patterns.
\"*\" bypasses every host. An IP host is compared against IP and CIDR patterns
(e.g. \"10.0.0.0/8\", \"::1\"); any other host matches a pattern exactly or as a domain
suffix. Defaults from the no_proxy / NO_PROXY environment variable.")

(defun strip-ipv6-brackets (host)
  "Strip RFC 2732 brackets: \"[::1]\" -> \"::1\"."
  (if (and (plusp (length host))
           (char= (char host 0) #\[))
      (let ((close (position #\] host)))
        (if close
            (subseq host 1 close)
            host))
      host))

(defun parse-ip-address (string)
  "Parse an IPv4 or IPv6 literal into (VALUES address-integer total-bits),
or NIL when STRING is not an IP literal."
  (let ((string (strip-ipv6-brackets string)))
    (if (find #\: string)
        (let ((bytes (ignore-errors (ipv6-host-to-vector string))))
          (when bytes
            (values (reduce (lambda (acc byte) (logior (ash acc 8) byte))
                            bytes :initial-value 0)
                    128)))
        (let ((parts (uiop:split-string string :separator ".")))
          (when (= (length parts) 4)
            (loop with address = 0
                  for part in parts
                  for byte = (and (<= 1 (length part) 3)
                                  (every #'digit-char-p part)
                                  (parse-integer part))
                  unless (and byte (<= byte 255))
                    do (return nil)
                  do (setf address (logior (ash address 8) byte))
                  finally (return (values address 32))))))))

(defun ip-in-network-p (address total-bits pattern)
  "Test an IP against a CIDR network like \"10.0.0.0/8\" or \"2001:db8::/32\"."
  (let ((slash (position #\/ pattern)))
    (when slash
      (multiple-value-bind (network network-bits)
          (parse-ip-address (subseq pattern 0 slash))
        (let ((prefix (ignore-errors (parse-integer (subseq pattern (1+ slash))))))
          (and network
               (eql total-bits network-bits)
               (integerp prefix)
               (<= 0 prefix network-bits)
               (= (ash address (- prefix network-bits))
                  (ash network (- prefix network-bits)))))))))

(defun ip-matches-pattern-p (address total-bits pattern)
  "Test an IP against a CIDR network or an IP literal (compared numerically,
so \"::1\" matches \"0:0:0:0:0:0:0:1\"). A :port suffix in PATTERN is ignored."
  (if (find #\/ pattern)
      (ip-in-network-p address total-bits pattern)
      (multiple-value-bind (pattern-address pattern-bits)
          (parse-ip-address
           (let ((colon (position #\: pattern)))
             (if (and colon (not (find #\: pattern :start (1+ colon))))
                 (subseq pattern 0 colon) ; single colon: an IPv4 :port suffix
                 pattern)))
        (and pattern-address
             (eql total-bits pattern-bits)
             (= address pattern-address)))))

(defun hostname-matches-pattern-p (host pattern)
  "Test a hostname against a NO_PROXY pattern: exact match or domain suffix.
A leading dot and any :port in PATTERN are ignored."
  (let* ((dotless (string-left-trim "." pattern))
         (pattern (subseq dotless 0 (position #\: dotless))))
    (or (string-equal host pattern)
        (let ((pl (length pattern))
              (hl (length host)))
          (and (plusp pl)
               (> hl pl)
               (char= (char host (- hl pl 1)) #\.)
               (string-equal pattern (subseq host (- hl pl))))))))

(defun host-bypassed-p (host no-proxy)
  "True if HOST should bypass the proxy according to NO-PROXY (NO_PROXY semantics).
NO-PROXY is a comma/space-separated string or a list of patterns. \"*\" bypasses
everything. An IP host matches IP/CIDR patterns; a hostname matches exactly or as a
domain suffix."
  (when (and host no-proxy)
    (let ((host (strip-ipv6-brackets host))
          (patterns (if (listp no-proxy)
                        no-proxy
                        (remove "" (mapcar (lambda (s) (string-trim '(#\Space #\Tab) s))
                                           (uiop:split-string no-proxy :separator ", "))
                                :test #'string=))))
      (multiple-value-bind (address total-bits) (parse-ip-address host)
        (some (lambda (pattern)
                (and (plusp (length pattern))
                     (or (string= pattern "*")
                         (if address
                             (ip-matches-pattern-p address total-bits pattern)
                             (hostname-matches-pattern-p host pattern)))))
              patterns)))))

(defun normalize-proxy (proxy)
  "Normalize PROXY to the alist form: a URL string becomes ((\"*\" . url))."
  (etypecase proxy
    (null nil)
    (string (list (cons "*" proxy)))
    (list proxy)))

(defun proxy-for-uri (uri proxy)
  "Select the proxy URL for URI from the scheme/host alist PROXY.
The most specific key wins: \"scheme://host\", then scheme, then \"*\"/\"all\"."
  (let* ((scheme (uri-scheme uri))
         (host (uri-host uri))
         (host-key (and scheme host (format nil "~A://~A" scheme host))))
    (cdr (or (and host-key (assoc host-key proxy :test #'string-equal))
             (and scheme (assoc scheme proxy :test #'string-equal))
             (assoc "*" proxy :test #'string-equal)
             (assoc "all" proxy :test #'string-equal)))))

(defun resolve-proxy (uri proxy &optional (no-proxy *no-proxy*))
  "Return the effective proxy URL string for URI, or NIL.
PROXY is NIL, a URL string, or a scheme/host alist (see *DEFAULT-PROXY*). Returns NIL when
URI's host matches NO-PROXY."
  (let ((u (quri:uri uri)))
    (unless (host-bypassed-p (uri-host u) no-proxy)
      (proxy-for-uri u (normalize-proxy proxy)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *speedy-declaration* '(declare (optimize (speed 3) (safety 0) (space 0) (compilation-speed 0))))
  (defvar *careful-declaration* '(declare (optimize (speed 3) (safety 2)))))

(defmacro defun-speedy (name lambda-list &body body)
  `(progn
     (declaim (notinline ,name))
     (defun ,name ,lambda-list
       ,*speedy-declaration*
       ,@body)))

(defmacro defun-careful (name lambda-list &body body)
  `(progn
     (declaim (notinline ,name))
     (defun ,name ,lambda-list
       ,*careful-declaration*
       ,@body)))

(deftype octets (&optional (len '*)) `(simple-array (unsigned-byte 8) (,len)))

(declaim (ftype (function (simple-string) octets) ascii-string-to-octets))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun-speedy %ascii-string-to-octets (string)
    (let ((result (make-array (length string) :element-type '(unsigned-byte 8))))
      (declare (type octets result))
      (dotimes (i (length string) result)
        (declare (type fixnum i))
        (setf (aref result i)
              (char-code (aref string i))))))

  (defun-speedy ascii-string-to-octets (string)
    (%ascii-string-to-octets string))

  (define-compiler-macro ascii-string-to-octets (&whole form string)
    (if (constantp string)
        (%ascii-string-to-octets string)
        form))

  (declaim (type octets +crlf+))
  (defvar +crlf+ (ascii-string-to-octets (format nil "~C~C" #\Return #\Newline))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *dexador-version*
    (asdf:component-version (asdf:find-system :dexador)))

  (defparameter *default-user-agent*
    (format nil "Dexador/~A (~A~@[ ~A~]); ~A;~@[ ~A~]"
            *dexador-version*
            (or (lisp-implementation-type) "Common Lisp")
            (or (lisp-implementation-version) "")
            (or #-clisp (software-type)
                #+(or win32 mswindows) "Windows"
                #-(or win32 mswindows) "Unix")
            (or #-clisp (software-version)))))

(defparameter *header-buffer* nil)

(defun write-first-line (method uri version &optional (buffer *header-buffer*))
  (fast-write-sequence (ascii-string-to-octets (string method)) buffer)
  (fast-write-byte #.(char-code #\Space) buffer)
  (fast-write-sequence (ascii-string-to-octets
                         (format nil "~A~:[~;~:*?~A~]"
                                 (or (uri-path uri) "/")
                                 (uri-query uri)))
                       buffer)
  (fast-write-byte #.(char-code #\Space) buffer)
  (fast-write-sequence (ecase version
                         (1.1 (ascii-string-to-octets "HTTP/1.1"))
                         (1.0 (ascii-string-to-octets "HTTP/1.0")))
                       buffer)
  (fast-write-sequence +crlf+ buffer))

(defun write-header-field (name buffer)
  (fast-write-sequence (if (typep name 'octets)
                           name
                           (ascii-string-to-octets (string-capitalize name)))
                       buffer))

(defun write-header-value (value buffer)
  (fast-write-sequence (if (typep value 'octets)
                           value
                           (ascii-string-to-octets (princ-to-string value)))
                       buffer))

(defun write-header (name value &optional (buffer *header-buffer*))
  (write-header-field name buffer)
  (fast-write-sequence (ascii-string-to-octets ": ") buffer)
  (write-header-value value buffer)
  (fast-write-sequence +crlf+ buffer))

(define-compiler-macro write-header (name value &optional (buffer '*header-buffer*))
  `(progn
     ,(if (and (constantp name)
               (typep name '(or keyword string)))
          `(fast-write-sequence (ascii-string-to-octets ,(string-capitalize name)) ,buffer)
          `(write-header-field ,name ,buffer))
     (fast-write-sequence (ascii-string-to-octets ": ") ,buffer)
     ,(if (constantp value)
          `(fast-write-sequence (ascii-string-to-octets ,(string value)) ,buffer)
          `(write-header-value ,value ,buffer))
     (fast-write-sequence +crlf+ ,buffer)))

(defmacro with-header-output ((buffer &optional output) &body body)
  `(with-fast-output (,buffer ,output)
     (declare (ignorable ,buffer))
     (let ((*header-buffer* ,buffer))
       ,@body)))

(defun write-connect-header (uri version buffer &optional proxy-auth)
  (fast-write-sequence (ascii-string-to-octets "CONNECT") buffer)
  (fast-write-byte #.(char-code #\Space) buffer)
  (fast-write-sequence (ascii-string-to-octets (format nil "~A:~A"
                                                       (uri-host uri)
                                                       (uri-port uri)))
                       buffer)
  (fast-write-byte #.(char-code #\Space) buffer)
  (fast-write-sequence (ecase version
                         (1.1 (ascii-string-to-octets "HTTP/1.1"))
                         (1.0 (ascii-string-to-octets "HTTP/1.0")))
                       buffer)
  (fast-write-sequence +crlf+ buffer)
  (fast-write-sequence (ascii-string-to-octets "Host:") buffer)
  (fast-write-byte #.(char-code #\Space) buffer)
  (fast-write-sequence (ascii-string-to-octets (format nil "~A:~A"
                                                       (uri-host uri)
                                                       (uri-port uri)))
                       buffer)
  (when proxy-auth
    (fast-write-sequence +crlf+ buffer)
    (fast-write-sequence (ascii-string-to-octets "Proxy-Authorization:") buffer)
    (fast-write-byte #.(char-code #\Space) buffer)
    (fast-write-sequence (ascii-string-to-octets proxy-auth) buffer))
  (fast-write-sequence +crlf+ buffer)
  (fast-write-sequence +crlf+ buffer))

(defun-speedy make-random-string (&optional (length 12))
  (declare (type fixnum length))
  (let ((result (make-string length)))
    (declare (type simple-string result))
    (dotimes (i length result)
      (setf (aref result i)
            (ecase (random 5)
              ((0 1) (code-char (+ #.(char-code #\a) (random 26))))
              ((2 3) (code-char (+ #.(char-code #\A) (random 26))))
              ((4) (code-char (+ #.(char-code #\0) (random 10)))))))))
