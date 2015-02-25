(in-package :cl-user)
(defpackage dexador.util
  (:use :cl)
  (:import-from :fast-io
                :with-fast-output
                :fast-write-byte
                :fast-write-sequence)
  (:import-from :quri
                :uri-path
                :uri-query)
  (:export :*default-timeout*
           :defun-insane
           :defun-speedy
           :defun-careful
           :octets
           :ascii-string-to-octets
           :+crlf+
           :*default-user-agent*
           :write-first-line
           :write-header
           :with-header-output))
(in-package :dexador.util)

(defvar *default-timeout* 10)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *insane-declaration* '(declare (optimize (speed 3) (safety 0) (space 0) (compilation-speed 0))))
  (defvar *speedy-declaration* '(declare (optimize (speed 3) (safety 0) (space 0) (compilation-speed 0))))
  (defvar *careful-declaration* '(declare (optimize (speed 3) (safety 2)))))

(defmacro defun-insane (name lambda-list &body body)
  `(progn
     (declaim (inline ,name))
     (defun ,name ,lambda-list
       ,*insane-declaration*
       ,@body)))

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
  (defun-speedy ascii-string-to-octets (string)
    (let ((result (make-array (length string) :element-type '(unsigned-byte 8))))
      (declare (type octets result))
      (dotimes (i (length string) result)
        (declare (type fixnum i))
        (setf (aref result i)
              (char-code (aref string i))))))

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
  (fast-write-sequence (ascii-string-to-octets (format nil "~A~:[~;~:*?~A~]"
                                                       (or (uri-path uri) "/")
                                                       (uri-query uri)))
                       buffer)
  (fast-write-byte #.(char-code #\Space) buffer)
  (fast-write-sequence (ecase version
                         (1.1 #.(ascii-string-to-octets "HTTP/1.1"))
                         (1.0 #.(ascii-string-to-octets "HTTP/1.0")))
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
  (fast-write-sequence #.(ascii-string-to-octets ": ") buffer)
  (write-header-value value buffer)
  (fast-write-sequence +crlf+ buffer))

(define-compiler-macro write-header (name value &optional (buffer '*header-buffer*))
  `(progn
     ,(if (and (constantp name)
               (typep name '(or keyword string)))
          `(fast-write-sequence ,(ascii-string-to-octets (string-capitalize name)) ,buffer)
          `(write-header-field ,name ,buffer))
     (fast-write-sequence ,(ascii-string-to-octets ": ") ,buffer)
     ,(if (constantp value)
          `(fast-write-sequence ,(ascii-string-to-octets (string value)) ,buffer)
          `(write-header-value ,value ,buffer))
     (fast-write-sequence +crlf+ ,buffer)))

(defmacro with-header-output ((buffer &optional output) &body body)
  `(with-fast-output (,buffer ,output)
     (declare (ignorable ,buffer))
     (let ((*header-buffer* ,buffer))
       ,@body)))
