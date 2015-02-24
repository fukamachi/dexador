(in-package :cl-user)
(defpackage dexador.util
  (:use :cl)
  (:export :defun-insane
           :defun-speedy
           :defun-careful
           :octets
           :ascii-string-to-octets
           :+crlf+
           :*default-user-agent*))
(in-package :dexador.util)

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
