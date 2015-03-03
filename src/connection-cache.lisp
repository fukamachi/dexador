(in-package :cl-user)
(defpackage dexador.connection-cache
  (:use :cl)
  (:import-from :bordeaux-threads
                :current-thread)
  (:export :*connection-pool*
           :*reuse-interval*
           :make-connection-pool
           :steal-connection
           :push-connection))
(in-package :dexador.connection-cache)

(defparameter *connection-pool* nil)

(defvar *threads-connection-pool* nil)

(defvar *reuse-interval* 5)

(defun make-connection-pool ()
  (make-hash-table :test 'equal))

#+thread-support
(defun make-threads-connection-pool ()
  (let ((pool (make-hash-table :test 'eq)))
    (setf (gethash (bt:current-thread) pool)
          (make-connection-pool))
    pool))
#-thread-support
(defun make-threads-connection-pool ()
  (make-connection-pool))

(defun initialize-threads-connection-pool ()
  (setf *threads-connection-pool* (make-threads-connection-pool)))

#+thread-support
(defun get-connection-pool ()
  (or *connection-pool*
      (gethash (bt:current-thread) *threads-connection-pool*)
      (setf (gethash (bt:current-thread) *threads-connection-pool*)
            (make-connection-pool))))
#-thread-support
(defun get-connection-pool ()
  (or *connection-pool*
      *threads-connection-pool*))

(defun steal-connection (host-port)
  (let* ((*connection-pool* (get-connection-pool))
         (conn (gethash host-port *connection-pool*)))
    (when conn
      (remhash host-port *connection-pool*)
      (if (<= (- (car conn) (get-universal-time)) *reuse-interval*)
          (cdr conn)
          nil))))

(defun push-connection (host-port socket)
  (let ((*connection-pool* (get-connection-pool)))
    (setf (gethash host-port *connection-pool*)
          (cons (get-universal-time)
                socket))))

(initialize-threads-connection-pool)
