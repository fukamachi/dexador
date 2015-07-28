(in-package :cl-user)
(defpackage dexador.connection-cache
  (:use :cl)
  (:import-from :bordeaux-threads
                :current-thread)
  (:export :*connection-pool*
           :*use-connection-pool*
           :make-connection-pool
           :steal-connection
           :push-connection
           :clear-connection-pool))
(in-package :dexador.connection-cache)

(defparameter *connection-pool* nil)

(defvar *threads-connection-pool* nil)

(defvar *use-connection-pool* t)

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
  (when *use-connection-pool*
    (let* ((*connection-pool* (get-connection-pool))
           (conn (gethash host-port *connection-pool*)))
      (when conn
        (remhash host-port *connection-pool*)
        conn))))

(defun push-connection (host-port socket)
  (when *use-connection-pool*
    (let* ((*connection-pool* (get-connection-pool))
           (old-conn (gethash host-port *connection-pool*)))
      (when old-conn
        (ignore-errors (close old-conn)))
      (setf (gethash host-port *connection-pool*)
            socket))))

(defun clear-connection-pool ()
  (let ((pool (get-connection-pool)))
    (maphash (lambda (host-port conn)
               (ignore-errors (close conn))
               (remhash host-port pool))
             pool)
    t))

(initialize-threads-connection-pool)
