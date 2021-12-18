(in-package :cl-user)
(defpackage dexador.connection-cache
  (:use :cl)
  (:import-from :bordeaux-threads
                :make-lock
                :with-lock-held)
  (:export :*connection-pool*
           :*use-connection-pool*
           :make-connection-pool
           :steal-connection
           :push-connection
           :clear-connection-pool))
(in-package :dexador.connection-cache)

(defvar *use-connection-pool* t)

(defstruct lru-pool-elt
  (prev nil :type (or null lru-pool-elt))
  (next nil :type (or null lru-pool-elt))
  (elt nil :type t)
  (key nil :type t)
  (eviction-callback nil :type (or null function)))

;; An LRU-POOL can have multiple entries for the same key
(defstruct lru-pool
  (lock #+thread-support (bt:make-lock "connection pool lock")
        #-thread-support nil)
  (hash-table nil :type (or null hash-table)) ;; hash table entries are lists of elements
  (head nil :type (or null lru-pool-elt)) ;; most recently used is here and it's a doubly-linked-list
  (tail nil :type (or null lru-pool-elt)) ;; least recently used is here
  (num-elts 0 :type fixnum)
  (max-elts 8 :type fixnum))

(defun make-connection-pool (&optional (max-active-connections 8))
  (make-lru-pool :hash-table (make-hash-table :test 'equal) :max-elts max-active-connections))

(defparameter *connection-pool* (make-connection-pool))

(defun get-from-lru-pool (lru-pool key)
  "Must be called with lru-cache-lock held.  Removes element from pool."
  (let* ((hash-table (lru-pool-hash-table lru-pool))
         (possible-elts (gethash key (lru-pool-hash-table lru-pool))))
    (when possible-elts
      (let ((remaining-elts (cdr possible-elts)))
        (if remaining-elts
            (setf (gethash key hash-table) remaining-elts)
            (remhash key hash-table)))
      (let ((elt (car possible-elts)))
        (let ((prev (lru-pool-elt-prev elt))
              (next (lru-pool-elt-next elt)))
          (if prev
              (setf (lru-pool-elt-next prev) next)
              (setf (lru-pool-head lru-pool) next))
          (if next
              (setf (lru-pool-elt-prev next) prev)
              (setf (lru-pool-tail lru-pool) prev)))
        (decf (lru-pool-num-elts lru-pool))
        (lru-pool-elt-elt elt)))))

(defun evict-tail (lru-pool)
  "Must be called with lru-pool-lock held.  Returns the eviction callback which you
   must call (after releasing the lru-pool-lock)."
  ;; slightly different from get-from-lru-pool because we want to get rid of the
  ;; actual oldest element (one could in principle call get-from-lru-pool on
  ;; (lru-pool-elt-key (lru-pool-tail lru-pool)) if you didn't care
  (let* ((tail (lru-pool-tail lru-pool)))
    (when tail
      (let ((prev (lru-pool-elt-prev tail)))
        (if prev
            (setf (lru-pool-elt-next prev) nil)
            (setf (lru-pool-head lru-pool) nil))
        (setf (lru-pool-tail lru-pool) prev)
        (let* ((hash-table (lru-pool-hash-table lru-pool))
               (key (lru-pool-elt-key tail))
               (remaining (delete tail (gethash key hash-table))))
          (if remaining
              (setf (gethash key hash-table) remaining)
              (remhash key hash-table))))
      (decf (lru-pool-num-elts lru-pool))
      (values (lru-pool-elt-eviction-callback tail) t))))

(defun add-to-lru-pool (lru-pool key elt eviction-callback)
  "Must be called with lru-pool-lock held.  Adds elt as most recently used.
   If an element was evicted, returns (values eviction-callback t) otherwise nil"
  (declare (type lru-pool lru-pool))
  (let* ((old-head (lru-pool-head lru-pool))
         (lru-pool-elt (make-lru-pool-elt :prev nil :next old-head :elt elt :key key :eviction-callback eviction-callback))
         (hash-table (lru-pool-hash-table lru-pool)))
    (setf (lru-pool-head lru-pool) lru-pool-elt)
    (push lru-pool-elt (gethash key hash-table))
    (when old-head
      (setf (lru-pool-elt-prev old-head) lru-pool-elt))
    (unless (lru-pool-tail lru-pool)
      (setf (lru-pool-tail lru-pool) lru-pool-elt))
    (when (> (incf (lru-pool-num-elts lru-pool)) (lru-pool-max-elts lru-pool))
      (evict-tail lru-pool))))

(defmethod print-object ((obj lru-pool-elt) str) ;; avoid printing loops
  (format str "<LRU-POOL-ELT ~A NEXT ~A>" (lru-pool-elt-key obj) (lru-pool-elt-next obj)))

(defmethod print-object ((obj lru-pool) str) ;; avoid printing loops
  (let (objs)
    (loop with lru-pool-elt = (lru-pool-head obj)
          while lru-pool-elt
          do (push (cons (lru-pool-elt-key lru-pool-elt) (lru-pool-elt-elt lru-pool-elt)) objs)
          do (setf lru-pool-elt (lru-pool-elt-next lru-pool-elt)))
    (format str "<LRU-POOL ~A/~A elts:~%~{ ~A~^~%~}>" (lru-pool-num-elts obj) (lru-pool-max-elts obj) objs)))

(defmacro with-lock (lock &body body)
  (declare (ignorable lock))
  #+thread-support `(bt:with-lock-held (,lock)
                      ,@body)
  #-thread-support `(progn ,@body))

(defun push-connection (host-port socket &optional eviction-callback)
  "Add socket back to connection pool"
  (when *use-connection-pool*
    (let ((pool *connection-pool*))
      (let ((evicted-elt-eviction-callback
              (with-lock (lru-pool-lock pool)
                (add-to-lru-pool pool host-port socket eviction-callback))))
        (and evicted-elt-eviction-callback (funcall evicted-elt-eviction-callback))
        (values)))))

(defun steal-connection (host-port)
  (when *use-connection-pool*
    (let ((pool *connection-pool*))
      (with-lock (lru-pool-lock pool)
        (get-from-lru-pool pool host-port)))))

(defun clear-connection-pool ()
  (when *use-connection-pool*
    (let ((pool *connection-pool*)
          eviction-callback element-was-evicted)
      (loop for count from 0
            do (setf (values eviction-callback element-was-evicted) (evict-tail pool))
            do (when eviction-callback (funcall eviction-callback))
            do (format t "Evicted ~A elts so far~%" count)
            while element-was-evicted))))

            
            
