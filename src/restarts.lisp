(defpackage dexador.restarts
  (:use :cl)
  (:export :retry-request
           :ignore-and-continue))
(in-package :dexador.restarts)

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
