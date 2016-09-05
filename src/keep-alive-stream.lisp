(in-package :cl-user)
(defpackage dexador.keep-alive-stream
  (:use :cl)
  (:import-from :trivial-gray-streams
                :fundamental-input-stream
                :stream-read-byte
                :stream-read-sequence
                :stream-element-type
                :open-stream-p)
  (:import-from :alexandria
                :xor)
  (:export :make-keep-alive-stream
           :keep-alive-stream
           :keep-alive-chunked-stream))
(in-package :dexador.keep-alive-stream)

(defclass keep-alive-stream (fundamental-input-stream)
  ((stream :type stream
           :initarg :stream
           :initform (error ":stream is required")
           :accessor keep-alive-stream-stream)
   (end :initarg :end
        :initform nil
        :accessor keep-alive-stream-end)))

(defclass keep-alive-chunked-stream (keep-alive-stream)
  ((state :type fixnum
          :initarg :state
          :initform -1)))

(defun make-keep-alive-stream (stream &key end chunked)
  (assert (xor end chunked))
  (if chunked
      (make-instance 'keep-alive-chunked-stream :stream stream)
      (make-instance 'keep-alive-stream :stream stream :end end)))

(defmethod stream-read-byte ((stream keep-alive-chunked-stream))
  (block nil
    (when (= (slot-value stream 'state) 3)
      (return :eof))

    (let ((byte (read-byte (keep-alive-stream-stream stream) nil nil)))
      (unless byte
        (return :eof))

      (with-slots (state) stream
        (ecase state
          (-1
           (when (= byte (char-code #\Return))
             (setf state 0)))
          ;; Read CR
          (0
           (if (= byte (char-code #\Newline))
               (setf state 1)
               (setf state -1)))
          ;; Read CRLF
          (1
           (if (= byte (char-code #\Return))
               (setf state 2)
               (setf state -1)))
          ;; Read CRLFCR
          (2
           (if (= byte (char-code #\Newline))
               (setf state 3)
               (setf state -1)))))

      (return byte))))

(defmethod stream-read-sequence ((stream keep-alive-stream) sequence start end &key)
  (declare (optimize speed))
  (let* ((to-read (min (- end start) (keep-alive-stream-end stream)))
         (n (read-sequence sequence (keep-alive-stream-stream stream)
                           :start start
                           :end (+ start to-read))))
    (decf (keep-alive-stream-end stream) (- n start))
    n))

(defmethod stream-read-sequence ((stream keep-alive-chunked-stream) sequence start end &key)
  (declare (optimize speed))
  (loop for i from start below end
        for byte = (read-byte stream nil nil)
        if byte
          do (setf (aref sequence i) byte)
        else
          do (return (max 0 (1- i)))
        finally (return i)))

(defmethod stream-element-type ((stream keep-alive-stream))
  '(unsigned-byte 8))

(defmethod open-stream-p ((stream keep-alive-stream))
  (open-stream-p (keep-alive-stream-stream stream)))

(defmethod close ((stream keep-alive-stream) &key abort)
  (with-slots (stream) stream
    (when (open-stream-p stream)
      (close stream :abort abort))))
