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
           :keep-alive-chunked-stream
   :keep-alive-stream-close-underlying-stream
   :keep-alive-stream-stream))
(in-package :dexador.keep-alive-stream)

(defclass keep-alive-stream (fundamental-input-stream)
  ((stream :type stream
           :initarg :stream
           :initform (error ":stream is required")
           :accessor keep-alive-stream-stream
           :documentation "A stream; when we read END elements from it, we call CLOSE-ACTION on it and
   set this slot to nil.")
   (end :initarg :end
        :initform nil
        :accessor keep-alive-stream-end)
   (close-action :initarg :on-close-or-eof :reader close-action
                 :documentation "A (lambda (stream abort)) which will be called with keep-alive-stream-stream
   when the stream is either closed or we hit end of file or we hit end")))

(defun keep-alive-stream-close-underlying-stream (underlying-stream abort)
  (when (and underlying-stream (open-stream-p underlying-stream))
    (close underlying-stream :abort abort)))

(defclass keep-alive-chunked-stream (keep-alive-stream)
  ((state :type fixnum
          :initarg :state
          :initform -1)))

(defun make-keep-alive-stream (stream &key end chunked (on-close-or-eof #'keep-alive-stream-close-underlying-stream))
  "ON-CLOSE-OR-EOF takes a single parameter, STREAM (the stream passed in here, not the
keep-alive-stream), and should handle clean-up of it"
  (assert (xor end chunked))
  (format t "CHUNKED IS ~A~%" chunked)
  (if chunked
      (make-instance 'keep-alive-chunked-stream :stream stream :on-close-or-eof on-close-or-eof)
      (make-instance 'keep-alive-stream :stream stream :end end :on-close-or-eof on-close-or-eof)))

(defmethod stream-read-byte ((stream keep-alive-stream))
  "Return :EOF or byte read.  When we hit EOF or finish reading our allowed content,
   call the close-action on our underlying-stream and return EOF."
  (let ((byte :eof)
        (underlying-stream (keep-alive-stream-stream stream)))
    (labels ((maybe-close-and-return-eof (&optional (force-close nil))
               (cond
                 ((not underlying-stream)
                  (return-from stream-read-byte :eof))
                 ((or (<= (keep-alive-stream-end stream) 0) force-close)
                  (funcall (close-action stream) underlying-stream nil)
                  (setf (keep-alive-stream-stream stream) nil)
                  (return-from stream-read-byte :eof)))))
      (maybe-close-and-return-eof)
      (setf byte (read-byte underlying-stream nil :eof))
      (decf (keep-alive-stream-end stream) 1)
      (maybe-close-and-return-eof (eql byte :eof))
      byte)))

(defmethod stream-read-byte ((stream keep-alive-chunked-stream))
  "Return :EOF or byte read.  When we hit :EOF or finish reading our chunk,
   call the close-action on our underlying-stream and return :EOF"
  (let ((byte :eof)
        (underlying-stream (keep-alive-stream-stream stream)))
    (with-slots (state) stream
      (labels ((maybe-close-and-return (&optional (force-close nil))
                 (cond
                   ((not underlying-stream)
                    (return-from stream-read-byte :eof))
                   ((or force-close (= state 3))
                    (funcall (close-action stream) underlying-stream nil)
                    (setf (keep-alive-stream-stream stream) nil)
                    (return-from stream-read-byte byte)))))
        (maybe-close-and-return)
        (setf byte (read-byte (keep-alive-stream-stream stream) nil :eof))
        (maybe-close-and-return (eql byte :eof))
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
               (setf state -1))))
        (maybe-close-and-return)
        byte))))

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
  (let ((underlying-stream (keep-alive-stream-stream stream)))
    (and underlying-stream (open-stream-p underlying-stream))))

(defmethod close ((stream keep-alive-stream) &key abort)
  (funcall (close-action stream) (keep-alive-stream-stream stream) abort))
