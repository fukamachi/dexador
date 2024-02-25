(defpackage #:dexador.body
  (:use #:cl)
  (:import-from #:dexador.encoding
                #:detect-charset)
  (:import-from #:dexador.decoding-stream
                #:make-decoding-stream)
  (:import-from #:dexador.util
                #:ascii-string-to-octets
                #:+crlf+
                #:octets)
  (:import-from #:alexandria #:copy-stream #:assoc-value)
  (:import-from #:babel
                #:octets-to-string
                #:character-decoding-error)
  (:import-from #:babel-encodings
                #:*suppress-character-coding-errors*)
  (:import-from :trivial-mimes
                :mime)
  (:import-from #:quri
                #:url-encode)
  (:import-from #:chipz
                #:make-decompressing-stream
                #:decompress
                #:make-dstate)
  (:export #:decode-body
           #:write-multipart-content
           #:multipart-value-content-type
           #:decompress-body
           #:write-as-octets
           #:multipart-content-length
           #:content-length
           #:with-content-caches
           #:content-type))
(in-package #:dexador.body)

(defun decode-body (content-type body &key default-charset on-close)
  (let ((charset (or (and content-type
                          (detect-charset content-type body))
                     default-charset))
        (babel-encodings:*suppress-character-coding-errors* t))
    (if charset
        (handler-case
            (if (streamp body)
                (make-decoding-stream body :encoding charset :on-close on-close)
                (babel:octets-to-string body :encoding charset))
          (babel:character-decoding-error (e)
            (warn (format nil "Failed to decode the body to ~S due to the following error (falling back to binary):~%  ~A"
                          charset
                          e))
            (return-from decode-body body)))
        body)))

(defun content-disposition (key val)
  (typecase val
    (cons (content-disposition key (first val)))
    (pathname
     (let* ((filename (file-namestring val))
            (utf8-filename-p (find-if (lambda (char)
                                        (< 127 (char-code char)))
                                      filename)))
       (format nil "Content-Disposition: form-data; name=\"~A\"; ~:[filename=\"~A\"~;filename*=UTF-8''~A~]~C~C"
               key
               utf8-filename-p
               (if utf8-filename-p
                   (url-encode filename :encoding :utf-8)
                   filename)
               #\Return #\Newline)))
    (otherwise
      (format nil "Content-Disposition: form-data; name=\"~A\"~C~C"
              key
              #\Return #\Newline))))

(defmacro define-alist-cache (cache-name)
  (let ((var (intern (format nil "*~A*" cache-name))))
  `(progn
     (defvar ,var)
     (defun ,(intern (format nil "LOOKUP-IN-~A" cache-name)) (elt)
       (when (boundp ',var)
         (alexandria:assoc-value ,var elt)))
     (defun (setf ,(intern (format nil "LOOKUP-IN-~A" cache-name))) (val elt)
       (when (boundp ',var)
         (setf (alexandria:assoc-value ,var elt) val))
       val))))

;; If bound, an alist mapping content to content-type,
;; used to avoid determining content type multiple times
(define-alist-cache content-type-cache)
;; If bound, an alist mapping content to encoded content, to avoid
;; double converting content when we must calculate its length first
(define-alist-cache content-encoding-cache)

(defmacro with-content-caches (&body body)
  `(let ((*content-type-cache* nil)
         (*content-encoding-cache* nil))
     ,@body))

(defun content-type (value)
  (typecase value
    (pathname (or (lookup-in-content-type-cache value)
                  (setf (lookup-in-content-type-cache value) (mimes:mime value))))
    (otherwise nil)))

(defun multipart-value-content-type (value)
  (typecase value
    (cons
     (destructuring-bind (val &key content-type)
         value
       (or content-type (content-type val))))
    (otherwise (content-type value))))

(defun convert-to-octets (val)
  (or (lookup-in-content-encoding-cache val)
      (setf (lookup-in-content-encoding-cache val)
            (typecase val
              (string (babel:string-to-octets val))
              ((array (unsigned-byte 8) (*)) val)
              (symbol (babel:string-to-octets (princ-to-string val)))
              (cons (convert-to-octets (first val)))
              (otherwise (babel:string-to-octets (princ-to-string val)))))))

(defun write-as-octets (stream val)
  (typecase val
    ((array (unsigned-byte 8) (*)) (write-sequence val stream))
    (pathname
     (with-open-file (in val :element-type '(unsigned-byte 8))
       (alexandria:copy-stream in stream)))
    (string
     (write-sequence (convert-to-octets val) stream))
    (cons (write-as-octets stream (first val)))
    (otherwise (write-sequence (convert-to-octets val) stream))))

(defun content-length (val)
  (typecase val
    (pathname (with-open-file (in val)
                (file-length in)))
    (cons (content-length (first val)))
    (otherwise (length (convert-to-octets val)))))

(defun multipart-content-length (content boundary)
  (declare (type simple-string boundary))
  (let ((boundary-length (length boundary)))
    (+ (loop for (key . val) in content
             sum (+ 2 ;; --
                    boundary-length
                    2 ;; CR LF
                    (length (the simple-string (content-disposition key val)))
                    (let ((content-type (multipart-value-content-type val)))
                      (if content-type
                          (+ #.(length "Content-Type: ") (length content-type) 2)
                          0))
                    2
                    (content-length val)
                    2)
               into total-length
             finally (return total-length))
       2 boundary-length 2 2)))

(defun write-multipart-content (content boundary stream)
  (let ((boundary (ascii-string-to-octets boundary)))
    (labels ((boundary-line (&optional endp)
               (write-sequence (ascii-string-to-octets "--") stream)
               (write-sequence boundary stream)
               (when endp
                 (write-sequence (ascii-string-to-octets "--") stream))
               (crlf))
             (crlf () (write-sequence +crlf+ stream)))
      (loop for (key . val) in content
            do (boundary-line)
               (write-sequence (ascii-string-to-octets (content-disposition key val)) stream)
               (let ((content-type (multipart-value-content-type val)))
                 (when content-type
                   (write-sequence
                     (ascii-string-to-octets
                       (format nil "Content-Type: ~A~C~C" content-type #\Return #\Newline))
                     stream)))
               (crlf)
               (write-as-octets stream val)
               (crlf)
            finally
               (boundary-line t)))))

(defun decompress-body (content-encoding body)
  (unless content-encoding
    (return-from decompress-body body))

  (cond
    ((string= content-encoding "gzip")
     (if (streamp body)
         (chipz:make-decompressing-stream :gzip body)
         (chipz:decompress nil (chipz:make-dstate :gzip) body)))
    ((string= content-encoding "deflate")
     (if (streamp body)
         (chipz:make-decompressing-stream :zlib body)
         (chipz:decompress nil (chipz:make-dstate :zlib) body)))
    (t body)))
