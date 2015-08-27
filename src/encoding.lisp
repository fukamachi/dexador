(in-package :cl-user)
(defpackage dexador.encoding
  (:use :cl)
  (:import-from :babel
                :list-character-encodings
                :*default-character-encoding*)
  (:import-from :ppcre
                :scan-to-strings)
  (:export :detect-charset))
(in-package :dexador.encoding)

(defun parse-content-type (content-type)
  (let ((types
          (nth-value 1
                     (ppcre:scan-to-strings "^\\s*?(\\w+)/([^;\\s]+)(?:\\s*;\\s*charset=([A-Za-z0-9_-]+))?"
                                            content-type))))
    (when types
      (values (aref types 0)
              (aref types 1)
              (aref types 2)))))

(defun charset-to-encoding (charset &optional
                                      (default babel:*default-character-encoding*))
  (cond
    ((null charset)
     default)
    ((string-equal charset "utf-8")
     :utf-8)
    ((string-equal charset "euc-jp")
     :eucjp)
    ((string-equal charset "shift_jis")
     :cp932)
    (t (or (find charset (babel:list-character-encodings)
                 :test #'string-equal)
           default))))

(defun detect-charset (content-type)
  (multiple-value-bind (type subtype charset)
      (parse-content-type content-type)
    (cond
      ((charset-to-encoding charset nil))
      ((string-equal type "text")
       (charset-to-encoding charset))
      ((and (string-equal type "application")
            (string-equal subtype "json"))
       ;; According to RFC4627 (http://www.ietf.org/rfc/rfc4627.txt),
       ;; JSON text SHALL be encoded in Unicode. The default encoding is UTF-8.
       ;; It's possible to determine if the encoding is UTF-16 or UTF-36
       ;; by looking at the first four octets, however, I leave it to the future.
       (charset-to-encoding charset :utf-8))
      ((and (string-equal type "application")
            (ppcre:scan "(?:[^+]+\\+)?xml" subtype))
       (charset-to-encoding charset)))))
