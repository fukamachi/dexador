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
                     (ppcre:scan-to-strings "^\\s*?(\\w+)/(\\w+)(?:\\s*;\\s*charset=([A-Za-z0-9_-]+))?"
                                            content-type))))
    (when types
      (values (aref types 0)
              (aref types 1)
              (aref types 2)))))

(defun detect-charset (content-type)
  (multiple-value-bind (type subtype charset)
      (parse-content-type content-type)
    (declare (ignore subtype))
    (when (string-equal type "text")
      (cond
        ((null charset)
         babel:*default-character-encoding*)
        ((string-equal charset "utf-8")
         :utf-8)
        ((string-equal charset "euc-jp")
         :eucjp)
        ((string-equal charset "shift_jis")
         :cp932)
        (T (or (find charset (babel:list-character-encodings)
                     :test #'string-equal)
               babel:*default-character-encoding*))))))
