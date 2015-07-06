(in-package :cl-user)
(defpackage dexador.error
  (:use :cl)
  (:import-from :quri
                :render-uri)
  (:export :http-request-failed
           :response-body
           :response-status
           :response-headers
           :request-uri))
(in-package :dexador.error)

(define-condition http-request-failed (error)
  ((body :initarg :body
         :reader response-body)
   (status :initarg :status
           :reader response-status)
   (headers :initarg :headers
            :reader response-headers)
   (uri :initarg :uri
        :reader request-uri))
  (:report (lambda (condition stream)
             (with-slots (uri status) condition
               (format stream "An HTTP request to ~S has failed (status=~D)."
                       (quri:render-uri uri)
                       status)))))
