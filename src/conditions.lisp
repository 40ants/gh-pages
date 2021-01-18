(defpackage #:gh-pages/conditions
  (:use #:cl)
  (:export
   #:subprocess-error-with-output
   #:unable-to-proceed))
(in-package gh-pages/conditions)


(define-condition unable-to-proceed (simple-error)
  ((message :initarg :message
            :reader get-message))
  (:report (lambda (condition stream)
             (format stream (get-message condition)))))


(define-condition subprocess-error-with-output (uiop::subprocess-error)
  ((stdout :initarg :stdout :reader subprocess-error-stdout)
   (stderr :initarg :stderr :reader subprocess-error-stderr))
  (:report (lambda (condition stream)
             (format stream "Subprocess ~@[~S~% ~]~@[with command ~S~% ~]exited with error~@[ code ~D ~]~@[ and this text at stderr:~% ~S~]"
                     (uiop:subprocess-error-process condition)
                     (uiop:subprocess-error-command condition)
                     (uiop:subprocess-error-code condition)
                     (subprocess-error-stderr condition)))))
