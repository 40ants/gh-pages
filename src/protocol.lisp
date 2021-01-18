(defpackage #:gh-pages/protocol
  (:use #:cl)
  (:export
   #:get-ci-system
   #:def-ci-discovery
   #:get-origin-to-push
   #:setup-gh-pages-repo))
(in-package gh-pages/protocol)


(defvar *ci-discovery-funcs* nil)


(defun get-ci-system ()
  (or
   (loop for func in *ci-discovery-funcs*
         for ci = (funcall func)
         thereis ci)
   :user-pc))


(defmacro def-ci-discovery (name args &rest body)
  (unless (null args)
    (error "Discovery function should not require any args."))
  `(prog1
       (defun ,name ,args
         ,@body)
     (pushnew ',name *ci-discovery-funcs*)))


(defgeneric get-origin-to-push (ci)
  (:documentation "This function will be called in the root of the checkout
                   and shoudl return an URL with to with permission to push
                   into the repository.

                   By default, it just returns upstream's URL. But it not always
                   might have PUSH permission."))


(defgeneric setup-gh-pages-repo (ci dir)
  (:documentation "This function will be called in the root of gh-pages branch
                   checkout and can set such things like username.

                   By default, it does nothing.")

  (:method ((ci t) dir)))


