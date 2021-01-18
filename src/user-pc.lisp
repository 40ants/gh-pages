(defpackage #:gh-pages/user-pc
  (:use #:cl)
  (:import-from #:gh-pages/protocol
                #:get-origin-to-push)
  (:import-from #:gh-pages/git
                #:get-git-upstream)
  (:import-from #:gh-pages/utils
                #:run))
(in-package gh-pages/user-pc)


(defmethod get-origin-to-push ((ci (eql :user-pc)))
  (let ((upstream (get-git-upstream)))
    
    (if upstream
        (values (run (concatenate 'string "git remote get-url " upstream)))
        (error "Repository has no upstream. Method get-origin-to-push has to be specialized for CI \"~A\""
               ci))))
