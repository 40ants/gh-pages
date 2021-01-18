(defpackage #:gh-pages/upload
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:gh-pages/git
                #:git-push
                #:checkout-branch
                #:repository-initialized-p)
  (:import-from #:gh-pages/protocol
                #:get-ci-system)
  (:import-from #:gh-pages/utils
                #:copy-files)
  (:export
   #:upload))
(in-package gh-pages/upload)


(defun upload (source-dir &key (branch "gh-pages"))
  "This function uploads content of the given directory into a
   separate branch of the current Git repository.

   If branch exists, a new commit will be pushed into it.

   If branch not exists, it will be created.
  "
  (log:info "Pushing changes from" source-dir "to" branch)

  (let* ((ci (get-ci-system))
         (stage-dir (uiop:merge-pathnames* (make-pathname :directory
                                                          (list :relative
                                                                (symbol-name
                                                                 (gensym "STAGE"))))
                                           (uiop:temporary-directory))))

    (unwind-protect
         (progn 
           (ensure-directories-exist stage-dir)
           (checkout-branch ci branch stage-dir)
           (copy-files source-dir stage-dir)
           (git-push stage-dir))
      (uiop:delete-directory-tree stage-dir
                                  :validate t
                                  :if-does-not-exist :ignore))

    (values)))
