(defpackage #:gh-pages/git
  (:use #:cl)
  (:import-from #:gh-pages/utils
                #:run)
  (:import-from #:gh-pages/protocol
                #:get-origin-to-push)
  (:import-from #:log4cl)
  (:export
   #:get-git-upstream
   #:repository-initialized-p
   #:git
   #:repository-was-changed-p
   #:checkout-branch
   #:git-push))
(in-package gh-pages/git)


(defun git (dir &rest commands)
  "Calls git command in gh-pages repository."
  
  (uiop:with-current-directory (dir)
    (let ((command (apply #'concatenate 'string
                          "git "
                          commands)))
        
      (log:info "Running" command "in" dir)
      (run command))))


(defun get-git-upstream (&optional (dir "."))
  "This command should be run inside the main root
   of the repository checkout."
  ;; taken from http://stackoverflow.com/a/9753364/70293
  (let ((upstream
          (uiop:with-current-directory (dir)
            (run "git rev-parse --abbrev-ref --symbolic-full-name @{u}"
                 :raise nil))))
    (when (> (length upstream)
             0)
      (subseq upstream
              0
              (search "/" upstream)))))


(defun repository-initialized-p (dir)
  "Checks if repository for documentation already initialized"
  (uiop:directory-exists-p (merge-pathnames ".git/"
                                            dir)))


(defun repository-was-changed-p (dir)
  ;; if git status returns something, then repository have uncommitted changes
  (> (length (git dir "status --porcelain"))
     0))


(defun remote-branch-exists (dir branch)
  (let* ((upstream (get-git-upstream))
         (result (git dir (format nil "branch --remote --list '~A/~A'"
                                  upstream
                                  branch))))
    (unless (string= result "")
      t)))


(defun checkout-branch (ci branch-name dir)
  "Checkouts given branch of the current repository into a given directory.

   If branch does not exists, it will be created.

   When function returns, `dir` will contain a fresh checkout
   with `origin` remote and branch `branch-name` ready to be pushed."
  (let ((origin-url (get-origin-to-push ci)))
    (git dir (format nil "clone ~A ."
                     origin-url))
    (cond
      ((remote-branch-exists dir branch-name)
       (git dir (format nil "checkout ~A"
                        branch-name)))
      (t
       ;; In case when branch does not exists,
       ;; we should create an empty repository and
       ;; push it as a new branch into existing repository
       (let ((upstream (get-git-upstream dir)))
         ;; Clear and recreate the temporary directory:   
         (uiop:delete-directory-tree dir :validate t)
         (ensure-directories-exist dir)
         
         (git dir (format nil "init"))
         (git dir (format nil "commit --allow-empty -m \"Initial commit\""))
         ;; rename branch into the needed name
         (git dir (format nil "branch -m ~A"
                          branch-name))
         (git dir (format nil "remote add ~A ~A"
                          upstream
                          origin-url))
         ;; (git dir "fetch")
         ;; Create remote branch and bind it to the local
         (git dir (format nil "push --set-upstream ~A ~A"
                          upstream
                          branch-name)))))
    
    (gh-pages/protocol:setup-gh-pages-repo ci dir)))


(defun git-push (dir)
  (git dir "add .")
  (cond
    ((repository-was-changed-p dir)
     (git dir "commit -m 'Update docs'")
     (git dir "push"))
    (t (log:info "Everything is up to date."))))
