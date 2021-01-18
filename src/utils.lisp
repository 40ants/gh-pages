(defpackage #:gh-pages/utils
  (:use #:cl)
  (:import-from #:gh-pages/conditions
                #:subprocess-error-with-output)
  (:export #:run
           #:copy-files)
  (:documentation "The utils for gh-pages"))
(in-package gh-pages/utils)


(defun run (command &key (raise t))
  "Runs command and returns it's stdout stderr and code.

If there was an error, raises subprocess-error-with-output, but this
behaviour could be overriden by keyword argument ``:raise t``."
  (let ((dir (uiop:getcwd)))
    (log:info "Running" command "in" dir))

  (multiple-value-bind (stdout stderr code)
      (uiop:run-program command
                        :output '(:string :stripped t)
                        :error-output '(:string :stripped t)
                        :ignore-error-status t)
    
    (when (and raise
               (not (eql code 0)))
      (error 'subprocess-error-with-output
             :stdout stdout
             :stderr stderr
             :code code
             :command command))
    (values stdout stderr code)))



(defun copy-files (from-dir to-dir)
  "Copies files from one dir to another.

   Preserves old files in `to-dir` if they are absent in the `from-dir`."
  (run (format nil "rsync -a --exclude .git --exclude .qlot \"~A\" \"~A\""
               from-dir
               to-dir)))
