(defpackage #:gh-pages-docs/index
  (:use #:cl)
  (:import-from #:40ants-doc
                #:defsection-copy
                #:defsection)
  (:import-from #:docs-config
                #:docs-config)
  (:export #:@readme
           #:@index))
(in-package #:gh-pages-docs/index)


(defsection @index (:title "GH-PAGES"
                    :ignore-words ("CI"))
  (gh-pages system)
  
  "
This utility is successor of <https://github.com/40ants/update-gh-pages>. Fully rewritten to be more maintainable and modular.

It can be used as a standalone script or from the Common Lisp system.

Currently `GH-PAGES` supports following CI systems:

- GitHub Actions
- Travis (not tested yet)

Also, you can run it from you own machine.

To use the utility on CI, make sure there is a github token in the `GITHUB_TOKEN`
environment variable. Don't commit it into the repository, usually CI provides
a way to set such secrets in a secure way.
")


(defsection-copy @readme @index)


(defmethod docs-config ((system (eql (asdf:find-system "gh-pages-docs"))))
  ;; 40ANTS-DOC-THEME-40ANTS system will bring
  ;; as dependency a full 40ANTS-DOC but we don't want
  ;; unnecessary dependencies here:
  #+quicklisp
  (uiop:symbol-call :ql :quickload :40ants-doc-theme-40ants)
  #-quicklisp
  (asdf:load-system :40ants-doc-theme-40ants)

  (list :theme
        (find-symbol "40ANTS-THEME"
                     (find-package "40ANTS-DOC-THEME-40ANTS"))))
