(defpackage #:docs
  (:nicknames #:docs/index)
  (:use #:cl)
  (:import-from #:mgl-pax
                #:section
                #:defsection)
  (:import-from #:example/app
                #:@app)
  (:import-from #:example/utils
                #:@utils)
  (:export
   #:build-docs))
(in-package docs)


(defsection @index (:title "GH-PAGES is utility to update your site from CI")
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

(defun build-docs ()
  (mgl-pax:update-asdf-system-readmes @index :example)
  
  (mgl-pax:update-asdf-system-html-docs
   @index :gh-pages
   :target-dir "docs/build/"
   :pages `((:objects
             (,docs:@index)
             :source-uri-fn ,(pax:make-github-source-uri-fn
                              :example
                              "https://github.com/40ants/gh-pages")))))
