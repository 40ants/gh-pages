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


(defsection @index (:title "GH-PAGES"
                    :ignore-words ("CI"
                                   "HTML"))
  (gh-pages system)
  
  "
This utility is successor of <https://github.com/40ants/update-gh-pages>. Fully rewritten to be more maintainable and modular.

It can be used as a standalone script or from the Common Lisp system.

How does it work
================

This script can be run inside a CI pipeline to upload HTML documentation into a separate
git branch, used to serve static site on GitHub Pages. Also, you can run it manually
on localhost.

The script does a few steps:

- it determines in which environment it is running
- what is the proper upstream Git repositor
- then set up a `gh-pages` branch
- uploads content of the given directory, as the content of the `gh-pages` branch.

CI Support
==========

Currently `GH-PAGES` supports following CI systems:

- GitHub Actions
- Travis (not tested yet)

Also, you can run it from you own machine.

To use the utility on CI, make sure there is a github token in the `GITHUB_TOKEN`
environment variable. Don't commit it into the repository, usually CI provides
a way to set such secrets in a secure way. Inside GitHub actions this token should
be already defined.

To learn, how to add support for other CI systems, read
[protocol.lisp](https://github.com/40ants/gh-pages/blob/master/src/protocol.lisp)
and [github.lisp](https://github.com/40ants/gh-pages/blob/master/src/github.lisp) as the
reference implementation.

"
  (@installation section)
  (@usage section))


(defsection-copy @readme @index)


(defsection @installation ()
  "To install GH-PAGES, use [Roswell][https://github.com/roswell/roswell]:

```
ros install 40ants/gh-pages
```

After this command, an executable file `~/.roswell/bin/gh-pages` will be created.
")


(defsection @usage ()
  "
```
Usage: gh-pages [-h] [OPTIONS] PATH...

  -h, --help                  Show help on this program.
  -b, --branch=STR            A branch to push to (default: gh-pages)
                              Default: gh-pages
```

In most cases you have to specify a path to a directory containing HTML docs
and script will do the rest.
")

