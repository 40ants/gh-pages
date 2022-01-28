(uiop:define-package #:gh-pages-docs/changelog
  (:use #:cl)
  (:import-from #:40ants-doc/changelog
                #:defchangelog)
  ;; (:export #:@changelog)
  )
(in-package #:gh-pages-docs/changelog)


(defchangelog (:external-docs ("https://40ants.com/defmain/"
                               "https://40ants.com/doc/"))
  (0.2.0 2022-01-28
         "
* Command line utility was fixed to work with the latest DEFMAIN:DEFMAIN macro.
* Documentation was moved to 40ANTS-DOC system.")
  (0.1.0 2021-01-18
         "Initial version"))
