(defsystem "gh-pages-docs"
  :build-pathname "docs/build/"
  :class :package-inferred-system
  :pathname "docs/source/"
  :homepage "https://40ants.com/gh-pages/"
  :source-control (:git "https://github.com/40ants/gh-pages/")
  :depends-on ("40ants-doc"
               "gh-pages-docs/changelog"
               "gh-pages-docs/index"))
