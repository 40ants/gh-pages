(defsystem "gh-pages" 
  :class :package-inferred-system
  :author "Alexander Artemenko"
  :license "Unlicense"
  :description "A helper to update gh-pages branch with documentation on your library."
  :homepage "https://40ants.com/gh-pages/"
  :source-control (:git "https://github.com/40ants/gh-pages/")
  :pathname "src"
  :depends-on ("gh-pages/api"
               "gh-pages/github"
               "gh-pages/travis"
               "gh-pages/user-pc"))
