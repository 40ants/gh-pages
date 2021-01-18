(defsystem "gh-pages" 
  :class :package-inferred-system
  :author "Alexander Artemenko"
  :license "Unlicense"
  :pathname "src"
  :description "A helper to update gh-pages branch with documentation on your library."
  :depends-on ("gh-pages/api"
               "gh-pages/github"
               "gh-pages/travis"
               "gh-pages/user-pc"))
