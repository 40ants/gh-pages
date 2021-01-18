(defpackage #:gh-pages/github
  (:use #:cl)
  (:import-from #:gh-pages/protocol
                #:setup-gh-pages-repo
                #:def-ci-discovery
                #:get-origin-to-push)
  (:import-from #:gh-pages/conditions
                #:unable-to-proceed)
  (:import-from #:gh-pages/git
                #:git))
(in-package gh-pages/github)


(def-ci-discovery github ()
  (when (uiop:getenv "GITHUB_ACTIONS")
    :github))


(defmethod get-origin-to-push ((ci (eql :github)))
  (let ((repo-token (uiop:getenv "GITHUB_TOKEN")))
    (unless repo-token
      (error 'unable-to-proceed
             :message "Please, provide GITHUB_TOKEN environment variable."))
    (format nil "https://~A:~A@github.com/~A"
            (uiop:getenv "GITHUB_ACTOR")
            repo-token
            (uiop:getenv "GITHUB_REPOSITORY"))))


(defmethod setup-gh-pages-repo ((ci (eql :github)) dir)
  (git dir "config --global user.name \"github-actions[bot]\"")
  (git dir "config --global user.email \"actions@github.com\""))
