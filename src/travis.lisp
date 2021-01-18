(defpackage #:gh-pages/travis
  (:use #:cl)
  (:import-from #:gh-pages/protocol
                #:def-ci-discovery)
  (:import-from #:gh-pages/conditions
                #:unable-to-proceed))
(in-package gh-pages/travis)


(def-ci-discovery travis ()
  (when (uiop:getenv "TRAVIS_REPO_SLUG")
    :travis))


(defmethod get-origin-to-push ((ci (eql :travis)))
  (let ((repo-slug (uiop:getenv "TRAVIS_REPO_SLUG"))
        (repo-token (uiop:getenv "GITHUB_TOKEN")))

    (unless repo-token
      (error 'unable-to-proceed
             :message "Please, provide GITHUB_TOKEN environment variable."))
    
    (unless repo-slug
      (error 'unable-to-proceed
             :message "Current branch does not track any upstream. There is no TRAVIS_REPO_SLUG."))
    
    (format nil "https://~A@github.com/~A"
            repo-token
            repo-slug)))
