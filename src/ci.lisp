(uiop:define-package #:gh-pages/ci
  (:use #:cl)
  (:import-from #:40ants-ci/workflow
                #:defworkflow))
(in-package #:gh-pages/ci)


(defworkflow docs
  :on-push-to "master"
  :on-pull-request t
  :jobs ((40ants-ci/jobs/docs:build-docs
          :asdf-system "gh-pages-docs")))
