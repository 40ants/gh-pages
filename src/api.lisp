(defpackage #:gh-pages
  (:use #:cl)
  (:nicknames #:gh-pages/api)
  (:import-from #:gh-pages/upload
                #:upload)
  (:export #:upload))
