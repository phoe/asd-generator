;;;; package.lisp

(defpackage #:asd-generator
  (:use #:cl
	#:cl-fad
	#:iterate
        #:trivia
        #:alexandria)
  (:shadow #:copy-stream
           #:copy-file)
  (:import-from #:alexandria
		"FLATTEN"
		"ONCE-ONLY")
  (:export #:write-asd
           #:regen))


