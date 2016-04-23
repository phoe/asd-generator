;;;; package.lisp

(defpackage #:asd-generator
  (:use #:cl
	#:cl-fad
	#:iterate)
  (:import-from #:alexandria
		"FLATTEN"
		"ONCE-ONLY")
  (:export #:write-asd))


