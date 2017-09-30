;;;; asd-generator.asd

(asdf:defsystem #:asd-generator
  :description "Automatic directory scanner/generator for .asd project files."
  :author "Michał \"phoe\" Herda <phoe@openmailbox.org>"
  :license "GPLv3"
  :depends-on (#:cl-fad
	       #:iterate
	       #:alexandria
               #:trivia)
  :serial t
  :components ((:file "package")
               (:file "asd-generator")))

