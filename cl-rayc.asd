;;;; cl-rayc.asd

(asdf:defsystem #:cl-rayc
  :description "Describe cl-rayc here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:cepl-default)
  :serial t
  :components ((:file "package")
	       (:file "cl-rayc")
	       (:file "render")))

