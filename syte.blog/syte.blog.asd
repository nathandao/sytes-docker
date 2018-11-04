;;;; syte.blog.asd

(asdf:defsystem #:syte.blog
  :description "Describe syte.blog here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:sytes)
  :components ((:file "package")
               (:file "syte.blog")))
