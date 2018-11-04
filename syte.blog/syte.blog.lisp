;;;; syte.blog.lisp

(in-package #:syte.blog)

(defclass syte.blog (syte)
  ()
  (:default-initargs
   :names '("localhost")
   :root (merge-pathnames "root/"
                          (asdf:component-pathname
                           (asdf:find-system :syte.blog)))))

;; create the Syte instance
(defparameter *site* (make-instance 'syte.blog))

;; register it with Sytes
(register-syte *site*)
