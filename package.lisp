(cl:in-package #:cl-user)

(defpackage #:disjoint-sets
  (:use #:cl)
  (:export
   #:make-disjoint-sets
   #:disjoint-sets-add
   #:disjoint-sets-find
   #:disjoint-sets-join
   #:disjoint-sets-same-set-p))
