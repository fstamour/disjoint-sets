(cl:in-package #:cl-user)

(defpackage #:disjoint-sets.test
  (:use #:cl #:disjoint-sets)
  (:import-from #:parachute
                #:define-test
                #:is)
  (:export #:run-tests))

(in-package #:disjoint-sets.test)

(define-test make-disjoint-sets
  (is equalp
      #()
      (make-disjoint-sets))
  (is equalp
      #(0 1 2 3 4 5 6 7 8 9)
      (make-disjoint-sets 10)))

(define-test disjoin-sets-add
  (is equalp
      '(0 #(0))
      (let ((sets (make-disjoint-sets)))
        (list (disjoint-sets-add sets)
              sets)))
  (is equalp
      `(1 #(0 1))
      (let ((sets (make-disjoint-sets 1)))
        (list (disjoint-sets-add sets)
              sets))))

(define-test disjoint-set-join
  (is equalp
      #(0 1 1 3 4 5 6 7 8 1)
      (let ((sets (make-disjoint-sets 10)))
        (disjoint-sets-join sets 1 2)
        (disjoint-sets-join sets 1 9)
        sets)))

(define-test disjoint-set-same-set-p
  (is equalp
      '(t t nil)
      (let ((sets (make-disjoint-sets 10)))
        (disjoint-sets-join sets 1 2)
        (disjoint-sets-join sets 1 9)
        (list (disjoint-sets-same-set-p sets 1 2)
              (disjoint-sets-same-set-p sets 2 9)
              (disjoint-sets-same-set-p sets 1 5)))))


(defun run-tests ()
  (parachute:test '#:disjoint-sets.test))
