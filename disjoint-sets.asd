;;;; disjoint-sets.asd

(asdf:defsystem #:disjoint-sets
  :description "Describe disjoint-sets here"
  :author "Francis St-Amour"
  :license  "BSD 2-Clause"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "disjoint-sets"))
  :in-order-to ((test-op (load-op #:disjoint-sets/test)))
  :perform
  (test-op (o c)
           (uiop:symbol-call
            '#:disjoint-sets.test '#:run-tests)))

(defsystem "disjoint-sets/test"
  :description "Tests for the disjoint-sets system."
  :version "0.0.1"
  :author "Francis St-Amour"
  :licence "BSD 2-Clause License"
  :depends-on (#:disjoint-sets #:parachute)
  :serial t
  :components
  ((:file "tests")))
