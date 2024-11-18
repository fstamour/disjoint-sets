#|

https://en.wikipedia.org/wiki/Disjoint-set_data_structure

- Each set has an id.
- Each set has a parent.
- A set that has itself as a parent is called a root.
- The root of a set is the representative of that set.
Said otherwise: a set is identified by the id of its root set.
- Sets are merged with the "union" operation.

|#

(cl:in-package #:disjoint-sets)

(defun make-disjoint-sets (&optional (arity 0))
  "Create a set of sets represented as an array.

Examples:
(make-disjoint-sets)
=> #()

(make-disjoint-sets 10)
;; => #(0 1 2 3 4 5 6 7 8 9)
"
  (let ((sets (make-array `(,arity) :element-type 'integer
                          :adjustable t :fill-pointer t)))
    (dotimes (i arity sets) (setf (aref sets i) i))))

(defun disjoint-sets-add (sets)
  "Add a new item into its own disjoint set. Return the new id.

Example:

(let ((id (disjoint-sets-add sets)))
  ;; SETS is modified
  ...)
"
  (vector-push-extend (length sets) sets))

(defun disjoint-sets-find (sets id)
  "Find the id of the set representative (the root).

Example:

(disjoint-sets-find sets 5)
"
  (let ((parent (aref sets id)))
    (if (= id parent) id             ; If `id' is the root, return it.
        (let ((root (disjoint-sets-find sets parent)))
          ;; Path compression: point directly to the root if it's not
          ;; already the case.
          (when (/= root parent)
            (setf (aref sets id) root))
          root))))

(defun disjoint-sets-union (sets id1 id2)
  "Merge two disjoint sets. Return the set representative (the root)

Example:

(disjoint-sets-union sets 1 2)
=> 4 ; SETS is modified.
"
  (let ((root1 (disjoint-sets-find sets id1))
        (root2 (disjoint-sets-find sets id2)))
    (setf (aref sets root2) root1)))

(defun copy-disjoint-sets (sets)
  "Copy a set of sets into a fresh array.

Exmaple:
(let* ((sets (make-disjoint-sets 2))
       (copy (copy-disjoint-sets sets)))
  (disjoint-sets-unify sets 0 1)
  (values sets copy))
=> #(0 0), #(0 1)
"
  (make-array (length sets) :element-type 'integer
                            :adjustable t :fill-pointer t
                            :initial-contents sets))

(defun disjoint-sets-same-set-p (sets id1 id2)
  "Test if 2 items are in the same set.

Example:

(disjoint-sets-same-set-p sets 1 2)
=> T or NIL"
  (= (disjoint-sets-find sets id1)
     (disjoint-sets-find sets id2)))
