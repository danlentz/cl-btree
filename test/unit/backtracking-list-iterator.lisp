(in-package :common-lisp-user)

(defpackage backtracking-list-iterator-tests
  (:use :cl :lisp-unit))

(in-package :backtracking-list-iterator-tests)

(define-test backtracking-list-iterator
  (let* ((lst '(1 2 3 4 5))
         (iterator (b-tree-impl::make-backtracking-list-iterator lst)))
    ;; initial setup
    (assert-typep 'b-tree-impl::backtracking-list-iterator iterator)
    (assert-true (b-tree-impl::next-p iterator))
    (assert-false (b-tree-impl::current-position iterator))
    (assert-false (b-tree-impl::current-value iterator))
    (assert-false (b-tree-impl::prev-p iterator))

    ;; advance to next -> first element on list
    (assert-eq iterator (b-tree-impl::next iterator))
    (assert-eq lst (b-tree-impl::current-position iterator))
    (assert-eq 1 (b-tree-impl::current-value iterator))
    (assert-true (b-tree-impl::prev-p iterator))

    ;; go back to previous.
    (assert-eq iterator (b-tree-impl::prev iterator))
    (assert-false (b-tree-impl::prev-p iterator))
    (assert-false (b-tree-impl::current-position iterator))
    (assert-false (b-tree-impl::current-value iterator))

    ;; step forward twice
    (assert-eq (cdr lst) (b-tree-impl::current-position (b-tree-impl::next (b-tree-impl::next iterator))))

    ;; step back once.
    (assert-eq lst (b-tree-impl::current-position (b-tree-impl::prev iterator)))
    
    ;; step forward three times.
    (assert-eq (cdddr lst) (b-tree-impl::current-position (b-tree-impl::next (b-tree-impl::next (b-tree-impl::next iterator)))))

    ;; step backwards three times.
    (assert-eq lst (b-tree-impl::current-position (b-tree-impl::prev (b-tree-impl::prev (b-tree-impl::prev iterator)))))
    (assert-eq 1 (b-tree-impl::current-value iterator))

    ;; step to end.
    (assert-false (b-tree-impl::current-position (b-tree-impl::next (b-tree-impl::next (b-tree-impl::next (b-tree-impl::next (b-tree-impl::next iterator)))))))
    (assert-false (b-tree-impl::next-p iterator))
    (b-tree-impl::prev iterator)
    (assert-eq (b-tree-impl::last lst) (b-tree-impl::current-position iterator))
    (assert-false (b-tree-impl::next-p iterator))))
