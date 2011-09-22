(in-package :b-tree-impl)

(defstruct (backtracking-list-iterator (:constructor make-backtracking-list-iterator (list &aux (current nil) (next list) (prev nil))))
  current
  next 
  prev)

(defun next-p (iterator)
  "Returns t if iterator has next, nil otherwise."
  (and iterator (backtracking-list-iterator-next iterator)))

(defun prev-p (iterator)
  "Returns t if iterator has previous, nil otherwise."
  (and iterator (backtracking-list-iterator-prev iterator)))

(defun current-position (iterator)
  "Returns iterator's current list position. Returns nil, if iterator
is nil or position is at the beginning or end of the list."
  (and iterator (backtracking-list-iterator-current iterator)))

(defun current-value (iterator)
  "Returns iterator's current value."
  (and iterator (car (backtracking-list-iterator-current iterator))))

(defun next (iterator)
  "Advances iterator to next position in list. Returns modified iterator."
  (when (or (current-position iterator)
            (next-p iterator))
    (push (backtracking-list-iterator-current iterator) (backtracking-list-iterator-prev iterator))
    (psetf (backtracking-list-iterator-current iterator) (backtracking-list-iterator-next iterator)
           (backtracking-list-iterator-next    iterator) (cdr (backtracking-list-iterator-next iterator))))
  iterator)

(defun prev (iterator)
  "Backtracks iterator to previous position in list. Returns modified iterator."
  (when (prev-p iterator)
    (psetf (backtracking-list-iterator-current iterator) (pop (backtracking-list-iterator-prev iterator))
           (backtracking-list-iterator-next    iterator) (backtracking-list-iterator-current iterator)))
  iterator)
