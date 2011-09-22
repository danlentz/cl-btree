(in-package :b-tree-impl)

(declaim (optimize (space 0) (speed 0) (debug 3)))

(defconstant +b-tree-eof+  0)
(defconstant +b-tree-ref+  1)
(defconstant +b-tree-item+ 2)


(defstruct (b-tree-item (:conc-name nil))
  (key            nil)
  (value          nil))

(defstruct b-tree-ref
  (file-position 0 :type integer))

(defstruct (b-tree-node (:constructor make-b-tree-node (&key (file-position 0) (contents nil) (key-count (count-if #'b-tree-item-p contents)) (child-count (count-if #'b-tree-ref-p contents)))))
  (file-position    0 :type integer)
  (contents       nil :type list)
  (key-count        0 :type integer)
  (child-count      0 :type integer))

(defstruct b-tree
  (version             1 :type integer)
  (minimum-degree      2 :type integer)
  (min-keys            0 :type integer)
  (max-keys            0 :type integer)
  (max-children        0 :type integer)
  (swap-file         nil :type (or swap-file:swap-file null))
  (root              nil :type (or b-tree-node null))
  (key-writer         #'little-endian:write-uint32)
  (key-reader         #'little-endian:read-uint32)
  (value-writer       #'little-endian:write-uint32)
  (value-reader       #'little-endian:read-uint32))

(defgeneric key< (a b)
  (:method ((a integer) (b integer))
    (< a b))
  (:method ((a string) (b string))
    (string< a b))
  (:method ((a string) (b symbol))
    (string< a (symbol-name b)))
  (:method ((a symbol) (b symbol))
    (string< (symbol-name a) (symbol-name b)))
  (:method ((a symbol) (b string))
    (string< (symbol-name a) b)))
  

(defgeneric key<= (a b)
  (:method ((a integer) (b integer))
    (<= a b))
  (:method ((a string) (b string))
    (string<= a b))
  (:method ((a string) (b symbol))
    (string<= a (symbol-name b)))
  (:method ((a symbol) (b symbol))
    (string<= (symbol-name a) (symbol-name b)))
  (:method ((a symbol) (b string))
    (string<= (symbol-name a) b)))


(defgeneric key> (a b)
  (:method ((a integer) (b integer))
    (> a b))
  (:method ((a string) (b string))
    (string> a b))
  (:method ((a string) (b symbol))
    (string> a (symbol-name b)))
  (:method ((a symbol) (b symbol))
    (string> (symbol-name a) (symbol-name b)))
  (:method ((a symbol) (b string))
    (string> (symbol-name a) b)))


(defgeneric key>= (a b)
  (:method ((a integer) (b integer))
    (>= a b))
  (:method ((a string) (b string))
    (string>= a b))
  (:method ((a string) (b symbol))
    (string>= a (symbol-name b)))
  (:method ((a symbol) (b symbol))
    (string>= (symbol-name a) (symbol-name b)))
  (:method ((a symbol) (b string))
    (string>= (symbol-name a) b)))


(defgeneric key= (a b)
  (:method ((a integer) (b integer))
    (= a b))
  (:method ((a string) (b string))
    (string= a b))
  (:method ((a string) (b symbol))
    (string= a (symbol-name b)))
  (:method ((a symbol) (b symbol))
    (string= (symbol-name a) (symbol-name b)))
  (:method ((a symbol) (b string))
    (string= (symbol-name a) b))
  (:method ((a b-tree-item) b)
    (key= (key a) b))
  (:method (a (b b-tree-item))
    (key= a (key b)))
  (:method ((a b-tree-ref) b)
    nil)
  (:method (a (b b-tree-ref))
    nil))

(defun next-item (contents-iterator)
  "Advances contents iterator to next B-tree item. Returns modified iterator."
  (do ((value (current-value (next contents-iterator))
              (current-value (next contents-iterator))))
      ((or (not value) (b-tree-item-p value))
       contents-iterator)))

(defun root-p (btree-node btree)
  "Returns t if node is root of B-tree, nil otherwise."
  (eq (b-tree-root btree) btree-node))

(defun full-p (btree-node btree)
  "Returns t if b-tree node number of keys has reached or exceeded the max keys, nil otherwise."
  (declare (type b-tree-node btree-node))
  (>= (b-tree-node-key-count btree-node) (b-tree-max-keys btree)))

(defun empty-p (btree-node)
  "Returns t if node has no contents (no keys nor children), nil otherwise."
  (declare (type b-tree-node btree-node))
  (null (b-tree-node-contents btree-node)))

(defun keys-p (btree-node)
  "Returns t if btree-node has any keys, nil otherwise."
  (> (b-tree-node-key-count btree-node) 0))

(defun min-keys-p (btree-node btree)
  "Returns t if B-tree node has exactly minimum required amount of keys, nil otherwise."
  (= (b-tree-node-key-count btree-node) (b-tree-min-keys btree)))

(defun leaf-p (btree-node)
  "Returns t if B-tree node is a leaf, nil otherwise."
  (= (b-tree-node-child-count btree-node) 0))

(defun valid-node-p (btree-node btree)
  (cond
    ((root-p btree-node btree)
     (or (empty-p btree-node)
         (and (<= 1 (b-tree-node-key-count btree-node) (b-tree-max-keys btree))
              (<= (b-tree-node-child-count btree-node) (b-tree-max-children btree)))))
    ((leaf-p btree-node)
     (<= (b-tree-min-keys btree) (b-tree-node-key-count btree-node) (b-tree-max-keys btree)))
    (t ;; internal node
     (and (<= (b-tree-min-keys btree) (b-tree-node-key-count btree-node) (b-tree-max-keys btree))
          (<= (b-tree-minimum-degree btree) (b-tree-node-child-count btree-node) (b-tree-max-children btree))))))

(defun element-count (btree)
  (+ (* (b-tree-max-keys btree) 3)
     (* (b-tree-max-children btree) 2)
     1))

(defun count-keys (btree-node)
  "Returns number of keys in B-tree node. Keys are counted from contents list."
  (declare (type b-tree-node btree-node))
  (count-if #'b-tree-item-p (b-tree-node-contents btree-node)))

(defun count-children (btree-node)
  "Returns number of children in B-tree node. Children are counted from contents list."
  (declare (type b-tree-node btree-node))
  (count-if #'b-tree-ref-p (b-tree-node-contents btree-node)))

(defun value-or-nil (item)
  "Returns values value and key of B-tree item or nil if item is nil."
  (unless (null item)
    (values (value item)
            (key item))))

(defun write-root-offset (btree)
  "Write root offset to swap file."
  (swap-file:set-file-position-after-header (b-tree-swap-file btree) 8)
  (swap-file:write-uint32-to-disk (b-tree-node-file-position (b-tree-root btree)) (b-tree-swap-file btree)))

(defun write-header (btree)
  "Writes b-tree header to swap file."
  (declare (type b-tree btree))
  ;;(assert (open-stream-p (b-tree-swap-file btree)))
  (swap-file:set-file-position-after-header (b-tree-swap-file btree))
  (little-endian:write-uint32 (b-tree-version btree) (swap-file:swap-file-stream (b-tree-swap-file btree)))
  (little-endian:write-uint32 (b-tree-minimum-degree btree) (swap-file:swap-file-stream (b-tree-swap-file btree)))
  (little-endian:write-uint32 (b-tree-node-file-position (b-tree-root btree)) (swap-file:swap-file-stream (b-tree-swap-file btree)))
  (swap-file:flush (b-tree-swap-file btree)))
     
(defun read-header (btree)
  "Reads b-tree header from swap file and fills b-tree structure with read information."
  (declare (type b-tree btree))
  (swap-file:set-file-position-after-header (b-tree-swap-file btree))
  (setf (b-tree-version btree)        (little-endian:read-uint32 (swap-file:swap-file-stream (b-tree-swap-file btree)))
        (b-tree-minimum-degree btree) (little-endian:read-uint32 (swap-file:swap-file-stream (b-tree-swap-file btree)))
        (b-tree-root btree)           (read-node btree (make-b-tree-ref :file-position (little-endian:read-uint32 (swap-file:swap-file-stream (b-tree-swap-file btree)))) :skip-check t))
  (setf (b-tree-max-keys btree)       (1- (* 2 (b-tree-minimum-degree btree)))
        (b-tree-min-keys btree)       (1- (b-tree-minimum-degree btree))
        (b-tree-max-children btree)   (* 2 (b-tree-minimum-degree btree)))
  btree)

(defun read-b-tree-ref (stream)
  "Reads and returns B-tree reference from stream."
  (make-b-tree-ref  :file-position (little-endian:read-uint32 stream)))

(defun read-b-tree-item (stream key-reader value-reader)
  "Reads and returns B-tree item from stream."
  (make-b-tree-item :key   (funcall key-reader stream)
                    :value (funcall value-reader stream)))

(defun read-node-contents (stream key-reader value-reader)
  "Reads and returns a list of B-tree references and items from stream."
  (let ((node-type (little-endian:read-uint8 stream)))
    (cond 
      ((= +b-tree-ref+  node-type) (cons (read-b-tree-ref stream) (read-node-contents stream key-reader value-reader)))
      ((= +b-tree-item+ node-type) (cons (read-b-tree-item stream key-reader value-reader) (read-node-contents stream  key-reader value-reader)))
      ((= +b-tree-eof+  node-type) nil)
      (t (error "Node contents read failed. Unknown node type ~a." node-type)))))
    
(defun read-node (btree btree-ref &key skip-check)
  (declare (optimize (speed 0) (space 0) (debug 3)))
  "Reads b-tree node from disk. Node is organized on disk
so that every second element is child ref and the other is
key value pair."
  (swap-file:with-open-block-stream (stream (b-tree-swap-file btree) (b-tree-ref-file-position btree-ref))
    (let ((btree-node (make-b-tree-node :file-position (b-tree-ref-file-position btree-ref)
                                        :contents (read-node-contents stream (b-tree-key-reader btree) (b-tree-value-reader btree)))))
      (unless skip-check
        (assert (valid-node-p btree-node btree)))
      btree-node)))
      

(defun write-node (btree btree-node)
  "Write B-tree node to swap file."
  (declare (optimize (speed 0) (space 0) (debug 3)))
  (assert (valid-node-p btree-node btree))
  (swap-file:with-open-block-stream (stream (b-tree-swap-file btree) (b-tree-node-file-position btree-node))
    (mapcar #'(lambda (item)
                (cond
                  ((b-tree-ref-p item)
                   (little-endian:write-uint8 +b-tree-ref+ stream)
                   (little-endian:write-uint32 (b-tree-ref-file-position item) stream))
                  ((b-tree-item-p item)
                   (little-endian:write-uint8 +b-tree-item+ stream)
                   (funcall (b-tree-key-writer btree) (key item) stream)
                   (funcall (b-tree-value-writer btree) (value item) stream))
                  (t
                   (error "Unknown item type: ~a" item))))
            (b-tree-node-contents btree-node))
    (little-endian:write-uint8 +b-tree-eof+ stream))
  btree-node)

(defun allocate-node (btree)
  "Returns swap file offset of allocated block for b-tree node."
  (declare (type b-tree btree))
  (swap-file:disk-block-offset (swap-file:create-block (b-tree-swap-file btree))))
;;    (swap-file:flush (b-tree-swap-file btree))

;; (defun clone-item (item)
;;   (etypecase item
;;     (b-tree-ref  (copy-b-tree-ref  item))
;;     (b-tree-item (copy-b-tree-item item)))) ; copy may have reference to original
;;                                             ; but item is read-only so should not be a problem.

;; (defun clone-contents (node)
;;   "Copy node contents. Copies also structures."
;;   (mapcar #'clone-item (b-tree-node-contents node)))

;; (defun clone-node (btree node)
;;   "Copy node. Copy is stored on swap file to newly allocated position."
;;   (write-node btree
;;                      (make-b-tree-node :file-position (allocate-node btree)
;;                                        :contents      (clone-contents node)
;;                                        :key-count     (b-tree-node-key-count node)
;;                                        :child-count   (b-tree-node-child-count node))))
  
(defun free-node (btree node)
  "Free allocated B-tree node."
  (swap-file:unlink-block (b-tree-swap-file btree) (b-tree-node-file-position node)))

(defun find-median-key-position (btree-node)
  (declare (type b-tree-node btree-node))
  (let ((iterator (make-backtracking-list-iterator (b-tree-node-contents btree-node))))
    (dotimes (i (ash (1+ (b-tree-node-key-count btree-node)) -1))
      (next-item iterator))
    (values (current-position iterator)
            (current-position (prev iterator)))))

(defun split-contents (btree-node)
  (multiple-value-bind (median predecessor) (find-median-key-position btree-node)
    (multiple-value-prog1 (values (b-tree-node-contents btree-node)
                                  median
                                  (cdr median))
      (rplacd median nil)
      (rplacd predecessor nil))))

(defun split-root-node (btree)
  "Splits root node contents half and creates a new root node."
  (declare (type b-tree btree))
  (multiple-value-bind (left median right) (split-contents (b-tree-root btree))
    (let ((old-root    (b-tree-root btree))
          (split-left  (make-b-tree-node :contents left  :file-position (allocate-node btree)))
          (split-right (make-b-tree-node :contents right :file-position (allocate-node btree))))
      (assert (not (null old-root)))
      (assert (not (null split-left)))
      (assert (not (null split-right)))
      (assert (< 0 (b-tree-node-file-position split-left)))
      (assert (< 0 (b-tree-node-file-position split-right)))
      (setf (b-tree-node-contents old-root) (list 
                                             (make-b-tree-ref :file-position (b-tree-node-file-position split-left))
                                             (car median)
                                             (make-b-tree-ref :file-position (b-tree-node-file-position split-right)))
            (b-tree-node-key-count old-root)   1
            (b-tree-node-child-count old-root) 2)
      (write-node btree old-root)
      (write-node btree split-left)
      (write-node btree split-right))))

(defun get-insert-position (btree-node for-key)
  (declare (type b-tree-node btree-node))
  (member-if #'(lambda (x)
                 (and (b-tree-item-p x)
                      (key<= for-key (key x))))
             (b-tree-node-contents btree-node)))

(defun insert-item (btree-node item)
  (let ((insert-pos (get-insert-position btree-node (key item))))
    (if (null insert-pos)
        (if (empty-p btree-node)
            (push item (b-tree-node-contents btree-node))
            (rplacd (last (b-tree-node-contents btree-node)) (list item)))
        (progn
          (rplacd insert-pos (cons (car insert-pos) (cdr insert-pos)))
          (rplaca insert-pos item)))
    (incf (b-tree-node-key-count btree-node))))
      
(defun find-child-pos (child-offset contents)
  (member-if #'(lambda (v)
                 (and (b-tree-ref-p v)
                      (= child-offset (b-tree-ref-file-position v))))
             contents))

(defun split-child (btree parent-node child-node)
  (declare (type b-tree btree)
           (type b-tree-node parent-node child-node))
  (multiple-value-bind (left median right) (split-contents child-node)
    (let ((split-left  (make-b-tree-node :contents left  :file-position (b-tree-node-file-position child-node)));;child-node)
          (split-right (make-b-tree-node :contents right :file-position (allocate-node btree))))
      (assert (not (null parent-node)))
      (assert (not (null split-left)))
      (assert (not (null split-right)))
      (assert (< 0 (b-tree-node-file-position split-left)))
      (assert (< 0 (b-tree-node-file-position split-right)))
      
      ;; alter parent contents: child-node -> (split-left median split-right)
      (let ((child-pos (find-child-pos (b-tree-node-file-position child-node) (b-tree-node-contents parent-node))))
        (assert (not (null child-pos)))
        (rplacd child-pos (append (list
                                    (car median)
                                    (make-b-tree-ref :file-position (b-tree-node-file-position split-right)))
                                   (cdr child-pos))))
      (incf (b-tree-node-key-count   parent-node))
      (incf (b-tree-node-child-count parent-node))
      (write-node btree parent-node)
      (write-node btree split-left)
      (write-node btree split-right))))

(defun insert-to-nonfull-leaf (btree btree-node item)
  (declare (type b-tree btree)
           (type b-tree-node btree-node)
           (type b-tree-item item))
  (insert-item btree-node item)
  (write-node btree btree-node))

(defun find-correct-child (btree-node item)
  "Returns given B-tree node's child."
  (do ((iterator (make-backtracking-list-iterator (b-tree-node-contents btree-node))))
      ((or (not (next-p iterator))
           (and (b-tree-item-p (current-value iterator)) (key>= (key (current-value iterator)) (key item))))
       (when (or (null (current-value iterator))
                 (b-tree-item-p (current-value iterator)))
         (prev iterator))
       (when (b-tree-ref-p (current-value iterator))
         (current-value iterator)))
    (next-item iterator)))

(defun insert-to-nonfull-internal (btree btree-node item)
  (let ((child-ref (find-correct-child btree-node item)))
    (declare (type b-tree-ref child-ref))
    (let ((child-node (read-node btree child-ref)))
      (declare (type b-tree-node child-node))
      (if (not (full-p child-node btree))
          (insert-to-nonfull btree child-node item)
          (progn
            (split-child btree btree-node child-node)
            (insert-to-nonfull btree btree-node item))))))

(defun insert-to-nonfull (btree btree-node item)
  (declare (type b-tree btree)
           (type b-tree-node btree-node)
           (type b-tree-item item))
  (if (leaf-p  btree-node)
      (insert-to-nonfull-leaf     btree btree-node item)
      (insert-to-nonfull-internal btree btree-node item)))

(defun search-from-contents (pos key)
  (cond
    ((null pos) nil)
    ((and (b-tree-item-p (car pos)) (key= (key (car pos)) key)) pos)
    ((and (b-tree-item-p (car pos)) (key> (key (car pos)) key)) nil)
    (t
     (let ((result (search-from-contents (cdr pos) key)))
       (if (null result)
           (when (b-tree-ref-p (car pos))
             pos)
           result)))))

(defun delete-from-contents (node key)
  (setf (b-tree-node-contents    node) (delete key (b-tree-node-contents node) :test #'key= :count 1))
  (setf (b-tree-node-key-count   node) (count-keys node))
  (setf (b-tree-node-child-count node) (count-children node))
  node)

(defun b-tree-searcher (btree key search-node)
  (declare (type b-tree btree)
           (type b-tree-node search-node))
  (let ((item (car (search-from-contents (b-tree-node-contents search-node) key))))
    (cond
      ((null item)
       nil)
      ((and (b-tree-item-p item) (key= key (key item)))
       (values item search-node))
      ((b-tree-ref-p item)
       (b-tree-searcher btree key (read-node btree item))))))

(defun max-finder (btree &optional (search-node (b-tree-root btree)))
  (let ((item (car (last (b-tree-node-contents search-node)))))
    (cond 
      ((null item)
       nil)
      ((b-tree-item-p item)
       (values item search-node))
      ((b-tree-ref-p item)
       (max-finder btree (read-node btree item))))))

(defun min-finder (btree &optional (search-node (b-tree-root btree)))
  (let ((item (car (b-tree-node-contents search-node))))
    (cond 
      ((null item)
       nil)
      ((b-tree-item-p item)
       (values item search-node))
      ((b-tree-ref-p item)
       (min-finder btree (read-node btree item))))))
  
(defun b-tree-mapper (btree node func)
  (do ((pos  (b-tree-node-contents node) (cdr pos)))
      ((null pos) nil)
    (let ((item (car pos)))
      (cond
        ((null item)
         nil)
        ((b-tree-item-p item)
         (funcall func (key item) (value item)))
        ((b-tree-ref-p item)
         (b-tree-mapper btree (read-node btree item) func))))))

(defun initialize-b-tree (btree)
  (setf (b-tree-root btree) (make-b-tree-node :file-position (allocate-node btree)))
  (write-node btree (b-tree-root btree))
  (write-header btree)
  (swap-file:flush (b-tree-swap-file btree))
  btree)

(defun remove-keyless-root (btree &aux (old-root (b-tree-root btree)))
  (if (empty-p old-root)
      (write-node btree old-root)
      (progn
        (setf (b-tree-root btree) (ensure-b-tree-node (first (b-tree-node-contents (b-tree-root btree))) btree))
        (write-root-offset btree)
        (free-node btree old-root))))
 
(defun delete-from-root (key btree)
  (if (keys-p (delete-from-contents (b-tree-root btree) key))
      (write-node btree (b-tree-root btree))
      (remove-keyless-root btree))
  key)

(defun delete-from-leaf (key btree node) ;; case 1
  (delete-from-contents node key)
  (write-node btree node))

(defun find-previous (key contents)
  (do ((pos contents (cdr contents))
       (found nil))
      ((or (null (cdr pos)) (key= (car pos) key) found)
       found)
    (when (key= (cadr pos) key)
      (setq found pos))))

(defun replace-with-predecessor (cur prev btree node) ;; case 2a
  (declare (ignore node))
  (let ((subtree (read-node btree (car prev))))
    (when (>= (b-tree-node-key-count subtree) (b-tree-minimum-degree btree))
      (multiple-value-bind (max-item max-node) 
          (max-finder btree subtree)
        (delete-from-leaf max-item btree max-node)
        (rplaca cur max-item)))))

(defun replace-with-successor (cur next btree node) ;; case 2b
  (declare (ignore node))
  (let ((subtree (read-node btree (car next))))
    (when (>= (b-tree-node-key-count subtree) (b-tree-minimum-degree btree))
      (multiple-value-bind (min-item min-node) 
          (min-finder btree subtree)
        (delete-from-leaf min-item btree min-node)
        (rplaca cur min-item)))))

(defun ensure-b-tree-node (node-spec btree)
  (typecase node-spec
    (b-tree-node node-spec)
    (b-tree-ref (read-node btree node-spec))))

(defun merge-children (prev-ref item next-ref btree parent-node) ;; case 2c
  (let ((prev-node (ensure-b-tree-node prev-ref btree))
        (next-node (ensure-b-tree-node next-ref btree)))
    ;; remove references to item and next node from parent-node contents
    (setf (b-tree-node-contents parent-node) (remove next-ref (b-tree-node-contents parent-node) :test #'eq))
    (setf (b-tree-node-contents parent-node) (remove item (b-tree-node-contents parent-node) :test #'eq))
    ;; merge item and contens of next node to previous node.
    (setf (b-tree-node-contents prev-node) (append (b-tree-node-contents prev-node) 
                                                   (list item)
                                                   (b-tree-node-contents next-node)))
    ;; write changes to disk
    (write-node btree parent-node)
    (write-node btree prev-node)
    ;; free next node.
    (free-node  btree next-node)
    prev-node))
    
(defun delete-from-internal (key btree node)
  (let* ((prev (find-previous key (b-tree-node-contents node)))
         (cur  (cdr prev))
         (next (cdr cur)))
  (cond
    ((replace-with-predecessor cur prev btree node)
     (write-node btree node))
    ((replace-with-successor cur next btree node)
     (write-node btree node))
    (t
     (setq prev (merge-children prev cur next btree node))
     (delete-key key btree prev)))))

(defun delete-key (key btree node)
  (cond 
    ((or (null key) (null node)) nil)
    ((root-p node btree)
     (delete-from-root key btree))
    ((leaf-p node) 
     (delete-from-leaf key btree node))
    (t
     (delete-from-internal key btree node))))

(defun find-immediate-siblings (child parent)
  (do* ((iterator (make-backtracking-list-iterator (b-tree-node-contents parent)))
        (value (current-value (next iterator)) (current-value (next iterator))))
       ((or (and (b-tree-ref-p value) (= (b-tree-ref-file-position value) (b-tree-node-file-position child)))
            (null value))
        (assert value) ; we should not be here if child is not found from parent.
        (let ((left-sibling nil)
              (right-sibling nil)
              (item-on-left nil)
              (item-on-right nil))
          (when (prev-p iterator)
            (prev iterator)            ; b-tree-item 
            (setq item-on-left (current-position iterator))
            (when (prev-p iterator)
              (prev iterator)            ; b-tree-ref
              (setq left-sibling (current-position iterator))
              (next iterator))
            (next iterator))    ; go back to start.
          (when (next-p iterator)
            (next iterator)            ; b-tree item
            (setq item-on-right (current-position iterator))
            (when (next-p iterator)
              (next iterator)
              (setq right-sibling (current-position iterator))))
          (values left-sibling item-on-left item-on-right right-sibling)))))

(defun move-key-from-left-sibling (child parent left-sibling-ref-pos item-on-left-pos btree)
  (when (and left-sibling-ref-pos item-on-left-pos)
    (let ((left-node (read-node btree (car left-sibling-ref-pos))))
      (unless (min-keys-p left-node btree)
        ;; move item-on-left to child (append to the end)
        (append (b-tree-node-contents child) (list (car item-on-left-pos)))
        (let ((split (split-max-key left-node)))
          ;; move max key from left sibling to parent (replace item-on-left with it)
          (psetf (car item-on-left-pos) (car split))
          ;; move max child from left sibling to child (append to the end)
          (append (b-tree-node-contents child) (list (cadr split))))
        (write-node btree left-node)
        (write-node btree parent)
        (write-node btree child)))))

(defun min-key (contents)
  "Returns position of minimum key from contents, or nil, if no key found."
  (member-if #'b-tree-item-p contents))

(defun max-key (contents)
  "Returns position of maximum key from contents, or nil, if no key found."
  (when contents
    (let ((max (max-key (cdr contents))))
    (if max
        max
        (when (b-tree-item-p (car contents))
          contents)))))

(defun split-min-key (b-tree-node)
  (let ((pos (min-key (b-tree-node-contents b-tree-node))))
    (when pos
      (prog1 (b-tree-node-contents b-tree-node)
        (setf (b-tree-node-contents b-tree-node) (cdr pos))
        (setf (cdr pos) nil)))))

(defun split-max-key (b-tree-node)
  (let ((pos (max-key (b-tree-node-contents b-tree-node))))
    (when pos
      (if (eq pos (b-tree-node-contents b-tree-node))
          (prog1 pos
            (setf (b-tree-node-contents b-tree-node) nil))
          (prog1 pos
            (setf (b-tree-node-contents b-tree-node) 
                  (nbutlast (b-tree-node-contents b-tree-node)
                            (length pos))))))))

(defun move-key-from-right-sibling (child parent right-sibling-ref-pos item-on-right-pos btree)
  (when (and right-sibling-ref-pos item-on-right-pos)
    (let ((right-node (read-node btree (car right-sibling-ref-pos))))
      (unless (min-keys-p right-node btree)
        ;; move item-on-right to child (push to beginning)
        (push (car item-on-right-pos) (b-tree-node-contents child))
        (let ((split (split-min-key right-node)))
          ;; move min key from right sibling to parent (replace item-on-right with it)
          (psetf (car item-on-right-pos) (cadr split))
          ;; move min child from right sibling to child (push to beginning)
          (push (car split) (b-tree-node-contents child)))
        (write-node btree right-node)
        (write-node btree parent)
        (write-node btree child)))))

(defun ensure-child-has-more-than-min-keys (child parent btree)
  (multiple-value-bind (left-ref-pos item-on-left-pos item-on-right-pos right-ref-pos) 
      (find-immediate-siblings child parent)
    (assert (or (b-tree-ref-p (car left-ref-pos))  (null left-ref-pos)))
    (assert (or (b-tree-ref-p (car right-ref-pos)) (null right-ref-pos)))
    (cond 
      ;; case 3a with left sibling
      ((move-key-from-left-sibling child parent left-ref-pos item-on-left-pos btree)
       child)
      ;; case 3a with right sibling
      ((move-key-from-right-sibling child parent right-ref-pos item-on-right-pos btree)
       child)
      ;; case 3b with left sibling
      ((car left-ref-pos)
       (merge-children (car left-ref-pos) (car item-on-left-pos) child btree parent))
      ;; case 3b with right sibling
      ((car right-ref-pos)
       (merge-children child (car item-on-right-pos) (car right-ref-pos) btree parent))
      (t 
       child))))

;;  (if sibling
      ;; case 3a
      ;;  move a key from parent to child
      ;;  move key from sibling to parent
      ;;  move appropriate child pointer from sibling to child
      ;; case 3b
      ;;  merge child with sibling. Median is a key from parent.
;;      (find-left-sibling child parent)
;;      (find-right-sibling child parent)
      ;;  
;;  )


(defun search-delete-position (key btree node)
  "Search B-tree node containing the key. The B-tree structure is
altered on the way down to prepare item deletion."
  (let ((item-pos (search-from-contents (b-tree-node-contents node) key)))
    (cond
      ((null item-pos) ; item not found and no candidates left.
       nil)
      ((b-tree-item-p (car item-pos)) ; item found.
       (delete-key (car item-pos) btree node))
      (t
       (let ((child-node (read-node btree (car item-pos))))
         (when (min-keys-p child-node btree)
           ;; execute cases 3a or 3b to ensure child has more than min keys.
           (setq child-node (ensure-child-has-more-than-min-keys child-node
                                                                 node
                                                                 btree)))
         ;; descent to child.
         (search-delete-position key btree child-node))))))

;;;
;;; Exported functions & macros
;;;

(defun map-b-tree (ret-type btree func)
  (if (null ret-type)
      (b-tree-mapper btree (b-tree-root btree) func)
      (let ((items nil))
        (b-tree-mapper btree (b-tree-root btree)
                       #'(lambda (k v)
                           (push (funcall func k v) items)))
        (nreverse items))))
    
(defun b-tree-search (btree key &optional (start-node (b-tree-root btree)))
  "Returns value associated with the key in b-tree or nil if key does not exist."
  (declare (type b-tree btree)
           (type b-tree-node start-node))
  (value-or-nil (b-tree-searcher btree key start-node)))

(defun b-tree-max (btree &optional (start-node (b-tree-root btree)))
  (declare (type b-tree btree)
           (type b-tree-node start-node))
  (value-or-nil (max-finder btree start-node)))

(defun b-tree-min (btree &optional (start-node (b-tree-root btree)))
  (declare (type b-tree btree)
           (type b-tree-node start-node))
  (value-or-nil (min-finder btree start-node)))
  
(defun b-tree-insert (btree key value)
  "Insert (key, value) pair to b-tree."
  (declare (type b-tree btree))
  (when (full-p (b-tree-root btree) btree)
    (split-root-node btree))
  (insert-to-nonfull btree (b-tree-root btree) (make-b-tree-item :key key :value value))
  (swap-file:flush (b-tree-swap-file btree))
  btree)

(defun b-tree-create (filespec &key (minimum-degree 3) (if-exists :error) (block-size 4096))
  "Create new file based b-tree to given filespec."
  (declare (type integer minimum-degree))
  (initialize-b-tree (make-b-tree :minimum-degree  minimum-degree
                                  :min-keys (1- minimum-degree)
                                  :max-keys (1- (* 2 minimum-degree))
                                  :max-children (* 2 minimum-degree)
                                  :swap-file (swap-file:create filespec :if-exists if-exists :block-size block-size))))

(defun string-b-tree-create (filespec &key (minimum-degree 3) (if-exists :error) (block-size 4096))
  "Create new file based b-tree to given filespec."
  (declare (type integer minimum-degree))
  (initialize-b-tree (make-b-tree :minimum-degree  minimum-degree
                                  :min-keys (1- minimum-degree)
                                  :max-keys (1- (* 2 minimum-degree))
                                  :max-children (* 2 minimum-degree)
                                  :key-reader #'read
                                  :key-writer #'prin1
                                  :value-reader #'read
                                  :value-writer #'prin1
                                  :swap-file (swap-file:create filespec :if-exists if-exists :block-size block-size))))

(defun b-tree-open (filespec &key (if-exists :overwrite) (if-does-not-exist :error))
  "Open existing file based b-tree."
  (read-header (make-b-tree :swap-file (swap-file:open filespec :if-exists if-exists :if-does-not-exist if-does-not-exist))))

(defun string-b-tree-open (filespec &key (if-exists :overwrite) (if-does-not-exist :error))
  "Open existing file based b-tree."
  (read-header (make-b-tree :key-reader #'read
                            :key-writer #'prin1
                            :value-reader #'read
                            :value-writer #'prin1
                            :swap-file (swap-file:open filespec :if-exists if-exists :if-does-not-exist if-does-not-exist))))

(defun b-tree-close (btree)
  "Close open b-tree."
  (declare (type b-tree btree))
  (swap-file:close (b-tree-swap-file btree)))

(defun b-tree-print (btree &optional (stream *standard-output*))
  "Write b-tree to stream."
  (labels ((collect-nodes (btree-node)
             (let ((nodes nil))
               (dolist (item (b-tree-node-contents btree-node))
                 (if (b-tree-ref-p item)
                     (setq nodes (append nodes (list item (collect-nodes (read-node btree item)))))
                     (setq nodes (append nodes (list item)))))
               nodes)))
    (print (collect-nodes (b-tree-root btree)) stream)))

(defun b-tree-delete (btree key)
  (search-delete-position key btree (b-tree-root btree)))
;;  (delete-key key btree (b-tree-root btree)))
;;  (multiple-value-bind (item node) 
;;      (b-tree-searcher btree key (b-tree-root btree))
;;    (delete-key item btree node)))

