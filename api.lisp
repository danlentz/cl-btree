(in-package :b-tree)

(defun map (ret-type btree func)
  "Map function func over B-tree."
  (b-tree-impl:map-b-tree ret-type btree func))
    
(defun search (btree key &optional (start-node (b-tree-impl:b-tree-root btree)))
  "Returns value associated with the key in b-tree or nil if key does not exist."
  (declare (type b-tree-impl:b-tree btree)
           (type b-tree-impl:b-tree-node start-node))
  (b-tree-impl:b-tree-search btree key start-node))

(defun max (btree &optional (start-node (b-tree-impl:b-tree-root btree)))
  "Returns two values from B-tree, or nil if B-tree is empty. If
B-tree is non-empty the first value is associated value of max key in
B-tree and the second value is the max key of B-tree."
  (declare (type b-tree-impl:b-tree btree)
           (type b-tree-impl:b-tree-node start-node))
  (b-tree-impl:b-tree-max btree start-node))

(defun min (btree &optional (start-node (b-tree-impl:b-tree-root btree)))
  "Returns two values from B-tree, or nil if B-tree is empty. If
B-tree is non-empty the first value is associated value of min key in
B-tree and the second value is the min key of B-tree."
  (declare (type b-tree-impl:b-tree btree)
           (type b-tree-impl:b-tree-node start-node))
  (b-tree-impl:b-tree-min btree start-node))
  
(defun insert (btree key value)
  "Insert (key, value) pair to b-tree."
  (declare (type b-tree-impl:b-tree btree))
  (b-tree-impl:b-tree-insert btree key value))

(defun create (filespec &key (type :default) (minimum-degree 3) (if-exists :error) (block-size 4096))
  "Create new file based b-tree to given filespec."
  (declare (type integer minimum-degree))
  (case type
    (:string
     (b-tree-impl:string-b-tree-create filespec :minimum-degree minimum-degree :if-exists if-exists :block-size block-size))
    (otherwise
     (b-tree-impl:b-tree-create filespec :minimum-degree minimum-degree :if-exists if-exists :block-size block-size))))

(defun open (filespec &key (type :default) (minimum-degree 3) (block-size 4096) (if-exists :overwrite) (if-does-not-exist :error if-does-not-exist-p))
  "Open B-tree."
  (unless (or (null if-exists) (member if-exists '(:overwrite :append :error)))
    (error "Unsupported :if-exists option: ~a." if-exists))
  (unless (or (null if-does-not-exist) (member if-does-not-exist '(:error :create)))
    (error "Unsupported :if-does-not-exist option: ~a." if-does-not-exist))
  
  (when (and (eql if-exists :append) (not if-does-not-exist-p))
    (setq if-does-not-exist :create))
  
  (if (probe-file filespec)
      (cond
        ((null if-exists)
         nil)
        ((eql if-exists :error)
         (error "B-Tree file exists: ~a." filespec))
        ((or (eql if-exists :overwrite) (eql if-exists :append))
         (if (eql type :string)
             (b-tree-impl:string-b-tree-open filespec :if-exists :overwrite)
             (b-tree-impl:b-tree-open filespec :if-exists :overwrite))))
      (cond
        ((null if-does-not-exist)
         nil)
        ((eql if-does-not-exist :error)
         (error "B-Tree file ~a does not exist." filespec))
        ((eql if-does-not-exist :create)
         (create filespec :type type :minimum-degree minimum-degree :block-size block-size)))))

(defun close (btree)
  "Close open b-tree."
  (declare (type b-tree-impl:b-tree btree))
  (b-tree-impl:b-tree-close btree))

(defun print (btree &optional (stream *standard-output*))
  "Write b-tree to stream."
  (declare (type b-tree-impl:b-tree btree))
  (b-tree-impl:b-tree-print btree stream))

(defun delete (btree key)
  "Delete key and its associated value from B-tree."
  (declare (type b-tree-impl:b-tree btree))
  (b-tree-impl:b-tree-delete btree key))
