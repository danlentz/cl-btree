(in-package :common-lisp-user)

(defpackage b-tree-impl-tests
  (:use :cl :lisp-unit))

(in-package :b-tree-impl-tests)

(define-test min-key
  (assert-false (b-tree-impl::min-key nil))
  (assert-false (b-tree-impl::min-key (list (b-tree-impl::make-b-tree-ref :file-position 1))))
  (let ((lst (list (b-tree-impl::make-b-tree-item :key 1 :value 1))))
    (assert-eq lst (b-tree-impl::min-key lst)))
  (let ((lst (list (b-tree-impl::make-b-tree-item :key 1 :value 1)
                   (b-tree-impl::make-b-tree-item :key 2 :value 2))))
    (assert-eq lst (b-tree-impl::min-key lst)))
  (let ((lst (list (b-tree-impl::make-b-tree-ref :file-position 1)
                   (b-tree-impl::make-b-tree-item :key 1 :value 1)
                   (b-tree-impl::make-b-tree-item :key 2 :value 2))))
    (assert-eq (cdr lst) (b-tree-impl::min-key lst))))

(define-test max-key-position
  (assert-false (b-tree-impl::max-key nil))
  (assert-false (b-tree-impl::max-key (list (b-tree-impl::make-b-tree-ref :file-position 1))))
  (let ((lst (list (b-tree-impl::make-b-tree-item :key 1 :value 1))))
    (assert-eq lst (b-tree-impl::max-key lst)))
  (let ((lst (list (b-tree-impl::make-b-tree-item :key 1 :value 1)
                   (b-tree-impl::make-b-tree-item :key 2 :value 2))))
    (assert-eq (cdr lst) (b-tree-impl::max-key lst)))
  (let ((lst (list (b-tree-impl::make-b-tree-item :key 1 :value 1)
                   (b-tree-impl::make-b-tree-item :key 2 :value 2)
                   (b-tree-impl::make-b-tree-ref :file-position 1))))
    (assert-eq (cdr lst) (b-tree-impl::max-key lst))))

(define-test split-min-key
  (assert-false (b-tree-impl::split-min-key (b-tree-impl::make-b-tree-node)))
  (let ((node (b-tree-impl::make-b-tree-node :contents (list (b-tree-impl::make-b-tree-ref :file-position 1)))))
    (assert-false (b-tree-impl::split-min-key node))
    (assert-eql 1 (length (b-tree-impl::b-tree-node-contents node))))
  (let ((node (b-tree-impl::make-b-tree-node :contents (list (b-tree-impl::make-b-tree-item :key 1 :value 1)))))
    (assert-equalp (list (b-tree-impl::make-b-tree-item :key 1 :value 1))
                   (b-tree-impl::split-min-key node))
    (assert-false (b-tree-impl::b-tree-node-contents node)))

  (let ((node (b-tree-impl::make-b-tree-node :contents (list (b-tree-impl::make-b-tree-item :key 1 :value 1)
                                                (b-tree-impl::make-b-tree-item :key 2 :value 2)))))
    (assert-equalp (list (b-tree-impl::make-b-tree-item :key 1 :value 1)) (b-tree-impl::split-min-key node))
    (assert-equalp (list (b-tree-impl::make-b-tree-item :key 2 :value 2))
                   (b-tree-impl::b-tree-node-contents node)))

  (let ((node (b-tree-impl::make-b-tree-node :contents (list (b-tree-impl::make-b-tree-ref :file-position 1)
                                                (b-tree-impl::make-b-tree-item :key 1 :value 1)
                                                (b-tree-impl::make-b-tree-item :key 2 :value 2)))))
    (assert-equalp (list (b-tree-impl::make-b-tree-ref :file-position 1)
                         (b-tree-impl::make-b-tree-item :key 1 :value 1)) (b-tree-impl::split-min-key node))
    (assert-equalp (list (b-tree-impl::make-b-tree-item :key 2 :value 2))
                   (b-tree-impl::b-tree-node-contents node))))

(define-test split-max-key
  (assert-false (b-tree-impl::split-max-key (b-tree-impl::make-b-tree-node)))
  (let* ((node (b-tree-impl::make-b-tree-node :contents (list (b-tree-impl::make-b-tree-ref :file-position 1)))))
    (assert-false (b-tree-impl::split-max-key node))
    (assert-eql 1 (length (b-tree-impl::b-tree-node-contents node))))
  (let* ((item (b-tree-impl::make-b-tree-item :key 1 :value 1))
         (node (b-tree-impl::make-b-tree-node :contents (list item))))
    (assert-equalp (list item) (b-tree-impl::split-max-key node))
    (assert-false (b-tree-impl::b-tree-node-contents node)))

  (let ((node (b-tree-impl::make-b-tree-node :contents (list (b-tree-impl::make-b-tree-item :key 1 :value 1)
                                                (b-tree-impl::make-b-tree-item :key 2 :value 2)))))
    (assert-equalp (list (b-tree-impl::make-b-tree-item :key 2 :value 2)) (b-tree-impl::split-max-key node))
    (assert-equalp (list (b-tree-impl::make-b-tree-item :key 1 :value 1))
                   (b-tree-impl::b-tree-node-contents node)))

  (let ((node (b-tree-impl::make-b-tree-node :contents (list (b-tree-impl::make-b-tree-ref :file-position 1)
                                                (b-tree-impl::make-b-tree-item :key 1 :value 1)
                                                (b-tree-impl::make-b-tree-item :key 2 :value 2)
                                                (b-tree-impl::make-b-tree-ref :file-position 2)))))
    (assert-equalp (list (b-tree-impl::make-b-tree-item :key 2 :value 2) 
                         (b-tree-impl::make-b-tree-ref :file-position 2)) 
                   (b-tree-impl::split-max-key node))
    (assert-equalp (list (b-tree-impl::make-b-tree-ref :file-position 1)
                         (b-tree-impl::make-b-tree-item :key 1 :value 1))
                   (b-tree-impl::b-tree-node-contents node))))

(define-test next-item
  (let ((iterator (b-tree-impl::make-backtracking-list-iterator (list (b-tree-impl::make-b-tree-ref  :file-position 1)))))
    (b-tree-impl::next-item iterator)
    (assert-false (b-tree-impl::current-value iterator)))

  (let ((iterator (b-tree-impl::make-backtracking-list-iterator (list (b-tree-impl::make-b-tree-ref  :file-position 1)
                                                         (b-tree-impl::make-b-tree-item :key 2
                                                                           :value 2)
                                                         (b-tree-impl::make-b-tree-ref :file-position 3)
                                                         (b-tree-impl::make-b-tree-item :key 4
                                                                           :value 4)
                                                         (b-tree-impl::make-b-tree-ref  :file-position 5)
                                                         (b-tree-impl::make-b-tree-item :key 6
                                                                           :value 6)))))
    (b-tree-impl::next-item iterator)
    (assert-typep 'b-tree-impl::b-tree-item (b-tree-impl::current-value iterator))
    (assert-eql 2 (b-tree-impl::key (b-tree-impl::current-value iterator)))

    (b-tree-impl::next-item iterator)
    (assert-typep 'b-tree-impl::b-tree-item (b-tree-impl::current-value iterator))
    (assert-eql 4 (b-tree-impl::key (b-tree-impl::current-value iterator)))

    (b-tree-impl::next-item iterator)
    (assert-typep 'b-tree-impl::b-tree-item (b-tree-impl::current-value iterator))
    (assert-eql 6 (b-tree-impl::key (b-tree-impl::current-value iterator)))

    (b-tree-impl::next-item iterator)
    (assert-false (b-tree-impl::current-value iterator))))
    

(define-test write-header
  (let ((btree (b-tree-impl::make-b-tree :minimum-degree 3
                            :root (b-tree-impl::make-b-tree-node :file-position 1234)
                            :swap-file (swap-file:create (binary-file:make-binary-array-io-stream) :block-size 32))))
    (b-tree-impl::write-header btree)
    (assert-equalp #(#x53 #x57 #x41 #x50 ;; "SWAP"
                     #x01 #x00 #x00 #x00 ;; swap-file version 1
                     #x20 #x00 #x00 #x00 ;; blocksize 
                     #x20 #x00 #x00 #x00 ;; next new offset
                     #x00 #x00 #x00 #x00 ;; available list header
                     #x01 #x00 #x00 #x00 ;; b-tree version 1
                     #x03 #x00 #x00 #x00 ;; minimum-degree
                     #xD2 #x04 #x00 #x00 ;; root node offset
                     ;; #x00 #x00 #x00 #x00 ;; header pad
                     )
                   (binary-file:binary-array (swap-file:swap-file-stream (b-tree-impl::b-tree-swap-file btree))))))

(define-test b-tree-create
  (let ((btree (b-tree-impl:b-tree-create (binary-file:make-binary-array-io-stream) :block-size 32)))
    ;;(b-tree-close btree)
    ;;(format-b-tree-blocks (binary-file:binary-array (swap-file::swap-file-stream (b-tree-impl::b-tree-swap-file btree))) 32))
    (assert-equalp
     #(#x53 #x57 #x41 #x50 ;; "SWAP"
       #x01 #x00 #x00 #x00 ;; swap-file version 1
       #x20 #x00 #x00 #x00 ;; blocksize 
       #x40 #x00 #x00 #x00 ;; next new offset
       #x00 #x00 #x00 #x00 ;; available list header
       #x01 #x00 #x00 #x00 ;; b-tree version 1
       #x03 #x00 #x00 #x00 ;; minimum-degree
       #x20 #x00 #x00 #x00 ;; root node offset
       ;; #x00 #x00 #x00 #x00 ;; header pad

       ;; root node's block
       #x00 
       #x00 #x00 #x00 #x00 
       #x01 #x00 #x00 #x00
       #x00 #x00 #x00 #x00
       #x00 #x00 #x00 #x00
       #x00 #x00 #x00 #x00
       #x00 #x00 #x00 #x00
       #x00 #x00 #x00 #x00
       #x00 #x00 #x00)
     (binary-file:binary-array (swap-file::swap-file-stream (b-tree-impl::b-tree-swap-file btree))))))

(define-test read-header
  (let ((btree (b-tree-impl:b-tree-create (binary-file:make-binary-array-io-stream) :block-size 32)))
    (b-tree-impl::read-header btree)
    (assert-eql 1 (b-tree-impl::b-tree-version btree))
    (assert-eql 3 (b-tree-impl::b-tree-minimum-degree btree))
    (assert-eql 5 (b-tree-impl::b-tree-max-keys btree))
    (assert-eql 6 (b-tree-impl::b-tree-max-children btree))))

(define-test allocate-node
  (let ((btree (b-tree-impl:b-tree-create (binary-file:make-binary-array-io-stream) :block-size 32)))
    (assert-equal 64 (b-tree-impl::allocate-node btree))
    (swap-file:flush (b-tree-impl::b-tree-swap-file btree))
    (assert-equalp
     #(#x53 #x57 #x41 #x50 ;; "SWAP"
       #x01 #x00 #x00 #x00 ;; swap-file version 1
       #x20 #x00 #x00 #x00 ;; blocksize 
       #x60 #x00 #x00 #x00 ;; next new offset
       #x00 #x00 #x00 #x00 ;; available list header
       #x01 #x00 #x00 #x00 ;; b-tree version 1
       #x03 #x00 #x00 #x00 ;; minimum-degree
       #x20 #x00 #x00 #x00 ;; root node offset
       ;; #x00 #x00 #x00 #x00 ;; header pad

       ;; root node's block
       #x00 
       #x00 #x00 #x00 #x00
       #x01 #x00 #x00 #x00         ; +b-tree-eof+ token takes one byte
       #x00 #x00 #x00 #x00              ; (= +b-tree-eof+ 0)
       #x00 #x00 #x00 #x00
       #x00 #x00 #x00 #x00
       #x00 #x00 #x00 #x00
       #x00 #x00 #x00 #x00
       #x00 #x00 #x00

       ;; allocated node's block
       ;; allocate node is totally empty.
       #x00
       #x00 #x00 #x00 #x00
       #x00 #x00 #x00 #x00
       #x00 #x00 #x00 #x00
       #x00 #x00 #x00 #x00
       #x00 #x00 #x00 #x00
       #x00 #x00 #x00 #x00
       #x00 #x00 #x00 #x00
       #x00 #x00 #x00)
     (binary-file:binary-array (swap-file::swap-file-stream (b-tree-impl::b-tree-swap-file btree))))))
        
(define-test write-node
  (let* ((btree (b-tree-impl:b-tree-create  (binary-file:make-binary-array-io-stream) :block-size 32))
         (btree-node (b-tree-impl::make-b-tree-node :key-count 3
                                       :contents (list (b-tree-impl::make-b-tree-ref  :file-position 1)
                                                       (b-tree-impl::make-b-tree-item :key 2
                                                                         :value 2)
                                                       (b-tree-impl::make-b-tree-ref :file-position 3)
                                                       (b-tree-impl::make-b-tree-ref  :file-position 4)
                                                       (b-tree-impl::make-b-tree-item :key 5
                                                                         :value 5)
                                                       (b-tree-impl::make-b-tree-item :key 6
                                                                         :value 6))
                                       :file-position (b-tree-impl::allocate-node btree))))
    (b-tree-impl::write-node btree btree-node)
    (swap-file:flush (b-tree-impl::b-tree-swap-file btree))
    (assert-equalp
     #(#x53 #x57 #x41 #x50              ; "SWAP"
       #x01 #x00 #x00 #x00              ; swap-file version 1
       #x20 #x00 #x00 #x00              ; blocksize 
       #x80 #x00 #x00 #x00              ; next new offset
       #x00 #x00 #x00 #x00              ; available list header
       #x01 #x00 #x00 #x00              ; b-tree version 1
       #x03 #x00 #x00 #x00              ; minimum-degree
       #x20 #x00 #x00 #x00              ; root node offset
       ;;#x00 #x00 #x00 #x00 ; header pad

       ;; root node's block
       #x00 
       #x00 #x00 #x00 #x00
       #x01 #x00 #x00 #x00
       #x00 #x00 #x00 #x00
       #x00 #x00 #x00 #x00
       #x00 #x00 #x00 #x00
       #x00 #x00 #x00 #x00
       #x00 #x00 #x00 #x00
       #x00 #x00 #x00 

       ;; b-tree node's block
       #x00                             ; block deleted-p
       #x60 #x00 #x00 #x00              ; next block offset 
       #x17 #x00 #x00 #x00              ; data size (=max data size)
       #x01                             ; +b-tree-ref+
       #x01 #x00 #x00 #x00              ; b-tree node offset
       #x02                             ; +b-tree-item+
       #x02 #x00 #x00 #x00              ; key
       #x02 #x00 #x00 #x00              ; value
       #x01                             ; +b-tree-ref+
       #x03 #x00 #x00 #x00              ; b-tree node offset
       #x01                             ; +b-tree-ref+
       #x04 #x00 #x00             ; b-tree node offset (3 first bytes)

       #x00                             ; block deleted-p
       #x00 #x00 #x00 #x00              ; next block offset 
       #x14 #x00 #x00 #x00              ; data size (=max data size)
       #x00                           ; b-tree node offset (last byte)
       #x02                             ; +b-tree-item+
       #x05 #x00 #x00 #x00              ; key
       #x05 #x00 #x00 #x00              ; value
       #x02                             ; +b-tree-item+
       #x06 #x00 #x00 #x00              ; key
       #x06 #x00 #x00 #x00              ; value
       #x00 #x00 #x00 #x00)
     (binary-file:binary-array (swap-file::swap-file-stream (b-tree-impl::b-tree-swap-file btree))))))

(define-test read-b-tree-ref
  (let ((b-tree-ref (b-tree-impl::read-b-tree-ref (binary-file:make-binary-array-io-stream '(#x01 #x02 #x03 #x04)))))
    (assert-typep 'b-tree-impl::b-tree-ref b-tree-ref)
    (assert-eql #x04030201 (b-tree-impl::b-tree-ref-file-position b-tree-ref))))

(define-test read-b-tree-item
  (let ((b-tree-item (b-tree-impl::read-b-tree-item (binary-file:make-binary-array-io-stream '(#x01 #x02 #x03 #x04 #x05 #x06 #x07 #x08)) #'little-endian:read-uint32 #'little-endian:read-uint32)))
    (assert-typep 'b-tree-impl::b-tree-item b-tree-item)
    (assert-eql #x04030201 (b-tree-impl::key b-tree-item))
    (assert-eql #x08070605 (b-tree-impl::value b-tree-item))))

(define-test read-b-tree-node-contents
  (let ((contents (b-tree-impl::read-node-contents (binary-file:make-binary-array-io-stream
                                       (list b-tree-impl::+b-tree-ref+  #x01 #x02 #x03 #x04
                                             b-tree-impl::+b-tree-item+ #x05 #x06 #x07 #x08  #x09 #x0a #x0b #x0c
                                             b-tree-impl::+b-tree-eof+))
                                      #'little-endian:read-uint32 #'little-endian:read-uint32)))
    (assert-typep 'cons contents)
    (assert-eql 2 (length contents))
    (assert-typep 'b-tree-impl::b-tree-ref (car contents))
    (assert-typep 'b-tree-impl::b-tree-item (cadr contents))
    (assert-eql #x04030201 (b-tree-impl::b-tree-ref-file-position (car contents)))
    (assert-eql #x08070605 (b-tree-impl::key (cadr contents)))
    (assert-eql #x0c0b0a09 (b-tree-impl::value (cadr contents)))))

(define-test read-node
  (let* ((btree (b-tree-impl:b-tree-create  (binary-file:make-binary-array-io-stream) :block-size 32))
         (btree-node (b-tree-impl::make-b-tree-node :key-count 3
                                                    :child-count 3
                                                    :contents (list (b-tree-impl::make-b-tree-ref  :file-position 1)
                                                       (b-tree-impl::make-b-tree-item :key 2
                                                                         :value 2)
                                                       (b-tree-impl::make-b-tree-ref :file-position 3)
                                                       (b-tree-impl::make-b-tree-ref  :file-position 4)
                                                       (b-tree-impl::make-b-tree-item :key 5
                                                                         :value 5)
                                                       (b-tree-impl::make-b-tree-item :key 6
                                                                         :value 6))
                                       :file-position (b-tree-impl::allocate-node btree))))
    (b-tree-impl::write-node btree btree-node)
    (swap-file:flush (b-tree-impl::b-tree-swap-file btree))
    (let ((read-node (b-tree-impl::read-node btree (b-tree-impl::make-b-tree-ref :file-position (b-tree-impl::b-tree-node-file-position btree-node)))))
      (assert-typep 'b-tree-impl::b-tree-node read-node)
      (assert-equalp btree-node read-node))))

(define-test full-p
  (let ((btree (b-tree-impl:b-tree-create (binary-file:make-binary-array-io-stream) :block-size 32 :minimum-degree 2)))
    (assert-false (b-tree-impl::full-p (b-tree-impl::make-b-tree-node) btree))
  
    (assert-true (b-tree-impl::full-p (b-tree-impl::make-b-tree-node :key-count 3
                                        :contents (list (b-tree-impl::make-b-tree-ref  :file-position 1)
                                                        (b-tree-impl::make-b-tree-item :key 2
                                                                          :value 2)
                                                        (b-tree-impl::make-b-tree-ref :file-position 3)
                                                        (b-tree-impl::make-b-tree-ref  :file-position 4)
                                                        (b-tree-impl::make-b-tree-item :key 5
                                                                          :value 5)
                                                        (b-tree-impl::make-b-tree-item :key 6
                                                                          :value 6)))
                      btree))))


(define-test empty-p
  (assert-true (b-tree-impl::empty-p (b-tree-impl::make-b-tree-node)))
          
  (let ((btree-node (b-tree-impl::make-b-tree-node :key-count 3
                                      :contents (list (b-tree-impl::make-b-tree-ref :file-position 1)
                                                      (b-tree-impl::make-b-tree-item :key 2
                                                                        :value 2)
                                                      (b-tree-impl::make-b-tree-ref :file-position 3)
                                                      (b-tree-impl::make-b-tree-ref :file-position 4)
                                                      (b-tree-impl::make-b-tree-item :key 5
                                                                        :value 5)
                                                      (b-tree-impl::make-b-tree-item :key 6
                                                                        :value 6)))))
    (assert-false (b-tree-impl::empty-p btree-node))))

(define-test count-keys
  (let ((btree-node (b-tree-impl::make-b-tree-node :key-count 3
                                      :contents (list (b-tree-impl::make-b-tree-ref :file-position 1)
                                                      (b-tree-impl::make-b-tree-item :key 2
                                                                        :value 2)
                                                      (b-tree-impl::make-b-tree-ref :file-position 3)
                                                      (b-tree-impl::make-b-tree-ref :file-position 4)
                                                      (b-tree-impl::make-b-tree-item :key 5
                                                                        :value 5)
                                                      (b-tree-impl::make-b-tree-item :key 6
                                                                        :value 6)))))
    (assert-eql 3 (b-tree-impl::count-keys btree-node))))

(define-test count-children
  (let ((btree-node (b-tree-impl::make-b-tree-node :key-count 3
                                      :contents (list (b-tree-impl::make-b-tree-ref :file-position 1)
                                                      (b-tree-impl::make-b-tree-item :key 2
                                                                        :value 2)
                                                      (b-tree-impl::make-b-tree-ref :file-position 3)
                                                      (b-tree-impl::make-b-tree-ref :file-position 4)
                                                      (b-tree-impl::make-b-tree-item :key 5
                                                                        :value 5)
                                                      (b-tree-impl::make-b-tree-item :key 6
                                                                        :value 6)))))
    (assert-eql 3 (b-tree-impl::count-children btree-node))))

(define-test find-median-key-position
  (let ((btree-node (b-tree-impl::make-b-tree-node :key-count 0
                                      :contents nil)))
    (assert-false (b-tree-impl::find-median-key-position btree-node)))

  (let ((btree-node (b-tree-impl::make-b-tree-node :key-count 0
                                      :contents (list (b-tree-impl::make-b-tree-ref :file-position 1)))))
    (assert-false (b-tree-impl::find-median-key-position btree-node)))

  (let ((btree-node (b-tree-impl::make-b-tree-node :key-count 1
                                      :contents (list (b-tree-impl::make-b-tree-item :key 1
                                                                        :value 1)))))
    (assert-true (b-tree-impl::find-median-key-position btree-node))
    (assert-true (listp (b-tree-impl::find-median-key-position btree-node)))
    (assert-eql 1 (b-tree-impl::key (car (b-tree-impl::find-median-key-position btree-node)))))

  (let ((btree-node (b-tree-impl::make-b-tree-node :key-count 2
                                      :contents (list (b-tree-impl::make-b-tree-item :key 1
                                                                        :value 1)
                                                      (b-tree-impl::make-b-tree-item :key 2
                                                                        :value 2)))))
    (assert-true (b-tree-impl::find-median-key-position btree-node))
    (assert-true (listp (b-tree-impl::find-median-key-position btree-node)))
    (assert-eql 1 (b-tree-impl::key (car (b-tree-impl::find-median-key-position btree-node)))))

  (let ((btree-node (b-tree-impl::make-b-tree-node :key-count 3
                                      :contents (list (b-tree-impl::make-b-tree-item :key 1
                                                                        :value 1)
                                                      (b-tree-impl::make-b-tree-item :key 2
                                                                        :value 2)
                                                      (b-tree-impl::make-b-tree-item :key 3
                                                                        :value 3)))))
    (assert-true (b-tree-impl::find-median-key-position btree-node))
    (assert-true (listp (b-tree-impl::find-median-key-position btree-node)))
    (assert-eql 2 (b-tree-impl::key (car (b-tree-impl::find-median-key-position btree-node)))))


  (let ((btree-node (b-tree-impl::make-b-tree-node :key-count 3
                                      :contents (list (b-tree-impl::make-b-tree-ref :file-position 1)
                                                      (b-tree-impl::make-b-tree-item :key 2
                                                                        :value 2)
                                                      (b-tree-impl::make-b-tree-ref :file-position 3)
                                                      (b-tree-impl::make-b-tree-item :key 4
                                                                        :value 4)
                                                      (b-tree-impl::make-b-tree-ref :file-position 5)
                                                      (b-tree-impl::make-b-tree-item :key 6
                                                                        :value 6)))))
    (assert-true (b-tree-impl::find-median-key-position btree-node))
    (assert-true (listp (b-tree-impl::find-median-key-position btree-node)))
    (assert-eql 4 (b-tree-impl::key (car (b-tree-impl::find-median-key-position btree-node)))))

  (let ((btree-node (b-tree-impl::make-b-tree-node :key-count 5
                                      :contents (list (b-tree-impl::make-b-tree-item :key 1
                                                                        :value 2)
                                                      (b-tree-impl::make-b-tree-item :key 2
                                                                        :value 2)
                                                      (b-tree-impl::make-b-tree-item :key 3
                                                                        :value 3)
                                                      (b-tree-impl::make-b-tree-item :key 4
                                                                        :value 4)
                                                      (b-tree-impl::make-b-tree-item :key 5
                                                                        :value 5)))))

    (assert-true (b-tree-impl::find-median-key-position btree-node))
    (assert-true (listp (b-tree-impl::find-median-key-position btree-node)))
    (assert-eql 3 (b-tree-impl::key (car (b-tree-impl::find-median-key-position btree-node))))))

(define-test split-contents
  (let ((btree-node
         (b-tree-impl::make-b-tree-node :key-count 3
                           :contents (list (b-tree-impl::make-b-tree-item :key 1 :value 1)
                                           (b-tree-impl::make-b-tree-item :key 2 :value 1)
                                           (b-tree-impl::make-b-tree-item :key 3 :value 1)))))
    (multiple-value-bind (left middle right) (b-tree-impl::split-contents btree-node)
      (assert-eql 1 (b-tree-impl::key (car left)))
      (assert-eql 2 (b-tree-impl::key (car middle)))
      (assert-eql 3 (b-tree-impl::key (car right)))))

  (let ((btree-node
         (b-tree-impl::make-b-tree-node :key-count 5
                           :contents (list (b-tree-impl::make-b-tree-item :key 1 :value 1)
                                           (b-tree-impl::make-b-tree-item :key 2 :value 2)
                                           (b-tree-impl::make-b-tree-item :key 3 :value 3)
                                           (b-tree-impl::make-b-tree-item :key 4 :value 4)
                                           (b-tree-impl::make-b-tree-item :key 5 :value 5)))))
    (multiple-value-bind (left middle right) (b-tree-impl::split-contents btree-node)
      (assert-eql 1 (b-tree-impl::key (car left)))
      (assert-eql 2 (b-tree-impl::key (cadr left)))
      (assert-eql 3 (b-tree-impl::key (car middle)))
      (assert-eql 4 (b-tree-impl::key (car right)))
      (assert-eql 5 (b-tree-impl::key (cadr right))))))

(define-test split-root-node
  (let* ((btree (b-tree-impl:b-tree-create (binary-file:make-binary-array-io-stream) :block-size 32))
         (root (b-tree-impl::b-tree-root btree)))
    (setf (b-tree-impl::b-tree-node-key-count root) 5
          (b-tree-impl::b-tree-node-contents root) (list (b-tree-impl::make-b-tree-item :key 1 :value 1)
                                            (b-tree-impl::make-b-tree-item :key 2 :value 2)
                                            (b-tree-impl::make-b-tree-item :key 3 :value 3)
                                            (b-tree-impl::make-b-tree-item :key 4 :value 4)
                                            (b-tree-impl::make-b-tree-item :key 5 :value 5)))
    (b-tree-impl::split-root-node btree)
    (assert-eql 1 (b-tree-impl::b-tree-node-key-count (b-tree-impl::b-tree-root btree)))
    (assert-eql 2 (b-tree-impl::b-tree-node-child-count (b-tree-impl::b-tree-root btree)))
    (assert-false (b-tree-impl::leaf-p (b-tree-impl::b-tree-root btree)))
    (assert-typep 'b-tree-impl::b-tree-ref (first  (b-tree-impl::b-tree-node-contents (b-tree-impl::b-tree-root btree))))
    (assert-typep 'b-tree-impl::b-tree-item (second (b-tree-impl::b-tree-node-contents (b-tree-impl::b-tree-root btree))))
    (assert-typep 'b-tree-impl::b-tree-ref (third  (b-tree-impl::b-tree-node-contents (b-tree-impl::b-tree-root btree))))
    (assert-eql 3 (b-tree-impl::key (second (b-tree-impl::b-tree-node-contents (b-tree-impl::b-tree-root btree)))))
    (let ((left-node (b-tree-impl::read-node btree (first (b-tree-impl::b-tree-node-contents (b-tree-impl::b-tree-root btree))))))
      (assert-eql 2 (b-tree-impl::b-tree-node-key-count left-node))
      (assert-eql 0 (b-tree-impl::b-tree-node-child-count left-node))
      (assert-typep 'b-tree-impl::b-tree-item (first  (b-tree-impl::b-tree-node-contents left-node)))
      (assert-typep 'b-tree-impl::b-tree-item (second (b-tree-impl::b-tree-node-contents left-node)))
      (assert-eql 1 (b-tree-impl::key (first (b-tree-impl::b-tree-node-contents left-node))))
      (assert-eql 2 (b-tree-impl::key (second (b-tree-impl::b-tree-node-contents left-node)))))
    (let ((right-node (b-tree-impl::read-node btree (third (b-tree-impl::b-tree-node-contents (b-tree-impl::b-tree-root btree))))))
      (assert-eql 2 (b-tree-impl::b-tree-node-key-count right-node))
      (assert-eql 0 (b-tree-impl::b-tree-node-child-count right-node))
      (assert-typep 'b-tree-impl::b-tree-item (first  (b-tree-impl::b-tree-node-contents right-node)))
      (assert-typep 'b-tree-impl::b-tree-item (second (b-tree-impl::b-tree-node-contents right-node)))
      (assert-eql 4 (b-tree-impl::key (first (b-tree-impl::b-tree-node-contents right-node))))
      (assert-eql 5 (b-tree-impl::key (second (b-tree-impl::b-tree-node-contents right-node)))))
    (b-tree-impl:b-tree-close btree))

  (let ((btree (b-tree-impl:b-tree-create (binary-file:make-binary-array-io-stream) :block-size 32)))
    (unwind-protect
         (progn
           (dolist (v '((36 . 67) (90 . 57) (59 . 18) (10 . 47) (99 . 38) ))
             (b-tree-impl:b-tree-insert btree (car v) (cdr v)))
           (assert-true (b-tree-impl::full-p (b-tree-impl::b-tree-root btree) btree))
           (b-tree-impl::split-root-node btree)
           (assert-eql 1 (b-tree-impl::b-tree-node-key-count (b-tree-impl::b-tree-root btree)))
           (assert-eql 2 (b-tree-impl::b-tree-node-child-count (b-tree-impl::b-tree-root btree)))
           (assert-false (b-tree-impl::leaf-p (b-tree-impl::b-tree-root btree)))
           (assert-typep 'b-tree-impl::b-tree-ref (first  (b-tree-impl::b-tree-node-contents (b-tree-impl::b-tree-root btree))))
           (assert-typep 'b-tree-impl::b-tree-item (second (b-tree-impl::b-tree-node-contents (b-tree-impl::b-tree-root btree))))
           (assert-typep 'b-tree-impl::b-tree-ref (third  (b-tree-impl::b-tree-node-contents (b-tree-impl::b-tree-root btree))))
           (assert-eql 59 (b-tree-impl::key (second (b-tree-impl::b-tree-node-contents (b-tree-impl::b-tree-root btree)))))
           (let ((left-node (b-tree-impl::read-node btree (first (b-tree-impl::b-tree-node-contents (b-tree-impl::b-tree-root btree))))))
             (assert-eql 2 (b-tree-impl::b-tree-node-key-count left-node))
             (assert-eql 0 (b-tree-impl::b-tree-node-child-count left-node))
             (assert-typep 'b-tree-impl::b-tree-item (first  (b-tree-impl::b-tree-node-contents left-node)))
             (assert-typep 'b-tree-impl::b-tree-item (second (b-tree-impl::b-tree-node-contents left-node)))
             (assert-eql 10 (b-tree-impl::key (first (b-tree-impl::b-tree-node-contents left-node))))
             (assert-eql 36 (b-tree-impl::key (second (b-tree-impl::b-tree-node-contents left-node)))))
           (let ((right-node (b-tree-impl::read-node btree (third (b-tree-impl::b-tree-node-contents (b-tree-impl::b-tree-root btree))))))
             (assert-eql 2 (b-tree-impl::b-tree-node-key-count right-node))
             (assert-eql 0 (b-tree-impl::b-tree-node-child-count right-node))
             (assert-typep 'b-tree-impl::b-tree-item (first  (b-tree-impl::b-tree-node-contents right-node)))
             (assert-typep 'b-tree-impl::b-tree-item (second (b-tree-impl::b-tree-node-contents right-node)))
             (assert-eql 90 (b-tree-impl::key (first (b-tree-impl::b-tree-node-contents right-node))))
             (assert-eql 99 (b-tree-impl::key (second (b-tree-impl::b-tree-node-contents right-node)))))
           (b-tree-impl:b-tree-insert btree 1 1))
      (b-tree-impl:b-tree-close btree))))

(define-test find-child-pos
  (let ((contents (list (b-tree-impl::make-b-tree-ref :file-position 1)
                        (b-tree-impl::make-b-tree-item :key 2
                                          :value 2)
                        (b-tree-impl::make-b-tree-ref :file-position 3)
                        (b-tree-impl::make-b-tree-ref :file-position 4)
                        (b-tree-impl::make-b-tree-item :key 5
                                          :value 5)
                        (b-tree-impl::make-b-tree-item :key 6
                                          :value 6))))
    (assert-false (b-tree-impl::find-child-pos 7 contents))
    (assert-false (b-tree-impl::find-child-pos 2 contents))

    (assert-typep 'cons (b-tree-impl::find-child-pos 1 contents))
    (assert-typep 'b-tree-impl::b-tree-ref (car (b-tree-impl::find-child-pos 1 contents)))
    (assert-eql 1 (b-tree-impl::b-tree-ref-file-position (car (b-tree-impl::find-child-pos 1 contents))))

    (assert-typep 'cons (b-tree-impl::find-child-pos 4 contents))
    (assert-typep 'b-tree-impl::b-tree-ref (car (b-tree-impl::find-child-pos 4 contents)))
    (assert-eql 4 (b-tree-impl::b-tree-ref-file-position (car (b-tree-impl::find-child-pos 4 contents))))))

;; (define-test get-insert-position
;;   (let ((btree-node (b-tree-impl::make-b-tree-node :contents (list (b-tree-impl::make-b-tree-ref :file-position 2)
;;                                                       (b-tree-impl::make-b-tree-item :key 10 :value 10)
;;                                                       (b-tree-impl::make-b-tree-ref :file-position 3)))))
;;     (assert-true (b-tree-impl:b-tree-ref-p (car (get-insert-position btree-node 1))))
;;     (assert-eql 2 (b-tree-impl::b-tree-ref-file-position (car (get-insert-position btree-node 1))))
;;     (assert-false (get-insert-position btree-node 3)))

;;   (let ((btree-node (b-tree-impl::make-b-tree-node :key-count 3
;;                                       :contents (list (b-tree-impl::make-b-tree-ref :file-position 2)
;;                                                       (b-tree-impl::make-b-tree-item :key 4
;;                                                                         :value 4)
;;                                                       (b-tree-impl::make-b-tree-ref :file-position 6)
;;                                                       (b-tree-impl::make-b-tree-item :key 8
;;                                                                         :value 8)
;;                                                       (b-tree-impl::make-b-tree-item :key 10
;;                                                                         :value 10)))))
;;     (assert-true (b-tree-impl:b-tree-ref-p (get-insert-position btree-node 1)))
;;     (assert-true (get-insert-position btree-node 3))
;;     (assert-eql 4 (b-tree-impl::key (car (get-insert-position btree-node 3))))
;;     (assert-true (get-insert-position btree-node 5))
;;     (assert-eql 6 (b-tree-impl::key (car (get-insert-position btree-node 5))))
;;     (assert-true (get-insert-position btree-node 7))
;;     (assert-eql 8 (b-tree-impl::key (car (get-insert-position btree-node 7))))))


(define-test insert-item
  (let ((btree-node (b-tree-impl::make-b-tree-node)))
    (b-tree-impl::insert-item btree-node (b-tree-impl::make-b-tree-item :key 1 :value 1))
    (assert-eql 1 (b-tree-impl::b-tree-node-key-count btree-node))
    (assert-eql 1 (length (b-tree-impl::b-tree-node-contents btree-node)))
    (assert-eql 1 (b-tree-impl::key (car (b-tree-impl::b-tree-node-contents btree-node))))
    (b-tree-impl::insert-item btree-node (b-tree-impl::make-b-tree-item :key 3 :value 3))
    (assert-eql 2 (b-tree-impl::b-tree-node-key-count btree-node))
    (assert-eql 2 (length (b-tree-impl::b-tree-node-contents btree-node)))
    (assert-eql 1 (b-tree-impl::key (car (b-tree-impl::b-tree-node-contents btree-node))))
    (assert-eql 3 (b-tree-impl::key (cadr (b-tree-impl::b-tree-node-contents btree-node))))
    (b-tree-impl::insert-item btree-node (b-tree-impl::make-b-tree-item :key 2 :value 2))
    (assert-eql 3 (b-tree-impl::b-tree-node-key-count btree-node))
    (assert-eql 3 (length (b-tree-impl::b-tree-node-contents btree-node)))
    (assert-eql 1 (b-tree-impl::key (car (b-tree-impl::b-tree-node-contents btree-node))))
    (assert-eql 2 (b-tree-impl::key (cadr (b-tree-impl::b-tree-node-contents btree-node))))
    (assert-eql 3 (b-tree-impl::key (caddr (b-tree-impl::b-tree-node-contents btree-node)))))

  (let ((btree-node (b-tree-impl::make-b-tree-node)))
    (b-tree-impl::insert-item btree-node (b-tree-impl::make-b-tree-item :key 2 :value 2))
    (b-tree-impl::insert-item btree-node (b-tree-impl::make-b-tree-item :key 5 :value 5))
    (assert-eql 2 (b-tree-impl::b-tree-node-key-count btree-node))
    (assert-eql 2 (length (b-tree-impl::b-tree-node-contents btree-node)))
    (assert-eql 2 (b-tree-impl::key (first (b-tree-impl::b-tree-node-contents btree-node))))
    (assert-eql 5 (b-tree-impl::key (second  (b-tree-impl::b-tree-node-contents btree-node))))))

(define-test find-previous
  (assert-false (b-tree-impl::find-previous (b-tree-impl::make-b-tree-item :key 1 :value 1) nil))
  (let ((item1 (b-tree-impl::make-b-tree-item :key 1 :value 1))
        (item2 (b-tree-impl::make-b-tree-item :key 2 :value 2))
        (item3 (b-tree-impl::make-b-tree-item :key 3 :value 3)))
    (assert-false (b-tree-impl::find-previous item1 (list item2)))
    (assert-false (b-tree-impl::find-previous item2 (list item2)))
    (assert-eq item1 (car (b-tree-impl::find-previous item2 (list item1 item2))))
    (assert-false (b-tree-impl::find-previous item2 (list item2 item1)))
    (assert-false (b-tree-impl::find-previous item2 (list item2 item1 item3)))
    (assert-eq item1 (car (b-tree-impl::find-previous item2 (list item1 item2 item3))))
    (assert-eq item1 (car (b-tree-impl::find-previous item2 (list item3 item1 item2))))))

(define-test find-correct-child
  (let ((btree-node (b-tree-impl::make-b-tree-node)))
    (assert-false (b-tree-impl::find-correct-child btree-node (b-tree-impl::make-b-tree-item :key 1 :value 1))))
  (let ((btree-node (b-tree-impl::make-b-tree-node :key-count 1
                                      :contents (list (b-tree-impl::make-b-tree-item :key 10 :value 10)))))
    (assert-false (b-tree-impl::find-correct-child btree-node (b-tree-impl::make-b-tree-item :key 1 :value 1))))
  (let ((btree-node (b-tree-impl::make-b-tree-node :key-count 1
                                      :contents (list
                                                 (b-tree-impl::make-b-tree-ref :file-position 0)
                                                 (b-tree-impl::make-b-tree-item :key 10 :value 10))))
        (result nil))
    (setq result (b-tree-impl::find-correct-child btree-node (b-tree-impl::make-b-tree-item :key 1 :value 1)))
    (assert-typep 'b-tree-impl::b-tree-ref result))

  (let ((btree-node (b-tree-impl::make-b-tree-node :key-count 1
                                      :contents (list
                                                 (b-tree-impl::make-b-tree-ref :file-position 0)
                                                 (b-tree-impl::make-b-tree-item :key 10 :value 10))))
        (result nil))
    (setq result (b-tree-impl::find-correct-child btree-node (b-tree-impl::make-b-tree-item :key 6 :value 1)))
    (assert-typep 'b-tree-impl::b-tree-ref result))

  (let ((btree-node (b-tree-impl::make-b-tree-node :key-count 2
                                      :contents (list
                                                 (b-tree-impl::make-b-tree-ref  :file-position 1)
                                                 (b-tree-impl::make-b-tree-item :key 10 :value 10)
                                                 (b-tree-impl::make-b-tree-ref  :file-position 2)
                                                 (b-tree-impl::make-b-tree-item :key 20 :value 10)
                                                 (b-tree-impl::make-b-tree-ref  :file-position 3))))
        (result nil))
    (setq result (b-tree-impl::find-correct-child btree-node (b-tree-impl::make-b-tree-item :key 9 :value 1)))
    (assert-typep 'b-tree-impl::b-tree-ref result)
    (assert-eql 1 (b-tree-impl::b-tree-ref-file-position result))
      
    (setq result (b-tree-impl::find-correct-child btree-node (b-tree-impl::make-b-tree-item :key 10 :value 1)))
    (assert-typep 'b-tree-impl::b-tree-ref result)
    (assert-eql 1 (b-tree-impl::b-tree-ref-file-position result))
    (setq result (b-tree-impl::find-correct-child btree-node (b-tree-impl::make-b-tree-item :key 11 :value 1)))
    (assert-typep 'b-tree-impl::b-tree-ref result)
    (assert-eql 2 (b-tree-impl::b-tree-ref-file-position result))

    (setq result (b-tree-impl::find-correct-child btree-node (b-tree-impl::make-b-tree-item :key 20 :value 1)))
    (assert-typep 'b-tree-impl::b-tree-ref result)
    (assert-eql 2 (b-tree-impl::b-tree-ref-file-position result))
    (setq result (b-tree-impl::find-correct-child btree-node (b-tree-impl::make-b-tree-item :key 21 :value 1)))
    (assert-typep 'b-tree-impl::b-tree-ref result)
    (assert-eql 3 (b-tree-impl::b-tree-ref-file-position result))))

(define-test b-tree-basics
  (let ((btree (b-tree-impl:b-tree-create (binary-file:make-binary-array-io-stream) :block-size 32)))
    (b-tree-impl:b-tree-insert btree 1 1)
    (b-tree-impl:b-tree-insert btree 131 131)
    (b-tree-impl:b-tree-insert btree 151 151)
    (b-tree-impl:b-tree-insert btree 171 171)
    (b-tree-impl:b-tree-insert btree 191 191)
    (assert-eql 1 (b-tree-impl:b-tree-search btree 1))
    (assert-false (b-tree-impl:b-tree-search btree 2))
    (b-tree-impl:b-tree-close btree))
  ;;    (format-b-tree-blocks (binary-file:binary-array (swap-file:swap-file-stream (b-tree-impl::b-tree-swap-file btree))) 32))

  (let ((btree (b-tree-impl:b-tree-create (binary-file:make-binary-array-io-stream) :block-size 32))
        (saved-data nil))
    (assert-true (b-tree-impl::leaf-p (b-tree-impl::b-tree-root btree)))
    (assert-eql 0 (b-tree-impl::b-tree-node-key-count (b-tree-impl::b-tree-root btree)))
    (assert-eql 0 (b-tree-impl::b-tree-node-child-count (b-tree-impl::b-tree-root btree)))
    ;;(assert-true (open-stream-p (b-tree-impl::b-tree-swap-file btree)))
    (dotimes (i 4)
      (b-tree-impl:b-tree-insert btree (1+ i) (1+ i)))
    (dotimes (i 4)
      (assert-eql (1+ i) (b-tree-impl:b-tree-search btree (1+ i))))
    (setq saved-data (binary-file:binary-array (swap-file:swap-file-stream (b-tree-impl::b-tree-swap-file btree))))
    (b-tree-impl:b-tree-close btree)
    (setq btree (b-tree-impl:b-tree-open (binary-file:make-binary-array-io-stream saved-data)))
    (assert-true (b-tree-impl::leaf-p (b-tree-impl::b-tree-root btree)))
    (assert-eql 4 (b-tree-impl::b-tree-node-key-count (b-tree-impl::b-tree-root btree)))
    (assert-eql 0 (b-tree-impl::b-tree-node-child-count (b-tree-impl::b-tree-root btree)))
    ;;(assert-true (open-stream-p (b-tree-impl::b-tree-swap-file btree)))
    (dotimes (i 4)
      (assert-eql (1+ i) (b-tree-impl:b-tree-search btree (1+ i))))
    (b-tree-impl:b-tree-close btree))
  
  (let ((btree (b-tree-impl:b-tree-create (binary-file:make-binary-array-io-stream) :block-size 32)))
    (assert-true (b-tree-impl::leaf-p (b-tree-impl::b-tree-root btree)))
    (assert-eql 0 (b-tree-impl::b-tree-node-key-count (b-tree-impl::b-tree-root btree)))
    (assert-eql 0 (b-tree-impl::b-tree-node-child-count (b-tree-impl::b-tree-root btree)))
    ;;(assert-true (open-stream-p (b-tree-impl::b-tree-swap-file btree)))
    (dotimes (i 7)
      (b-tree-impl:b-tree-insert btree (1+ i) (1+ i)))
    (dotimes (i 7)
      (assert-eql (1+ i) (b-tree-impl:b-tree-search btree (1+ i))))
    (b-tree-impl:b-tree-close btree)
    (setq btree (b-tree-impl:b-tree-open (binary-file:make-binary-array-io-stream (binary-file:binary-array (swap-file:swap-file-stream (b-tree-impl::b-tree-swap-file btree))))))
    (assert-false (b-tree-impl::leaf-p (b-tree-impl::b-tree-root btree)))
    (assert-eql 1 (b-tree-impl::b-tree-node-key-count (b-tree-impl::b-tree-root btree)))
    (assert-eql 2 (b-tree-impl::b-tree-node-child-count (b-tree-impl::b-tree-root btree)))
    ;;(assert-true (open-stream-p (b-tree-impl::b-tree-swap-file btree)))
    (dotimes (i 7)
      (assert-eql (1+ i) (b-tree-impl:b-tree-search btree (1+ i))))
    (b-tree-impl:b-tree-close btree))

  (let ((btree (b-tree-impl:b-tree-create (binary-file:make-binary-array-io-stream) :block-size 32)))
    (dotimes (i 200)
      (b-tree-impl:b-tree-insert btree (1+ i) (1+ i)))
    (dotimes (i 200)
      (assert-eql (1+ i) (b-tree-impl:b-tree-search btree (1+ i))))
    (b-tree-impl:b-tree-close btree))

  (let ((btree (b-tree-impl:b-tree-create (binary-file:make-binary-array-io-stream) :block-size 32)))
    (unwind-protect
         (progn
           (dolist (v '((13 . 11) (40 . 94) (60 . 50) (2 . 94) (87 . 31) (7 . 37) (90 . 61)
                        (68 . 87) (6 . 34)))
             (b-tree-impl:b-tree-insert btree (car v) (cdr v)))
           (dolist (v '((13 . 11) (40 . 94) (60 . 50) (2 . 94) (87 . 31) (7 . 37) (90 . 61)
                        (68 . 87) (6 . 34)))
             (assert-eql (cdr v) (b-tree-impl:b-tree-search btree (car v)))))
      (b-tree-impl:b-tree-close btree)))
  
  (let ((btree (b-tree-impl:b-tree-create (binary-file:make-binary-array-io-stream) :block-size 32)))
    (unwind-protect
         (progn
           (dolist (v '((36 . 67) (90 . 57) (59 . 18) (10 . 47) (99 . 38)  (1 . 42)))
             ;;(b-tree-print btree)
             (b-tree-impl:b-tree-insert btree (car v) (cdr v)))
           (dolist (v '((36 . 67) (90 . 57) (59 . 18) (10 . 47) (99 . 38) (1 . 42)))
             (assert-eql (cdr v) (b-tree-impl:b-tree-search btree (car v))))
           (b-tree-impl:b-tree-close btree)))))

(define-test map-b-tree
  (let ((btree (b-tree-impl:b-tree-create (binary-file:make-binary-array-io-stream) :block-size 32)))
    (unwind-protect
         (progn
           (dotimes (i 7)
             (b-tree-impl:b-tree-insert btree (1+ i) (1+ i)))
           (assert-equalp '((1 . 1) (2 . 2) (3 . 3) (4 . 4) (5 . 5) (6 . 6) (7 . 7))
                          (b-tree-impl:map-b-tree 'list btree #'cons)))
      (b-tree-impl:b-tree-close btree)))
  

  (let ((btree (b-tree-impl:b-tree-create (binary-file:make-binary-array-io-stream) :block-size 32))
        (values nil))
    (unwind-protect
         (progn
           (dotimes (i 10)
             (let ((k (1+ (random 100)))
                   (v (1+ (random 100))))
               
               (when (null (b-tree-impl:b-tree-search btree k))
                 (push (cons k v) values)
                 ;;(print (reverse values))
                 (b-tree-impl:b-tree-insert btree k v))))
           (assert-true (apply #'< (mapcar #'car (b-tree-impl:map-b-tree 'list btree #'cons))))
           (b-tree-impl:b-tree-close btree)))))

(define-test string-b-tree-basics
  (let ((btree (b-tree-impl:string-b-tree-create (binary-file:make-binary-array-io-stream) :block-size 32)))
    (unwind-protect
         (progn
           (b-tree-impl:b-tree-insert btree 'foo 'bar)
           (b-tree-impl:b-tree-insert btree "hello" "there")
           (assert-eql 'bar (b-tree-impl:b-tree-search btree 'foo))
           (assert-equal "there" (b-tree-impl:b-tree-search btree "hello"))
           (assert-false (b-tree-impl:b-tree-search btree "hello2"))
           (assert-false (b-tree-impl:b-tree-search btree 'foo2)))
      (b-tree-impl:b-tree-close btree))))

(define-test search-from-contents
  (assert-false (b-tree-impl::search-from-contents nil 1))
  (assert-false (b-tree-impl::search-from-contents (list (b-tree-impl::make-b-tree-item :key 11 :value 11)) 10))
  (assert-false (b-tree-impl::search-from-contents (list (b-tree-impl::make-b-tree-item :key 11 :value 11) (b-tree-impl::make-b-tree-ref :file-position 1)) 10))
  (assert-false (b-tree-impl::search-from-contents (list (b-tree-impl::make-b-tree-item :key 9 :value 9)) 10))

  (let ((correct (b-tree-impl::make-b-tree-item :key 10 :value 10)))
    (assert-eq correct (first (b-tree-impl::search-from-contents (list correct) 10)))
    (assert-eq correct (first (b-tree-impl::search-from-contents (list (b-tree-impl::make-b-tree-item :key 1 :value 1) correct) 10)))
    (assert-eq correct (first (b-tree-impl::search-from-contents (list correct (b-tree-impl::make-b-tree-item :key 11 :value 11)) 10)))
    (assert-eq correct (first (b-tree-impl::search-from-contents (list (b-tree-impl::make-b-tree-ref  :file-position 1) correct) 10)))
    (assert-eq correct (first (b-tree-impl::search-from-contents (list correct (b-tree-impl::make-b-tree-ref  :file-position 1)) 10))))

  (let ((correct (b-tree-impl::make-b-tree-ref :file-position 1234)))
    (assert-eq correct (first (b-tree-impl::search-from-contents (list correct) 10)))
    (assert-eq correct (first (b-tree-impl::search-from-contents (list correct (b-tree-impl::make-b-tree-item :key 11 :value 11)) 10)))
    (assert-eq correct (first (b-tree-impl::search-from-contents (list (b-tree-impl::make-b-tree-ref :file-position 2) correct (b-tree-impl::make-b-tree-item :key 11 :value 11)) 10)))
    (assert-eq correct (first (b-tree-impl::search-from-contents (list (b-tree-impl::make-b-tree-item :key 9 :value 9) correct (b-tree-impl::make-b-tree-item :key 11 :value 11)) 10)))))


(define-test b-tree-max
   (let ((btree (b-tree-impl:b-tree-create (binary-file:make-binary-array-io-stream) :block-size 32)))
     (dotimes (i 100)
       (b-tree-impl:b-tree-insert btree (1+ i) (1+ i)))
     (assert-eql 100 (b-tree-impl:b-tree-max btree))))

 (define-test b-tree-min
   (let ((btree (b-tree-impl:b-tree-create (binary-file:make-binary-array-io-stream) :block-size 32)))
     (dotimes (i 100)
       (b-tree-impl:b-tree-insert btree (1+ i) (1+ i)))
     (assert-eql 1 (b-tree-impl:b-tree-min btree))))

(define-test delete-from-contents
  (let ((node (b-tree-impl::make-b-tree-node :key-count 3
                                :child-count 3
                                :contents (list (b-tree-impl::make-b-tree-ref  :file-position 1)
                                                (b-tree-impl::make-b-tree-item :key 2
                                                                  :value 2)
                                                (b-tree-impl::make-b-tree-ref :file-position 3)
                                                (b-tree-impl::make-b-tree-ref  :file-position 4)
                                                (b-tree-impl::make-b-tree-item :key 5
                                                                  :value 5)
                                                (b-tree-impl::make-b-tree-item :key 6
                                                                  :value 6))
                                :file-position 0)))
    (assert-eq node (b-tree-impl::delete-from-contents node (find-if #'(lambda (v)
                                                            (and (b-tree-impl::b-tree-item-p v) (= (b-tree-impl::key v) 2)))
                                                        (b-tree-impl::b-tree-node-contents node))))
    (assert-eql 5 (length (b-tree-impl::b-tree-node-contents node)))
    (assert-eql 3 (b-tree-impl::b-tree-node-child-count node))
    (assert-eql 2 (b-tree-impl::b-tree-node-key-count node))
    (assert-false (find-if #'(lambda (v)
                             (and (b-tree-impl::b-tree-item-p v) (= (b-tree-impl::key v) 2)))
                         (b-tree-impl::b-tree-node-contents node)))))

(define-test write-root-offset
  (let ((btree (b-tree-impl:b-tree-create (binary-file:make-binary-array-io-stream) :block-size 32)))
    (b-tree-impl::allocate-node btree)))

(define-test remove-keyless-root
  ;; empty b-tree is not affected.
  (let* ((btree (b-tree-impl:b-tree-create (binary-file:make-binary-array-io-stream) :block-size 32))
         (old-root (b-tree-impl::b-tree-root btree)))
    (assert-false (b-tree-impl::keys-p (b-tree-impl::b-tree-root btree)))
    (b-tree-impl::remove-keyless-root btree)
    (assert-eq old-root (b-tree-impl::b-tree-root btree)))
  ;; keyless root with a child node
  ;;  a) child becomes root
  ;;  b) old root is deleted
  (let* ((btree (b-tree-impl:b-tree-create (binary-file:make-binary-array-io-stream) :block-size 32))
         (child-node (b-tree-impl::make-b-tree-node :file-position (b-tree-impl::allocate-node btree)))
         (old-root (b-tree-impl::b-tree-root btree)))
    (setf (b-tree-impl::b-tree-node-contents child-node) (list (b-tree-impl::make-b-tree-item :key 1 :value 1)
                                                  (b-tree-impl::make-b-tree-item :key 2 :value 2))
          (b-tree-impl::b-tree-node-key-count child-node) 2)
    (setf (b-tree-impl::b-tree-node-contents old-root) (list 
                                           (b-tree-impl::make-b-tree-ref :file-position (b-tree-impl::b-tree-node-file-position child-node))
                                           (b-tree-impl::make-b-tree-item :key 3 :value 3))
          (b-tree-impl::b-tree-node-child-count old-root) 1
          (b-tree-impl::b-tree-node-key-count old-root) 1)
    (b-tree-impl::write-node btree child-node)
    (b-tree-impl::write-node btree old-root)
    (swap-file:flush (b-tree-impl::b-tree-swap-file btree))
    ;;(format-b-tree-blocks btree)
    (b-tree-impl::delete-from-contents old-root 3)
    (assert-false (b-tree-impl::keys-p (b-tree-impl::b-tree-root btree)))
    (assert-false (b-tree-impl::empty-p (b-tree-impl::b-tree-root btree)))
    (b-tree-impl::remove-keyless-root btree)
    (assert-false (eq old-root (b-tree-impl::b-tree-root btree)))))

(define-test search-delete-position
  (assert-false (b-tree-impl::search-delete-position 1 nil (b-tree-impl::make-b-tree-node :key-count 1 :contents (list (b-tree-impl::make-b-tree-item :key 10 :value 10))))))

;;  (define-test delete-from-root
;;    (let ((btree (b-tree-impl:b-tree-create (binary-file:make-binary-array-io-stream) :block-size 32)))
;;      (b-tree-impl:b-tree-insert btree 123 321)
;;      (delete-from-root (car (b-tree-impl::b-tree-node-contents (b-tree-impl::b-tree-root btree))) btree)
;;      (swap-file:flush (b-tree-impl::b-tree-swap-file btree))
;;      (assert-true (b-tree-impl::empty-p (b-tree-impl::b-tree-root btree)))
;;      (assert-eql 0 (b-tree-impl::b-tree-node-key-count (b-tree-impl::b-tree-root btree)))
;;      (assert-eql 0 (b-tree-impl::b-tree-node-child-count (b-tree-impl::b-tree-root btree)))
;;      (assert-equalp
;;       #(#x53 #x57 #x41 #x50              ; "SWAP"
;;         #x01 #x00 #x00 #x00              ; swap-file version 1
;;         #x20 #x00 #x00 #x00              ; blocksize 
;;         #x40 #x00 #x00 #x00              ; next new offset
;;         #x00 #x00 #x00 #x00              ; available list header
;;         #x01 #x00 #x00 #x00              ; b-tree version 1
;;         #x03 #x00 #x00 #x00              ; minimum-degree
;;         #x20 #x00 #x00 #x00              ; root node offset

;;         ;; root node's block
;;         #x00                             ; block deleted-p
;;         #x00 #x00 #x00 #x00              ; next block offset
;;         #x01 #x00 #x00 #x00              ; data size
;;         #x00                             ; +b-tree-eof+
;;         ) ;; rest of data is truncated because of garbage
;;       (subseq 
;;        (binary-file:binary-array (swap-file::swap-file-stream (b-tree-impl::b-tree-swap-file btree)))
;;        0 (- (* 2 32) (1- (swap-file::max-data-size 32)))))))

(define-test delete-key
  (let ((btree (b-tree-impl:b-tree-create (binary-file:make-binary-array-io-stream) :block-size 32))
        (delete-from-root-called nil)
        (delete-from-leaf-called nil)
        (delete-from-internal-called nil))
    (mock:with-mock-functions ((b-tree-impl::delete-from-root  #'(lambda (key btree)
                                                      (declare (ignore key btree))
                                                      (setq delete-from-root-called t)))
                               (b-tree-impl::delete-from-leaf #'(lambda (key btree node)
                                                     (declare (ignore key btree node))
                                                     (setq delete-from-leaf-called t)))
                               (b-tree-impl::delete-from-internal #'(lambda (key btree node)
                                                         (declare (ignore key btree node))
                                                         (setq delete-from-internal-called t))))
      (assert-false (b-tree-impl::delete-key nil btree (b-tree-impl::make-b-tree-node :key-count 1 :contents (list (b-tree-impl::make-b-tree-item :key 10 :value 10)))))
      (assert-false delete-from-root-called)
      (assert-false delete-from-leaf-called)
      (assert-false delete-from-internal-called)

      (assert-false (b-tree-impl::delete-key 1 btree nil))
      (assert-false delete-from-root-called)
      (assert-false delete-from-leaf-called)
      (assert-false delete-from-internal-called)
      
      (b-tree-impl::delete-key 1 btree (b-tree-impl::b-tree-root btree))
      (assert-true delete-from-root-called)
      (assert-false delete-from-leaf-called)
      (assert-false delete-from-internal-called)

      (setq delete-from-root-called nil)
      (b-tree-impl::delete-key 1 btree (b-tree-impl::make-b-tree-node :key-count 1 :contents (list (b-tree-impl::make-b-tree-item :key 10 :value 10))))
      (assert-true delete-from-leaf-called)
      (assert-false delete-from-root-called)
      (assert-false delete-from-internal-called)

      (setq delete-from-leaf-called nil)
      (b-tree-impl::delete-key 1 btree (b-tree-impl::make-b-tree-node :key-count 1 :child-count 1 :contents (list (b-tree-impl::make-b-tree-item :key 10 :value 10)
                                                                                        (b-tree-impl::make-b-tree-ref :file-position 1))))
      (assert-true delete-from-internal-called)
      (assert-false delete-from-root-called)
      (assert-false delete-from-leaf-called))))

(define-test b-tree-delete
  (let ((btree (b-tree-impl:b-tree-create (binary-file:make-binary-array-io-stream) :block-size 64)))
    (b-tree-impl:b-tree-insert btree 1 1)
    (b-tree-impl:b-tree-delete btree 1)
    (assert-false (b-tree-impl:b-tree-search btree 1))
    (assert-false (b-tree-impl:b-tree-min btree))
    (assert-false (b-tree-impl:b-tree-max btree))
    (assert-false (b-tree-impl:map-b-tree 'list btree #'identity)))

  (let ((btree (b-tree-impl:b-tree-create (binary-file:make-binary-array-io-stream) :block-size 64)))
    (dotimes (i 100)
      (b-tree-impl:b-tree-insert btree (1+ i) (1+ i)))
    (b-tree-impl:b-tree-delete btree 1)
    (assert-false (b-tree-impl:b-tree-search btree 1))
    (assert-eql 2 (b-tree-impl:b-tree-min btree))
    (b-tree-impl:b-tree-delete btree 100)
    (assert-false (b-tree-impl:b-tree-search btree 100))
    (assert-eql 99 (b-tree-impl:b-tree-max btree))))



;; (define-test b-tree-basics-on-file
;;   (let ((btree (b-tree-impl:b-tree-create "/tmp/b-tree-test.bin" :if-exists :rename :block-size 32)))
;;     (unwind-protect
;;          (progn
;;            (b-tree-impl:b-tree-insert btree 1 1)
;;            (b-tree-impl:b-tree-insert btree 131 131)
;;            (b-tree-impl:b-tree-insert btree 151 151)
;;            (b-tree-impl:b-tree-insert btree 171 171)
;;            (b-tree-impl:b-tree-insert btree 191 191)
;;            (assert-eql 1 (b-tree-impl:b-tree-search btree 1))
;;            (assert-false (b-tree-impl:b-tree-search btree 2)))
;;       (b-tree-impl:b-tree-close btree)))
;;   (let ((btree (b-tree-impl:b-tree-open "/tmp/b-tree-test.bin")))
;;     (unwind-protect
;;          (progn
;;            (assert-eql 1 (b-tree-impl:b-tree-search btree 1))
;;            (assert-false (b-tree-impl:b-tree-search btree 2)))
;;       (b-tree-impl:b-tree-close btree))))

;; (define-test string-b-tree-basics-on-file
;;   (let ((btree (b-tree-impl:string-b-tree-create "/tmp/b-tree-test.bin" :if-exists :rename :block-size 32)))
;;     (unwind-protect
;;          (progn
;;            (b-tree-impl:b-tree-insert btree 'foo 'bar)
;;            (b-tree-impl:b-tree-insert btree "hello" "there")
;;            (assert-eql 'bar (b-tree-impl:b-tree-search btree 'foo))
;;            (assert-equal "there" (b-tree-impl:b-tree-search btree "hello"))
;;            (assert-false (b-tree-impl:b-tree-search btree "hello2"))
;;            (assert-false (b-tree-impl:b-tree-search btree 'foo2)))
;;       (b-tree-impl:b-tree-close btree))))

;;   ;;    (format-b-tree-blocks (binary-file:binary-array (swap-file:swap-file-stream (b-tree-impl::b-tree-swap-file btree))) 32))
