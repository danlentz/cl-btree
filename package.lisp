(in-package :common-lisp-user)

;;(require :unit-test)

(defpackage b-tree.implementation
  (:use :cl) ;;  :toolbox :unit-test
  (:nicknames :b-tree-impl)
  (:export #:b-tree
           #:b-tree-node
           #:b-tree-root
           #:map-b-tree
           #:b-tree-search
           #:b-tree-max
           #:b-tree-min
           #:b-tree-insert
           #:b-tree-create
           #:string-b-tree-create
           #:b-tree-open
           #:string-b-tree-open
           #:b-tree-close
           #:b-tree-print
           #:b-tree-delete))

(defpackage b-tree
  (:use :cl)
  (:shadow #:map
           #:search
           #:max
           #:min
           #:open
           #:close
           #:delete
           #:print)
  (:export #:map
           #:search
           #:max
           #:min
           #:insert
           #:open
           #:close
           #:print
           #:delete))
