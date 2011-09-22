(in-package :common-lisp-user)

(defpackage :b-tree.system
  (:use :asdf :cl)
  (:export #:cl-btree-0.5))

(in-package :b-tree.system)

(defsystem cl-btree-0.5
   :name "Common Lisp B-tree."
   :description "B-tree is a trivial disk serialized B-tree implementation."
   :version "0.5"
   :author "Sami Makinen <sami.o.makinen@gmail.com>"
   :components ((:file "package")
                (:file "backtracking-list-iterator" :depends-on ("package"))
                (:file "b-tree" :depends-on ("backtracking-list-iterator"))
                (:file "debug" :depends-on ("b-tree"))
                (:file "api" :depends-on ("b-tree"))
                (:module "vendor" :components
                 ((:file "lisp-unit") (:file "mock")))
                (:module "test" :depends-on ("api" "vendor") :components
                 ((:module "unit" :components
                   ((:file "backtracking-list-iterator") 
                    (:file "b-tree-test")
                    (:file "api"))))))
   :depends-on (:cl-swap-file-0.5 :cl-binary-file-0.4))
