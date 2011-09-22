(in-package :common-lisp-user)

;;; Load cl-btree
;;;
;;; This is an utility for a cl-btree developer to load project.
;;; Later there should be possibility to use asdf-install to setup
;;; cl-btree and its dependencies properly for use with other
;;; projects.
;;; 
;;; Loader assumes you have all dependent projects checked out at the
;;; same level as cl-btree and that you have .systems -directory with
;;; links to dependent projects' ASDF-file.
;;;
;;; Your directory should look like this:
;;; <lisp source dir>/.systems
;;; <lisp source dir>/cl-btree
;;; <lisp source dir>/cl-swap-file
;;; <lisp source dir>/cl-wal
;;; <lisp source dir>/cl-binary-file
;;; <lisp source dir>/cl-unit-test
;;; <lisp source dir>/cl-toolbox
;;;

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun load-time-dir ()
    (make-pathname :directory (pathname-directory *load-pathname*)))
  
  (defun parent-dir (dir)
    (make-pathname :directory (butlast (pathname-directory dir))))
  
  (defun append-dir (parent dir)
    (make-pathname :directory (append (pathname-directory parent) (cdr (pathname-directory dir)))))
  
  (defun make-systems-dir ()
    (append-dir (parent-dir (load-time-dir)) (make-pathname :directory ".systems"))))

(defvar *systems-dir* (make-systems-dir))

(pushnew *systems-dir* asdf:*central-registry*)
(asdf:operate 'asdf:load-op 'b-tree-0.2)