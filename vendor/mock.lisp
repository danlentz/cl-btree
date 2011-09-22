(in-package :common-lisp-user)

(defpackage :unit-test.mock
  (:nicknames :mock)
  (:use :common-lisp :asdf)
  (:export #:with-mock-function
           #:with-mock-functions))
           
(in-package :unit-test.mock)

(defmacro with-mock-function ((function-name mock) &body body)
  (let ((old (gensym)))
    `(let ((,old (symbol-function ',function-name)))
      (unwind-protect 
           (progn
             (setf (symbol-function ',function-name) ,mock)
             ,@body)
        (setf (symbol-function ',function-name) ,old)))))
      
(defmacro with-mock-functions ((&rest function-name/mock-list) &body body)
  (let ((old (gensym))
        (names (gensym))
        (mocks (gensym)))
    `(let ((,old (make-hash-table))
           (,names ',(mapcar #'first function-name/mock-list))
           (,mocks (list ,@(mapcar #'second function-name/mock-list))))
      (unwind-protect 
           (progn
             (mapcar #'(lambda (function-name mock)
                         (setf (gethash function-name ,old) (symbol-function function-name))
                         (setf (symbol-function function-name) mock))
                     ,names ,mocks)
             ,@body)
        (maphash #'(lambda (function-name old-def)
                     (setf (symbol-function function-name) old-def))
                 ,old)))))
      