(in-package :kc.util)

(defmacro with-allocated-foreign-string ((var len-var form) &body body)
  `(multiple-value-bind (,var ,len-var) ,form
     (unwind-protect (progn ,@body) (foreign-free ,var))))

(defmacro with-allocated-foreign-strings (bindings &body body)
  (destructuring-bind (binding . rest) bindings
    (if rest
        `(with-allocated-foreign-string ,binding
           (with-allocated-foreign-strings ,rest ,@body))
        `(with-allocated-foreign-string ,binding ,@body))))

(defmacro with-kcmalloced-pointer ((var form) &body body)
  `(let ((,var ,form))
     (unwind-protect (progn ,@body)
       (kcfree ,var))))

(defmacro with-kcmalloced-pointers (bindings &body body)
  (destructuring-bind (binding . rest) bindings
    (if rest
        `(with-kcmalloced-pointer ,binding
           (with-kcmalloced-pointers ,rest ,@body))
        `(with-kcmalloced-pointer ,binding ,@body))))

(defmacro aif/ptr (test then &optional else)
  `(let ((it ,test))
     (if (null-pointer-p it) ,else ,then)))

(defun set-method->ffi-symbol (method)
  (find-symbol (format nil "KCDB~a" method) :kc.ffi))
