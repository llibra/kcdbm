(in-package :kc.test)

(defmacro with-db ((var &optional (path *default-db-pathname*)) &body body)
  (once-only (path)
    `(let ((,path (namestring (translate-logical-pathname ,path))))
       (kc.db:with-db (,var ,path :reader :writer :create) ,@body))))

(defmacro with-new-db ((var &optional (path *default-db-pathname*)) &body body)
  (once-only (path)
    `(let ((,path (namestring (translate-logical-pathname ,path))))
       (kc.db:with-db (,var ,path :reader :writer :create :truncate) ,@body))))
