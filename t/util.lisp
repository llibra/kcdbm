(in-package :kc.test)

(defmacro with-db ((var &optional (path *default-db-pathname*)) &body body)
  (once-only (path)
    `(let ((,path (namestring (translate-logical-pathname ,path))))
       (kc.db:with-db (,var ,path :reader :writer :create) ,@body))))

(defmacro with-new-db ((var &optional (path *default-db-pathname*)) &body body)
  (once-only (path)
    `(let ((,path (namestring (translate-logical-pathname ,path))))
       (kc.db:with-db (,var ,path :reader :writer :create :truncate) ,@body))))

(cffi:defcallback rm :pointer
    ((kbuf :pointer) (ksiz kc.ffi:size_t) (vbuf :pointer) (vsiz kc.ffi:size_t)
     (sp :pointer) (opq :pointer))
  (declare (ignore kbuf ksiz vbuf vsiz sp opq))
  kc.ffi:+kcvisremove+)

(cffi:defcallback nop :pointer
    ((kbuf :pointer) (ksiz kc.ffi:size_t) (vbuf :pointer) (vsiz kc.ffi:size_t)
     (sp :pointer) (opq :pointer))
  (declare (ignore kbuf ksiz vbuf vsiz sp opq))
  kc.ffi:+kcvisnop+)
