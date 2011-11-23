(in-package :kc.test)

(5am:in-suite db)

(defparameter *test-db-name* "test.kch")

(defmacro with-in ((var) &body body)
  `(kc.db:with-db (,var *test-db-name* :reader)
     ,@body))

(defmacro with-out ((var) &body body)
  `(kc.db:with-db (,var *test-db-name* :writer :create)
     ,@body))

(defmacro with-io ((var &optional (path *test-db-name*)) &body body)
  `(kc.db:with-db (,var ,path :reader :writer :create)
     ,@body))

;;; TODO: Very poor. Need refactoring.
(5am:test set
  (with-out (db)
    (5am:finishes (kc.db:set db "John" "McCarthy"))
    (5am:finishes (kc.db:set db "ジョン" "マッカーシー"))))

(5am:test get
  (with-io (db)
    (kc.db:set db "x" "1")
    (5am:is (equal "1" (kc.db:get db "x")))))

(5am:test seize
  (with-io (db)
    (flet ((test (fn)
             (kc.db:set db "x" "1")
             (5am:finishes (funcall fn))
             (5am:signals error (kc.db:get db "x"))))
      (test (lambda () (kc.db:get db "x" :remove-p t)))
      (test (lambda () (kc.db:seize db "x"))))))

(cffi:defcallback rm :pointer
    ((kbuf :pointer) (ksiz kc.ffi:size_t) (vbuf :pointer) (vsiz kc.ffi:size_t)
     (sp :pointer) (opq :pointer))
  (declare (ignore kbuf ksiz vbuf vsiz sp opq))
  kc.ffi:+kcvisremove+)

(5am:test iterate
  (with-io (db)
    (kc.db:clear db)
    (kc.db:set db "x" "1")
    (kc.db:set db "y" "2")
    (kc.db:set db "z" "3")
    (kc.db:iterate db (cffi:callback rm))
    (5am:is (zerop (kc.db:count db)))))

(5am:test append
  (with-io (db)
    (kc.db:set db "x" "+")
    (kc.db:set db "x" "+" :method :append)
    (5am:is (equal "++" (kc.db:get db "x")))
    (kc.db:append db "x" "+")
    (5am:is (equal "+++" (kc.db:get db "x")))))

(5am:test remove
  (with-io (db)
    (kc.db:clear db)
    (kc.db:set db "x" "1")
    (kc.db:set db "y" "2")
    (kc.db:remove db "x")
    (kc.db:remove db "y")
    (5am:is (zerop (kc.db:count db)))
    (5am:signals error (kc.db:get db "x"))
    (5am:signals error (kc.db:get db "y"))))

(5am:test clear
  (with-io (db)
    (kc.db:clear db)
    (kc.db:set db "x" "1")
    (kc.db:set db "y" "2")
    (kc.db:clear db)
    (5am:signals error (kc.db:get db "x"))
    (5am:signals error (kc.db:get db "y"))
    (5am:is (zerop (kc.db:count db)))))

(5am:test count
  (with-io (db)
    (kc.db:clear db)
    (5am:is (zerop (kc.db:count db)))
    (kc.db:set db "x" "1")
    (5am:is (= (kc.db:count db) 1))
    (kc.db:set db "y" "2")
    (5am:is (= (kc.db:count db) 2))))

(5am:test size
  (5am:is (= (with-io (db)
               (kc.db:size db))
             (with-open-file (s *test-db-name*)
               (file-length s)))))

(5am:test merge
  (with-io (src1 "src1.kch")
    (kc.db:clear src1)
    (kc.db:set src1 "x" "1")
    (with-io (src2 "src2.kch")
      (kc.db:clear src2)
      (kc.db:set src2 "y" "2")
      (with-io (dest "dest.kch")
        (kc.db:clear dest)
        (kc.db:merge dest (list src1 src2) :set)
        (5am:is (equal "1" (kc.db:get dest "x")))
        (5am:is (equal "2" (kc.db:get dest "y")))))))
