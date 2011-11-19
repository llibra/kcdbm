(in-package :kc.test)

(5am:in-suite db)

(defparameter *test-db-name* "test.kch")

(defmacro with-out ((var) &body body)
  `(kc.db:with-db (,var *test-db-name* :writer :create)
     ,@body))

(defmacro with-io ((var) &body body)
  `(kc.db:with-db (,var *test-db-name* :reader :writer :create)
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

(5am:test append
  (with-io (db)
    (kc.db:set db "x" "+")
    (kc.db:set db "x" "+" :method :append)
    (5am:is (equal "++" (kc.db:get db "x")))
    (kc.db:append db "x" "+")
    (5am:is (equal "+++" (kc.db:get db "x")))))
