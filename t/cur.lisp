(in-package :kc.test)

(5am:in-suite cur)

(5am:test db
  (with-io (db)
    (let ((cur (kc.db:cursor db)))
      (5am:is (= (cffi:pointer-address db)
                 (cffi:pointer-address (kc.cur:db cur))))
      (kc.cur:delete cur))))

(5am:test error
  (with-io (db)
    (let ((cur (kc.db:cursor db)))
      (5am:signals kc.cur:error (kc.cur:error cur "An error occured."))
      (5am:signals kc.cur:error (kc.cur:error cur "With an argument. ~a" 0))
      (handler-case (kc.cur:error cur "An error occured.")
        (kc.cur:error (c)
          (5am:is (equal *test-db-name* (kc.err:path c)))
          (5am:is (eq :success (kc.err:code c)))
          (5am:is (equal "no error" (kc.err:message c)))))
      (kc.cur:delete cur))))

(5am:test new/delete
  (with-io (db)
    (5am:finishes
      (let ((c (kc.db:cursor db)))
        (5am:is (cffi:pointerp c))
        (kc.cur:delete c)))))
