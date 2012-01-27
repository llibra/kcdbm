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

(5am:test jump
  (with-io (db)
    (kc.db:clear db)
    (let ((cur (kc.db:cursor db)))
      ;; Expect to signal kc.cur:error due to no record.
      (5am:signals kc.cur:error (kc.cur:jump cur))
      (kc.db:set db "x" "1")
      (5am:is-true (kc.cur:jump cur))
      (kc.cur:delete cur))))

(5am:test with-cursor
  (with-io (db)
    (5am:is (zerop (kc.cur:with-cursor (cur db) 0)))))

(5am:test get-key/low
  (with-io (db)
    (kc.db:clear db)
    (kc.db:set db "x" "1")
    (kc.cur:with-cursor (cur db)
      (kc.cur:jump cur)
      (multiple-value-bind (ptr len) (kc.cur.low:get-key cur t)
        (kc.util:with-kcmalloced-pointer (ptr ptr)
          (5am:is (equal "x" (cffi:foreign-string-to-lisp ptr))))
        (5am:is (= 1 len)))
      ;; Expect no record.
      (5am:signals kc.cur:error (kc.cur.low:get-key cur t))
      (kc.cur:jump cur)
      (5am:finishes (progn (kc.ffi:kcfree (kc.cur.low:get-key cur nil))
                           (kc.ffi:kcfree (kc.cur.low:get-key cur t)))))))

(5am:test get-key
  (with-io (db)
    (kc.db:clear db)
    (kc.db:set db "x" "1")
    (kc.cur:with-cursor (cur db)
      (kc.cur:jump cur)
      (5am:is (equal "x" (kc.cur:get-key cur t)))
      (5am:signals kc.cur:error (kc.cur:get-key cur t))
      (kc.cur:jump cur)
      (5am:finishes (progn (kc.cur:get-key cur nil)
                           (kc.cur:get-key cur t))))))
