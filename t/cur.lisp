(in-package :kc.test)

(5am:in-suite cur)

(5am:test db
  (with-new-db (db)
    (let ((cur (kc.db:cursor db)))
      (5am:is (= (cffi:pointer-address db)
                 (cffi:pointer-address (kc.cur:db cur))))
      (kc.cur:delete cur))))

(5am:test error
  (with-new-db (db)
    (let ((cur (kc.db:cursor db)))
      (5am:signals kc.cur:error (kc.cur:error cur "An error occured."))
      (5am:signals kc.cur:error (kc.cur:error cur "With an argument. ~a" 0))
      (handler-case (kc.cur:error cur "An error occured.")
        (kc.cur:error (c)
          (5am:is (equal (namestring *default-db-pathname*) (kc.err:path c)))
          (5am:is (eq :success (kc.err:code c)))
          (5am:is (equal "no error" (kc.err:message c)))))
      (kc.cur:delete cur))))

(5am:test new/delete
  (with-new-db (db)
    (5am:finishes
      (let ((c (kc.db:cursor db)))
        (5am:is (cffi:pointerp c))
        (kc.cur:delete c)))))

(5am:test jump
  (with-new-db (db)
    (let ((cur (kc.db:cursor db)))
      ;; Expect to signal kc.cur:error due to no record.
      (5am:signals kc.cur:error (kc.cur:jump cur))
      (kc.db:set db "x" "1")
      (5am:is-true (kc.cur:jump cur))
      (kc.cur:delete cur))))

(5am:test with-cursor
  (with-new-db (db)
    (kc.db:set db "set" "but not used")
    (5am:is (zerop (kc.cur:with-cursor (cur db) 0)))))

(labels ((value-and-length (fn)
           (with-new-db (db)
             (kc.db:set db "x" "1")
             (kc.cur:with-cursor (cur db)
               (kc.cur:jump cur)
               (multiple-value-bind (ptr len) (funcall fn cur t)
                 (kc.util:with-kcmalloced-pointer (ptr ptr)
                   (5am:is (equal "x" (cffi:foreign-string-to-lisp ptr))))
                 (5am:is (= 1 len))))))
         (step (fn)
           (with-new-db (db)
             (kc.db:set db "x" "1")
             (kc.cur:with-cursor (cur db)
               (kc.cur:jump cur)
               ;; Expect no record.
               (5am:signals kc.cur:error
                 (progn (kc.ffi:kcfree (funcall fn cur t))
                        (funcall fn cur t)))
               (kc.cur:jump cur)
               (5am:finishes
                 (progn (kc.ffi:kcfree (funcall fn cur nil))
                        (kc.ffi:kcfree (funcall fn cur t)))))))
         (get-x/low (fn)
           (value-and-length fn)
           (step fn)))
  (5am:test (get-key/low :compile-at :definition-time)
    (get-x/low #'kc.cur.low:get-key)))

(5am:test get-key
  (with-new-db (db)
    (kc.db:set db "x" "1")
    (kc.cur:with-cursor (cur db)
      (5am:is (equal "x" (kc.cur:get-key cur t)))
      (5am:signals kc.cur:error (kc.cur:get-key cur t))
      (kc.cur:jump cur)
      (5am:finishes (progn (kc.cur:get-key cur nil)
                           (kc.cur:get-key cur t))))))
