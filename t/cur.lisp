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

(5am:test with-cursor/no-record
  (with-new-db (db)
    (5am:signals kc.cur:error
      (kc.cur:with-cursor (cur db) :not-evaluated))))

(5am:test with-cursor/return-value
  (with-new-db (db)
    (kc.db:set db "set" "but not used")
    (5am:is (zerop (kc.cur:with-cursor (cur db) 0)))))

(flet ((body (key value fn value-pred len-pred)
         (with-new-db (db)
           (kc.db:set db key value)
           (kc.cur:with-cursor (cur db)
             (multiple-value-bind (ptr len) (funcall fn cur t)
               (kc.util:with-kcmalloced-pointer (ptr ptr)
                 (let ((s (cffi:foreign-string-to-lisp ptr)))
                   (5am:is (funcall value-pred s))))
               (5am:is (funcall len-pred len)))))))
  (5am:test (get-key/low/value :compile-at :definition-time)
    (body "x" "1"
          #'kc.cur.low:get-key
          (lambda (v) (equal "x" v))
          (lambda (l) (= 1 l))))
  (5am:test (get-value/low/value :compile-at :definition-time)
    (body "x" "1"
          #'kc.cur.low:get-value
          (lambda (v) (equal "1" v))
          (lambda (l) (= 1 l)))))

(flet ((body (key value fn pred)
         (with-new-db (db)
           (kc.db:set db key value)
           (kc.cur:with-cursor (cur db)
             (5am:is (funcall pred (funcall fn cur)))))))
  (5am:test (get-key/value :compile-at :definition-time)
    (body "x" "1"
          (lambda (c) (kc.cur:get-key c nil))
          (lambda (v) (equal "x" v))))
  (5am:test (get-value/value :compile-at :definition-time)
    (body "x" "1"
          (lambda (c) (kc.cur:get-value c nil))
          (lambda (v) (equal "1" v)))))

(5am:test get/low/value
  (with-new-db (db)
    (kc.db:set db "x" "1")
    (kc.cur:with-cursor (cur db)
      (multiple-value-bind (key-buf key-len value-buf value-len)
          (kc.cur.low:get cur nil)
        (kc.util:with-kcmalloced-pointer (key-buf key-buf)
          (kc.util:with-kcmalloced-pointer (value-buf value-buf)
            (flet ((->string (buf len)
                     (cffi:foreign-string-to-lisp buf :count len)))
              (let ((key (->string key-buf key-len))
                    (value (->string value-buf value-len)))
                (5am:is (equal "x" key))
                (5am:is (equal "1" value))))))))))

(5am:test get/value
  (with-new-db (db)
    (kc.db:set db "x" "1")
    (kc.cur:with-cursor (cur db)
      (multiple-value-bind (key value) (kc.cur:get cur nil)
        (5am:is (equal "x" key))
        (5am:is (equal "1" value))))))

(flet ((body (fn)
         (with-new-db (db)
           (kc.db:set db "x" "1")
           (kc.cur:with-cursor (cur db)
             ;; Expect no record.
             (5am:signals kc.cur:error
               (progn (funcall fn cur t) (funcall fn cur t)))
             (kc.cur:jump cur)
             (5am:finishes
               (progn (funcall fn cur nil) (funcall fn cur t)))))))
  (5am:test (get-key/low/step :compile-at :definition-time)
    (body (lambda (cur step)
            (kc.ffi:kcfree (kc.cur.low:get-key cur step)))))
  (5am:test (get-value/low/step :compile-at :definition-time)
    (body (lambda (cur step)
            (kc.ffi:kcfree (kc.cur.low:get-value cur step)))))
  (5am:test (get/low/step :compile-at :definition-time)
    (body (lambda (cur step)
            (multiple-value-bind (kb kl vb vl) (kc.cur.low:get cur step)
              (declare (ignore kl vl))
              (kc.ffi:kcfree kb)
              (kc.ffi:kcfree vb)))))
  (5am:test (get-key/step :compile-at :definition-time)
    (body #'kc.cur:get-key))
  (5am:test (get-value/step :compile-at :definition-time)
    (body #'kc.cur:get-value))
  (5am:test (get/step :compile-at :definition-time)
    (body #'kc.cur:get)))
