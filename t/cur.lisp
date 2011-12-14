(in-package :kc.test)

(5am:in-suite cur)

(5am:test new/delete
  (with-io (db)
    (5am:finishes
      (let ((c (kc.db:cursor db)))
        (5am:is (cffi:pointerp c))
        (kc.cur:delete c)))))
