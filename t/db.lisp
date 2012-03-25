(in-package :kc.test)

(5am:in-suite db)

(5am:test db/error
  (with-new-db (db)
    (5am:signals kc.db:error (kc.db:error db "An error occured."))
    (5am:signals kc.db:error (kc.db:error db "With an argument. ~a" 0))))

;;; TODO: Very poor. Need refactoring.
(5am:test db/set
  (with-new-db (db)
    (5am:finishes (kc.db:set db "John" "McCarthy"))
    (5am:finishes (kc.db:set db "ジョン" "マッカーシー"))))

(5am:test db/get
  (with-new-db (db)
    (kc.db:set db "x" "1")
    (5am:is (equal "1" (kc.db:get db "x")))))

(5am:test db/seize
  (with-new-db (db)
    (flet ((test (fn)
             (kc.db:set db "x" "1")
             (5am:finishes (funcall fn))
             (5am:signals kc.db:error (kc.db:get db "x"))))
      (test (lambda () (kc.db:get db "x" :remove t)))
      (test (lambda () (kc.db:seize db "x"))))))

(5am:test db/iterate
  (with-new-db (db)
    (kc.db:set db "x" "1")
    (kc.db:set db "y" "2")
    (kc.db:set db "z" "3")
    (kc.db.low:iterate db (cffi:callback rm))
    (5am:is (zerop (kc.db:count db)))))

(5am:test append
  (with-new-db (db)
    (kc.db:set db "x" "+")
    (kc.db:set db "x" "+" :method :append)
    (5am:is (equal "++" (kc.db:get db "x")))
    (kc.db:append db "x" "+")
    (5am:is (equal "+++" (kc.db:get db "x")))))

(5am:test db/increment
  (with-new-db (db)
    (5am:is (= 1 (kc.db:increment db "i" 1)))
    (5am:is (= 2 (kc.db:increment db "i" 1)))
    (5am:is (= 4 (kc.db:increment db "i" 2)))
    (5am:is (= 0.5d0 (kc.db:increment db "f" 0.5d0)))
    (5am:is (= 1.0d0 (kc.db:increment db "f" 0.5d0)))
    (5am:is (= 2.5d0 (kc.db:increment db "f" 1.5d0)))))

(5am:test db/cas
  (with-new-db (db)
    (kc.db:set db "x" "old")
    (kc.db:cas db "x" "old" "new")
    (5am:is (equal "new" (kc.db:get db "x")))))

(5am:test db/remove
  (with-new-db (db)
    (kc.db:set db "x" "1")
    (kc.db:set db "y" "2")
    (kc.db:remove db "x")
    (kc.db:remove db "y")
    (5am:is (zerop (kc.db:count db)))
    (5am:signals kc.db:error (kc.db:get db "x"))
    (5am:signals kc.db:error (kc.db:get db "y"))))

(5am:test db/clear
  (with-new-db (db)
    (kc.db:set db "x" "1")
    (kc.db:set db "y" "2")
    (kc.db:clear db)
    (5am:signals kc.db:error (kc.db:get db "x"))
    (5am:signals kc.db:error (kc.db:get db "y"))
    (5am:is (zerop (kc.db:count db)))))

(5am:test db/snapshot
  (let ((dest "test.kcss"))
    (with-new-db (db)
      (kc.db:set db "x" "1")
      (kc.db:set db "y" "2")
      (kc.db:dump-snapshot db dest)
      (let ((count (kc.db:count db)))
        (kc.db:clear db)
        (kc.db:load-snapshot db dest)
        (5am:is (equal "1" (kc.db:get db "x")))
        (5am:is (equal "2" (kc.db:get db "y")))
        (5am:is (= count (kc.db:count db)))))))

(5am:test db/count
  (with-new-db (db)
    (5am:is (zerop (kc.db:count db)))
    (kc.db:set db "x" "1")
    (5am:is (= (kc.db:count db) 1))
    (kc.db:set db "y" "2")
    (5am:is (= (kc.db:count db) 2))))

(5am:test db/size
  (5am:is (= (with-new-db (db)
               (kc.db:size db))
             (with-open-file (s *default-db-pathname*)
               (file-length s)))))

(5am:test db/merge
  (with-new-db (src1 "src1.kch")
    (kc.db:set src1 "x" "1")
    (with-new-db (src2 "src2.kch")
      (kc.db:set src2 "y" "2")
      (with-new-db (dest "dest.kch")
        (kc.db:merge dest (list src1 src2) :set)
        (5am:is (equal "1" (kc.db:get dest "x")))
        (5am:is (equal "2" (kc.db:get dest "y")))))))

(5am:test db/copy
  (with-new-db (src "src.kch")
    (kc.db:set src "x" "1")
    (kc.db:set src "y" "2")
    (kc.db:copy src "dest.kch")
    (with-db (dest "dest.kch")
      (5am:is (equal "1" (kc.db:get dest "x")))
      (5am:is (equal "2" (kc.db:get dest "y")))
      (5am:signals kc.db:error (kc.db:get dest "z"))
      (5am:is (= 2 (kc.db:count dest))))))
