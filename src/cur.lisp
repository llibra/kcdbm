(in-package :kc.cur.base)

(define-condition error (kc.err:error) ())

(defun db (cur) (kccurdb cur))

(defun error (cur control &rest args)
  (let* ((db (db cur))
         (path (kc.db:path db))
         (code (foreign-enum-keyword 'error-code (kccurecode cur)))
         (message (foreign-string-to-lisp (kccuremsg cur))))
    (cl:error 'error :format-control control :format-arguments args
              :path path :code code :message message)))

(defun delete (cur)
  (kccurdel cur))

(defun jump (cur)
  (if (zerop (kccurjump cur))
      (error cur "Can't jump to the first record.")
      t))

(defmacro with-cursor ((var db) &body body)
  `(let ((,var (kc.db:cursor ,db)))
     (unwind-protect (progn (jump ,var) ,@body)
       (delete ,var))))
