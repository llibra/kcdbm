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

(defun accept (cur fn &key (opaque *null-pointer*) (writable t) step)
  (let ((writable (convert-to-foreign writable :boolean))
        (step (convert-to-foreign step :boolean)))
    (if (zerop (kccuraccept cur fn opaque writable step))
        (error cur "Can't accept the visitor function.")
        t)))

(defun set-value (cur buf len &key step)
  (let ((step (convert-to-foreign step :boolean)))
    (if (zerop (kccursetvalue cur buf len step))
        (error cur "Can't set the value of the current record.")
        t)))

(defun remove (cur)
  (if (zerop (kccurremove cur))
      (error cur "Can't remove the current record.")
      t))

(defun jump (cur)
  (if (zerop (kccurjump cur))
      (error cur "Can't jump to the first record.")
      t))

(defmacro with-cursor ((var db) &body body)
  `(let ((,var (kc.db:cursor ,db)))
     (unwind-protect (progn (jump ,var) ,@body)
       (delete ,var))))

(flet ((body (fn msg cur step)
         (let ((step (convert-to-foreign step :boolean)))
           (with-foreign-object (value-len 'size_t) 
             (aif/ptr (funcall fn cur value-len step)
                      (values it (mem-aref value-len 'size_t))
                      (error cur msg))))))
  (declare (inline body))
  (defun get-key (cur &key step)
    (body #'kccurgetkey "Can't get the key of the current record."
          cur step))
  (defun get-value (cur &key step)
    (body #'kccurgetvalue "Can't get the value of the current record."
          cur step)))

(defun get (cur &key step)
  (let ((step (convert-to-foreign step :boolean)))
    (with-foreign-objects ((key-len 'size_t)
                           (value-buf :pointer)
                           (value-len 'size_t))
      (aif/ptr (kccurget cur key-len value-buf value-len step)
               (values it
                       (mem-aref key-len 'size_t)
                       (mem-aref value-buf :pointer)
                       (mem-aref value-len 'size_t))
               (error cur "Can't get the data of the current record.")))))

(in-package :kc.cur)

(defun set-value (cur value &rest args)
  (with-allocated-foreign-string (buf len (x->foreign-string value))
    (apply #'kc.cur.base:set-value cur buf len args)))

(flet ((body (fn cur step as)
         (multiple-value-bind (value-ptr value-len) (funcall fn cur :step step)
           (with-kcmalloced-pointer (value-ptr value-ptr)
             (foreign-string->x as value-ptr value-len)))))
  (declare (inline body))
  (defun get-key (cur &key step (as :string))
    (body #'kc.cur.base:get-key cur step as))
  (defun get-value (cur &key step (as :string))
    (body #'kc.cur.base:get-value cur step as)))

(defun get (cur &key step (as :string))
  (let ((as (if (consp as) as (cons as as))))
    (multiple-value-bind (key-buf key-len value-buf value-len)
        (kc.cur.base:get cur :step step)
      (with-kcmalloced-pointers ((key-buf key-buf)
                                 (value-buf value-buf))
        (values (foreign-string->x (car as) key-buf key-len)
                (foreign-string->x (cdr as) value-buf value-len))))))
