(in-package :kc.db.base)

(define-condition error (kc.err:error) ())

(defun path (db)
  "Returns the associated path of the database object DB."
  (with-kcmalloced-pointer (path-ptr (kcdbpath db))
    (foreign-string-to-lisp path-ptr)))

(defun error (db control &rest args)
  (let ((path (path db))
        (code (foreign-enum-keyword 'error-code (kcdbecode db)))
        (message (foreign-string-to-lisp (kcdbemsg db))))
    (cl:error 'error :format-control control :format-arguments args
              :path path :code code :message message)))

(defun new ()
  "Creates a polymorphic database object and returns it. The object must be
released with DELETE when it's no longer in use."
  (kcdbnew))

(defun delete (db)
  "Destroys the database object DB."
  (kcdbdel db))

(defun open (db path &rest mode)
  "Opens the database file specified by PATH and associates it with the database
object DB."
  (with-foreign-string (p path)
    (if (zerop (kcdbopen db p (foreign-bitfield-value 'open-mode mode)))
        (error db "Can't open the database file ~a." path)
        t)))

(defun close (db)
  "Closes the database file associated with the database object DB. Returns T if
succeed and signals an error otherwise."
  (if (zerop (kcdbclose db))
      (error db "Can't close the database file ~a.")
      t))

(defun accept (db key-buf key-len full-fn empty-fn &key (opaque *null-pointer*)
                                                        (writable t))
  (let ((writable (convert-to-foreign writable :boolean)))
    (if (zerop (kcdbaccept db key-buf key-len full-fn empty-fn opaque writable))
        (error db "Can't accept the visitor functions.")
        t)))

(defun iterate (db full-fn &key (opaque *null-pointer*) (writable t))
  (let ((writable (convert-to-foreign writable :boolean)))
    (if (zerop (kcdbiterate db full-fn opaque writable))
        (error db "The iteration for the records in the database failed.")
        t)))

(defun scan-in-parallel (db visitor n &key (opaque *null-pointer*))
  (if (zerop (kcdbscanpara db visitor opaque n))
      (error db "The parallel scanning for the database failed.")
      t))

(define-compiler-macro set (&whole form
                            db key-buf key-len value-buf value-len
                            &key (method :set)
                            &environment env)
  (once-only (db)
    (if (constantp method env)
        `(if (zerop (,(set-method->ffi-symbol method)
                      ,db ,key-buf ,key-len ,value-buf ,value-len))
             (error db "Can't set the value associated with the key.")
             t)
        form)))

(defun set (db key-buf key-len value-buf value-len &key (method :set))
  "Sets the value of the record whose key equals KEY-BUF in the database
associated with DB to VALUE-BUF. KEY-BUF and VALUE-BUF are CFFI foreign strings
and KEY-LEN and VALUE-LEN are the lengths of them. 

METHOD is a method to update the value of a record. It should be one of :SET,
:ADD, :REPLACE or :APPEND. Each method corresponds to kcdbset, kcdbadd,
kcdbreplace, or kcdbappend.

If succeeds to set a value, T is returned. Otherwise, an error occurs."
  (if (zerop (funcall (set-method->ffi-symbol method)
                      db key-buf key-len value-buf value-len))
      (error db "Can't set the value associated with the key.")
      t))

(defun increment (db key-buf key-len n &key (origin 0))
  (let ((new (kcdbincrint db key-buf key-len n origin)))
    (if (= new +int64-min+)
        (error db "Can't increment the value of the record.")
        new)))

(defun increment/double (db key-buf key-len n &key (origin 0.0d0))
  (let ((new (kcdbincrdouble db key-buf key-len n origin)))
    (flet ((err ()
             (error db "Can't increment the value of the record.")))
      ;; check whether the returned value is NaN
      (handler-case (if (= new new) new (err))
        ;; if a floating point trap occurs
        (floating-point-invalid-operation (c)
          (declare (ignore c))
          (err))))))

(defun cas (db key-buf key-len old-buf old-len new-buf new-len)
  (if (zerop (kcdbcas db key-buf key-len old-buf old-len new-buf new-len))
      (error db "Can't perform compare-and-swap.")
      t))

(defun remove (db key-buf key-len)
  (if (zerop (kcdbremove db key-buf key-len))
      (error db "Can't remove the record.")
      t))

(defun get (db key-buf key-len &key remove)
  "Finds the record whose key equals KEY-BUF in the database associated with DB
and returns its value as a C foreign string and the length of it. If there's no
corresponding record, an error is signaled. KEY-BUF is a CFFI foreign string and
KEY-LEN is the length of it. The foreign string as the first return value should
be released with KC.FFI:KCFREE when it's no longer in use.

If REMOVE is true, the record is removed at the same time."
  (let ((fn (if remove #'kcdbseize #'kcdbget)))
    (with-foreign-object (value-len 'size_t)
      (aif/ptr (funcall fn db key-buf key-len value-len)
               (values it (mem-ref value-len 'size_t))
               (error db "Can't get the value associated with the key.")))))

(defun get/buffer (db key-buf key-len value-buf value-len)
  (let ((len (kcdbgetbuf db key-buf key-len value-buf value-len)))
    (if (= len -1)
        (error db "Can't get the value associated with the key.")
        len)))

(defun synchronize (db &key hard
                            (post-processor *null-pointer*)
                            (opaque *null-pointer*))
  (let ((hard (convert-to-foreign hard :boolean)))
    (if (zerop (kcdbsync db hard post-processor opaque))
        (error db "The synchronization of the database failed.")
        t)))

(defun occupy (db fn &key (writable t) (opaque *null-pointer*))
  (let ((writable (convert-to-foreign writable :boolean)))
    (if (zerop (kcdboccupy db writable fn opaque))
        (error db "The atomic and exclusive operation to the database failed.")
        t)))

(defun copy (db dest)
  (with-foreign-string (dest-buf dest)
    (if (zerop (kcdbcopy db dest-buf))
        (error db "Can't copy the database.")
        t)))

(defun begin-transaction (db &key (wait t) hard)
  (if (zerop (funcall (if wait #'kcdbbegintran #'kcdbbegintrantry)
                      db (convert-to-foreign hard :boolean)))
      (error db "Can't begin a transaction.")
      t))

(defun end-transaction (db &key (commit t))
  (if (zerop (kcdbendtran db (convert-to-foreign commit :boolean)))
      (error db "Can't end the current transaction.")
      t))

(defun clear (db)
  (if (zerop (kcdbclear db))
      (error db "Can't clear the database.")
      t))

(defun dump-snapshot (db dest)
  (with-foreign-string (dest-buf dest)
    (if (zerop (kcdbdumpsnap db dest-buf))
        (error db "Can't dump the records of the database into the file.")
        t)))

(defun load-snapshot (db src)
  (with-foreign-string (src-buf src)
    (if (zerop (kcdbloadsnap db src-buf))
        (error db "Can't load records from snapshot.")
        t)))

(defun count (db)
  (let ((n (kcdbcount db)))
    (if (= n -1)
        (error db "Can't count the records in the database.")
        n)))

(defun size (db)
  (let ((size (kcdbsize db)))
    (if (= size -1)
        (error db "Can't calculate the size of the database.")
        size)))

(defun status (db)
  (aif/ptr (kcdbstatus db)
           (with-kcmalloced-pointer (ptr it)
             (mapcar (lambda (line) (apply #'cons (split "\\t" line)))
                     (split "\\n" (foreign-string-to-lisp ptr))))
           (error db "Can't retrieve the miscellaneous status information.")))

(defun merge (db src mode)
  (let ((src-len (length src))
        (mode (foreign-enum-value 'merge-mode mode)))
    (with-foreign-object (src-ary :pointer src-len)
      (dotimes (n src-len)
        (setf (mem-aref src-ary :pointer n) (elt src n)))
      (if (zerop (kcdbmerge db src-ary src-len mode))
          (error db "Can't merge records from the databases.")
          t))))

(in-package :kc.db)

(defmacro with-db ((db filespec &rest args) &body body)
  `(let ((,db (new)))
     (unwind-protect (progn
                       (open ,db ,filespec ,@args)
                       (unwind-protect (progn ,@body)
                         (close ,db)))
       (delete ,db))))

(defun get (db key &key (as :string) remove)
  "Finds the record whose key equals KEY in the database associated with DB and
returns its value. If there's no corresponding record, an error occurs. The
return value is converted to an object of the type specified by AS.

If REMOVE is true, the record is removed at the same time."
  (with-allocated-foreign-string (key-buf key-len (x->foreign-string key))
    (multiple-value-bind (value-ptr value-len)
        (kc.db.base:get db key-buf key-len :remove remove)
      (with-kcmalloced-pointer (value-ptr value-ptr)
        (foreign-string->x as value-ptr value-len)))))

(defun seize (db key &key (as :string))
  (get db key :as as :remove t))

;;; For compiler macro expansion of KC.DB.FS:SET.
(define-compiler-macro set (db key value &rest args)
  `(with-allocated-foreign-strings
       ((key-buf key-len (x->foreign-string ,key))
        (value-buf value-len (x->foreign-string ,value)))
     (kc.db.base:set ,db key-buf key-len value-buf value-len ,@args)))

(defun set (db key value &rest args)
  "Sets the value of the record whose key equals KEY in the database associated
with DB to VALUE. If succeeds to set the value, T is returned. Otherwise, an
error is signaled.

METHOD is a method to update the value of a record. It should be one of :SET,
:ADD, :REPLACE or :APPEND. Each method corresponds to kcdbset, kcdbadd,
kcdbreplace, or kcdbappend."
  (with-allocated-foreign-strings
      ((key-buf key-len (x->foreign-string key))
       (value-buf value-len (x->foreign-string value)))
    (apply #'kc.db.base:set db key-buf key-len value-buf value-len args)))

(defun add (db key value)
  "Corresponds to kcdbadd. A wrapper of SET."
  (set db key value :method :add))

(defun replace (db key value)
  "Corresponds to kcdbreplace. A wrapper of SET."
  (set db key value :method :replace))

(defun append (db key value)
  "Corresponds to kcdbappend. A wrapper of SET."
  (set db key value :method :append))

(defun increment (db key n &key origin)
  (let ((fn (if (integerp n) #'kc.db.base:increment #'increment/double))
        (origin (if origin origin (if (integerp n) 0 0.0d0))))
    (with-allocated-foreign-string (key-buf key-len (x->foreign-string key))
      (funcall fn db key-buf key-len n :origin origin))))

(defun cas (db key old new)
  (with-allocated-foreign-strings ((key-buf key-len (x->foreign-string key))
                                   (old-buf old-len (x->foreign-string old))
                                   (new-buf new-len (x->foreign-string new)))
    (kc.db.base:cas db key-buf key-len old-buf old-len new-buf new-len)))

(defun remove (db key)
  (with-allocated-foreign-string (key-buf key-len (x->foreign-string key))
    (kc.db.base:remove db key-buf key-len)))

(defmacro with-transaction ((db &rest args) &body body)
  (with-gensyms (commit)
    (once-only (db)
      `(let ((,commit nil))
         (begin-transaction ,db ,@args)
         (unwind-protect (prog1 (progn ,@body) (setf ,commit t))
           (end-transaction ,db :commit ,commit))))))
