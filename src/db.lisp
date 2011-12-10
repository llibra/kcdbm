(in-package :kc.db.base)

(defun new ()
  "Creates a polymorphic database object and returns it. The object must be
released with DELETE when it's no longer in use."
  (kcdbnew))

(defun delete (db)
  "Destroys the database object DB."
  (kcdbdel db))

(defun error-message (db)
  (foreign-string-to-lisp (kcdbemsg db)))

(defun path (db)
  "Returns the associated path of the database object DB."
  (with-kcmalloced-pointer (path-ptr (kcdbpath db))
    (foreign-string-to-lisp path-ptr)))

(defun open (db path &rest mode)
  "Opens the database file specified by PATH and associates it with the database
object DB."
  (with-foreign-string (p path)
    (if (zerop (kcdbopen db p (foreign-bitfield-value 'open-mode mode)))
        (error "Can't open the database file ~a. (~a)" path (error-message db))
        t)))

(defun close (db)
  "Closes the database file associated with the database object DB. Returns T if
succeed and signals an error otherwise."
  (if (zerop (kcdbclose db))
      (error "Can't close the database file ~a. (~a)"
             (path db) (error-message db))
      t))

(defun error-code (db)
  (foreign-enum-keyword 'error-code (kcdbecode db)))

(defun accept (db key-buf key-len full-fn empty-fn &key (opaque *null-pointer*)
                                                        (writable t))
  (let ((writable (convert-to-foreign writable :boolean)))
    (if (zerop (kcdbaccept db key-buf key-len full-fn empty-fn opaque writable))
        (error "Can't accept the visitor functions. (~a)"
               (error-message db))
        t)))

(defun iterate (db full-fn &key (opaque *null-pointer*) (writable t))
  (let ((writable (convert-to-foreign writable :boolean)))
    (ematch (kcdbiterate db full-fn opaque writable)
      (1 t)
      (0 (error "The iteration for the records in the database failed. (~a)"
                (error-message db))))))

(defun scan-in-parallel (db visitor n &key (opaque *null-pointer*))
  (if (zerop (kcdbscanpara db visitor opaque n))
      (error "The parallel scanning for the database failed. (~a)"
             (error-message db))
      t))

(define-compiler-macro set
    (&whole form db key-buf key-len value-buf value-len &key (method :set)
     &environment env)
  (once-only (db)
    (if (constantp method env)
        `(if (zerop (,(set-method->ffi-symbol method)
                      ,db ,key-buf ,key-len ,value-buf ,value-len))
             (error "Can't set the value associated with the key. (~a)"
                    (error-message ,db))
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
      (error "Can't set the value associated with the key. (~a)"
             (error-message db))
      t))

(defun increment (db key-buf key-len n &key (origin 0))
  (let ((new (kcdbincrint db key-buf key-len n origin)))
    (if (= new +int64-min+)
        (error "Can't increment the value of the record. (~a)"
               (error-message db))
        new)))

(defun increment/double (db key-buf key-len n &key (origin 0.0d0))
  (let ((new (kcdbincrdouble db key-buf key-len n origin)))
    (flet ((err ()
             (error "Can't increment the value of the record. (~a)"
                    (error-message db))))
      ;; check whether the returned value is NaN
      (handler-case (if (= new new) new (err))
        ;; if a floating point trap occurs
        (floating-point-invalid-operation (c)
          (declare (ignore c))
          (err))))))

(defun cas (db key-fs key-len old-fs old-len new-fs new-len)
  (if (zerop (kcdbcas db key-fs key-len old-fs old-len new-fs new-len))
      (error "Can't perform compare-and-swap. (~a)" (error-message db))
      t))

(defun remove (db key-buf key-len)
  (ematch (kcdbremove db key-buf key-len)
    (1 t)
    (0 (error "Can't remove the record. (~a)" (error-message db)))))

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
               (error "Can't get the value associated with the key. (~a)"
                      (error-message db))))))

(defun get/buffer (db key-buf key-len value-buf value-len)
  (let ((len (kcdbgetbuf db key-buf key-len value-buf value-len)))
    (if (= len -1)
        (error "Can't get the value associated with the key. (~a)"
               (error-message db))
        len)))

(defun synchronize (db &key (post-proc *null-pointer*)
                            (physical-p nil)
                            (opaque *null-pointer*))
  (let ((physical-p (convert-to-foreign physical-p :boolean)))
    (if (zerop (kcdbsync db physical-p post-proc opaque))
        (error "The synchronization of the database failed. (~a)"
               (error-message db))
        t)))

(defun occupy (db fn &key (opaque *null-pointer*) (writable t))
  (let ((writable (convert-to-foreign writable :boolean)))
    (if (zerop (kcdboccupy db writable fn opaque))
        (error "The atomic and exclusive operation to the database failed. (~a)"
               (error-message db))
        t)))

(defun copy (db dest)
  (with-foreign-string (dest-fs dest)
    (if (zerop (kcdbcopy db dest-fs))
        (error "Can't copy the database. (~a)" (error-message db))
        t)))

(defun begin-transaction (db &key (blocking-p t) physical-p)
  (if (zerop (funcall (if blocking-p #'kcdbbegintran #'kcdbbegintrantry)
                      db (convert-to-foreign physical-p :boolean)))
      (error "Can't begin a transaction. (~a)" (error-message db))
      t))

(defun end-transaction (db &key (commit t))
  (if (zerop (kcdbendtran db (convert-to-foreign commit :boolean)))
      (error "Can't end the current transaction. (~a)" (error-message db))
      t))

(defun clear (db)
  (ematch (kcdbclear db)
    (1 t)
    (0 (error "Can't clear the database. (~a)" (error-message db)))))

(defun dump-snapshot (db dest)
  (with-foreign-string (dest-fs dest)
    (if (zerop (kcdbdumpsnap db dest-fs))
        (error "Can't dump the records of the database into the file. (~a)"
               (error-message db))
        t)))

(defun load-snapshot (db src)
  (with-foreign-string (src-fs src)
    (if (zerop (kcdbloadsnap db src-fs))
        (error "Can't load records from snapshot. (~a)" (error-message db))
        t)))

(defun count (db)
  (match (kcdbcount db)
    (-1 (error "Can't count the records in the database. (~a)"
               (error-message db)))
    (n n)))

(defun size (db)
  (match (kcdbsize db)
    (-1 (error "Can't calculate the size of the database. (~a)"
               (error-message db)))
    (size size)))

(defun status (db)
  (aif/ptr (kcdbstatus db)
           (with-kcmalloced-pointer (ptr it)
             (mapcar (lambda (line) (apply #'cons (split "\\t" line)))
                     (split "\\n" (foreign-string-to-lisp ptr))))
           (error "Can't retrieve the miscellaneous status information. (~a)"
                  (error-message db))))

(defun merge (db src mode)
  (let ((src-len (length src))
        (mode (foreign-enum-value 'merge-mode mode)))
    (with-foreign-object (src-ary :pointer src-len)
      (dotimes (n src-len)
        (setf (mem-aref src-ary :pointer n) (elt src n)))
      (ematch (kcdbmerge db src-ary src-len mode)
        (1 t)
        (0 (error "Can't merge records from the databases. (~a)"
                  (error-message db)))))))

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
(define-compiler-macro set (db key value &rest rest)
  `(with-allocated-foreign-strings
       ((key-buf key-len (x->foreign-string ,key))
        (value-buf value-len (x->foreign-string ,value)))
     (kc.db.base:set ,db key-buf key-len value-buf value-len ,@rest)))

(defun set (db key value &rest rest)
  "Sets the value of the record whose key equals KEY in the database associated
with DB to VALUE. If succeeds to set the value, T is returned. Otherwise, an
error is signaled.

METHOD is a method to update the value of a record. It should be one of :SET,
:ADD, :REPLACE or :APPEND. Each method corresponds to kcdbset, kcdbadd,
kcdbreplace, or kcdbappend."
  (with-allocated-foreign-strings
      ((key-buf key-len (x->foreign-string key))
       (value-buf value-len (x->foreign-string value)))
    (apply #'kc.db.base:set db key-buf key-len value-buf value-len rest)))

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
  (with-allocated-foreign-strings ((key-fs key-len (x->foreign-string key))
                                   (old-fs old-len (x->foreign-string old))
                                   (new-fs new-len (x->foreign-string new)))
    (kc.db.base:cas db key-fs key-len old-fs old-len new-fs new-len)))

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
