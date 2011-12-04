(in-package :kc.db.low)

(defun error-message (db)
  (foreign-string-to-lisp (kcdbemsg db)))

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

(defun scan-parallel (db visitor n &key (opaque *null-pointer*))
  (if (zerop (kcdbscanpara db visitor opaque n))
      (error "The parallel scanning for the database failed. (~a)"
             (error-message db))
      t))

(defun get (db key-buf key-len &key (as :string) remove-p)
  "Finds the record whose key is KEY-BUF in the database associated with DB and
returns the associated value. If there's no corresponding record, returns NIL.
KEY-BUF is a CFFI's foreign string and KEY-LEN is the length of KEY-BUF.

If STRING-P is true, returns the value as a Lisp string. Returns as a vector
otherwise."
  (let ((fn (if remove-p #'kcdbseize #'kcdbget)))
    (with-foreign-object (value-len 'size_t)
      (aif/ptr (funcall fn db key-buf key-len value-len)
               (with-kcmalloced-pointer (value-ptr it)
                 (foreign-string->x as value-ptr (mem-ref value-len 'size_t)))
               (error "Can't get the value associated with the key. (~a)"
                      (error-message db))))))

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
  "Sets the value of the record whose key is KEY-BUF in the database associated
with DB to VALUE-BUF. KEY-BUF and VALUE-BUF are CFFI's foreign strings and
KEY-LEN and VALUE-LEN are the lengths of KEY-BUF and VALUE-BUF. 

METHOD is a method to update the value of a record. It should be one of :SET,
:ADD :REPLACE or :APPEND. Each method corresponds to kcdbset, kcdbadd,
kcdbreplace, or kcdbappend.

If succeeds to set a value, T is returned. Otherwise, NIL is returned."
  (if (zerop (funcall (set-method->ffi-symbol method)
                      db key-buf key-len value-buf value-len))
      (error "Can't set the value associated with the key. (~a)"
             (error-message db))
      t))

(defun remove (db key-buf key-len)
  (ematch (kcdbremove db key-buf key-len)
    (1 t)
    (0 (error "Can't remove the record. (~a)" (error-message db)))))

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

(in-package :kc.db)

(defun new ()
  "Creates a polymorphic database object and returns it. The object must be
released with DELETE when it's no longer in use."
  (kcdbnew))

(defun delete (db)
  "Destroys the database object DB."
  (kcdbdel db))

(defun path (db)
  "Returns the path of the database object DB."
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
succeed, or NIL otherwise."
  (if (zerop (kcdbclose db))
      (error "Can't close the database file ~a. (~a)"
             (path db) (error-message db))
      t))

(defmacro with-db ((db filespec &rest args) &body body)
  `(let ((,db (new)))
     (unwind-protect (progn
                       (open ,db ,filespec ,@args)
                       (unwind-protect (progn ,@body)
                         (close ,db)))
       (delete ,db))))

(defun get (db key &rest rest)
  "Finds the record whose key is KEY in the database associated with DB and
returns the associated value. If there's no corresponding record, returns NIL.
This function supports the same keyword arguments as GET/FS.

It accepts a string and an octet vector as KEY. If you would like to support
more types, it's a convenient way to define a specialized method of
KC.EXT:X->FOREIGN-STRING."
  (with-allocated-foreign-string (key-buf key-len (x->foreign-string key))
    (apply #'kc.db.low:get db key-buf key-len rest)))

(defun seize (db key &key (as :string))
  (get db key :as as :remove-p t))

;;; For compiler macro expansion of KC.DB.FS:SET.
(define-compiler-macro set (db key value &rest rest)
  `(with-allocated-foreign-strings
       ((key-buf key-len (x->foreign-string ,key))
        (value-buf value-len (x->foreign-string ,value)))
     (kc.db.low:set ,db key-buf key-len value-buf value-len ,@rest)))

(defun set (db key value &rest rest)
  "Sets the value of the record whose key is KEY in the database associated with
DB KEY to VALUE. This function supports the same keyword arguments as SET/FS.

It accepts a string and an octet vector as KEY and VALUE. Like GET, if you would
like to support more types, it's a convenient way to define a specialized method
of KC.EXT:X->FOREIGN-STRING.

If succeeds to set a value, T is returned. Otherwise, NIL is returned."
  (with-allocated-foreign-strings
      ((key-buf key-len (x->foreign-string key))
       (value-buf value-len (x->foreign-string value)))
    (apply #'kc.db.low:set db key-buf key-len value-buf value-len rest)))

(defun add (db key value)
  "Corresponds to kcdbadd. A wrapper of SET."
  (set db key value :method :add))

(defun replace (db key value)
  "Corresponds to kcdbreplace. A wrapper of SET."
  (set db key value :method :replace))

(defun append (db key value)
  "Corresponds to kcdbappend. A wrapper of SET."
  (set db key value :method :append))

(defun remove (db key)
  (with-foreign-string ((key-buf key-len) key :null-terminated-p nil)
    (kc.db.low:remove db key-buf key-len)))

(defun begin-transaction (db &key (blocking-p t) physical-p)
  (if (zerop (funcall (if blocking-p #'kcdbbegintran #'kcdbbegintrantry)
                      db (convert-to-foreign physical-p :boolean)))
      (error "Can't begin a transaction. (~a)" (error-message db))
      t))

(defun end-transaction (db &key (commit-p t))
  (if (zerop (kcdbendtran db (convert-to-foreign commit-p :boolean)))
      (error "Can't end the current transaction. (~a)" (error-message db))
      t))

(defmacro with-transaction ((db &rest args) &body body)
  (with-gensyms (commit-p)
    (once-only (db)
      `(let ((,commit-p nil))
         (begin-transaction ,db ,@args)
         (unwind-protect (prog1 (progn ,@body) (setf ,commit-p t))
           (end-transaction ,db :commit-p ,commit-p))))))

(defun clear (db)
  (ematch (kcdbclear db)
    (1 t)
    (0 (error "Can't clear the database. (~a)" (error-message db)))))

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

(defun copy (db dest)
  (with-foreign-string (dest-fs dest)
    (if (zerop (kcdbcopy db dest-fs))
        (error "Can't copy the database. (~a)" (error-message db))
        t)))

(defun status (db)
  (aif/ptr (kcdbstatus db)
           (with-kcmalloced-pointer (ptr it)
             (mapcar (lambda (line) (apply #'cons (split "\\t" line)))
                     (split "\\n" (foreign-string-to-lisp ptr))))
           (error "Can't retrieve the miscellaneous status information. (~a)"
                  (error-message db))))
