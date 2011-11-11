(in-package :kc.db)

(deftype octet ()
  "An octet. An alias of (unsigned-byte 8)."
  '(unsigned-byte 8))

(deftype octets (&optional (size '*))
  "A vector of octet."
  `(vector octet ,size))

(deftype simple-octets (&optional (size '*))
  "A simple-array of octet."
  `(simple-array octet (,size)))

(defmacro with-allocated-foreign-string ((var len-var form) &body body)
  `(multiple-value-bind (,var ,len-var) ,form
     (unwind-protect (progn ,@body) (foreign-free ,var))))

(defmacro with-allocated-foreign-strings (bindings &body body)
  (destructuring-bind (binding . rest) bindings
    (if rest
        `(with-allocated-foreign-string ,binding
           (with-allocated-foreign-strings ,rest ,@body))
        `(with-allocated-foreign-string ,binding ,@body))))

(defun new ()
  "Creates a polymorphic database object and returns it. The object must be
released with DELETE when it's no longer in use."
  (kcdbnew))

(defun delete (db)
  "Destroys the database object DB."
  (kcdbdel db))

(defbitfield open-mode
  (:reader 1)
  :writer
  :create
  :truncate
  :auto-tran
  :auto-sync
  :no-lock
  :try-lock
  :no-repair)

(defun open (db path &rest mode)
  "Opens the database file specified by PATH and associates it with the database
object DB."
  (with-foreign-string (p path)
    (if (zerop (kcdbopen db p (foreign-bitfield-value 'open-mode mode)))
        (error "Can't open the database file ~a. (~a)" path (kcdbemsg db))
        t)))

(defun close (db)
  "Closes the database file associated with the database object DB. Returns T if
succeed, or NIL otherwise."
  (if (zerop (kcdbclose db))
      (error "Can't close the database file ~a. (~a)"
             (kcdbpath db) (kcdbemsg db))
      t))

(defmacro with-db ((db filespec &rest args) &body body)
  `(let ((,db (new)))
     (unwind-protect (progn
                       (open ,db ,filespec ,@args)
                       (unwind-protect (progn ,@body)
                         (close ,db)))
       (delete ,db))))

(declaim (inline string->foreign-string))
(defun string->foreign-string (string)
  (foreign-string-alloc string))

(defun octets->foreign-string (octets)
  (let* ((octets-len (length octets))
         (fs-len (1+ octets-len))
         (fs (foreign-alloc :uint8 :count fs-len)))
    (do* ((n 0 (1+ n)))
         ((= n octets-len))
      (setf (mem-aref fs :uint8 n) (aref octets n)))
    (setf (mem-aref fs :uint8 octets-len) 0)))

(defun x->foreign-string (x)
  (typecase x
    (string
     (string->foreign-string x))
    ((or octets simple-octets)
     (octets->foreign-string x))
    (t
     (kc.ext:x->foreign-string x))))

(declaim (inline foreign-string->string))
(defun foreign-string->string (fs)
  (foreign-string-to-lisp fs))

(defun foreign-string->octets (fs len)
  (do* ((octets (make-array len :element-type 'octet))
        (n 0 (1+ n))
        (octet (mem-aref fs :uint8 n) (mem-aref fs :uint8 n)))
       ((= n len) octets)
    (setf (aref octets n) octet)))

(defun accept (db key-buf key-len full-fn empty-fn
               &key (opaque (load-time-value (null-pointer))) (writable t))
  (let ((writable (convert-to-foreign writable :boolean)))
    (if (zerop (kcdbaccept db key-buf key-len full-fn empty-fn opaque writable))
        (error "Can't accept the visitor functions. (~a)" (kcdbemsg db))
        t)))

(defun get/fs (db key-buf key-len &key (string-p t))
  "Finds the record whose key is KEY-BUF in the database associated with DB and
returns the associated value. If there's no corresponding record, returns NIL.
KEY-BUF is a CFFI's foreign string and KEY-LEN is the length of KEY-BUF.

If STRING-P is true, returns the value as a Lisp string. Returns as a vector
otherwise."
  (with-foreign-object (value-len 'size_t)
    (let ((value-ptr (kcdbget db key-buf key-len value-len)))
      (if (null-pointer-p value-ptr)
          (error "Can't get the value associated with the key. (~a)"
                 (kcdbemsg db))
          (unwind-protect
               (if string-p
                   (foreign-string->string value-ptr)
                   (foreign-string->octets value-ptr
                                           (mem-ref value-len 'size_t)))
            (kcfree value-ptr))))))

(defun get (db key &rest rest)
  "Finds the record whose key is KEY in the database associated with DB and
returns the associated value. If there's no corresponding record, returns NIL.
This function supports the same keyword arguments as GET/FS.

It accepts a string and an octet vector as KEY. If you would like to support
more types, it's a convenient way to define a specialized method of
KC.EXT:X->FOREIGN-STRING."
  (with-allocated-foreign-string (key-buf key-len (x->foreign-string key))
    (apply #'get/fs db key-buf (1- key-len) rest)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun set-method->ffi-symbol (method)
    (find-symbol (format nil "KCDB~a" method) :kc.ffi.core)))

(define-compiler-macro set/fs
    (&whole form db key-buf key-len value-buf value-len &key (method :set)
     &environment env)
  (once-only (db)
    (if (constantp method env)
        `(if (zerop (,(set-method->ffi-symbol method)
                      ,db ,key-buf ,key-len ,value-buf ,value-len))
             (error "Can't set the value associated with the key. (~a)" (kcdbemsg ,db))
             t)
        form)))

(defun set/fs (db key-buf key-len value-buf value-len &key (method :set))
  "Sets the value of the record whose key is KEY-BUF in the database associated
with DB to VALUE-BUF. KEY-BUF and VALUE-BUF are CFFI's foreign strings and
KEY-LEN and VALUE-LEN are the lengths of KEY-BUF and VALUE-BUF. 

METHOD is a method to update the value of a record. It should be one of :SET,
:ADD :REPLACE or :APPEND. Each method corresponds to kcdbset, kcdbadd,
kcdbreplace, or kcdbappend.

If succeeds to set a value, T is returned. Otherwise, NIL is returned."
  (if (zerop (funcall (set-method->ffi-symbol method)
                      db key-buf key-len value-buf value-len))
      (error "Can't set the value associated with the key. (~a)" (kcdbemsg db))
      t))

;;; For compiler macro expansion of set/fs.
(define-compiler-macro set (db key value &rest rest)
  `(with-allocated-foreign-strings
       ((key-buf key-len (x->foreign-string ,key))
        (value-buf value-len (x->foreign-string ,value)))
     (set/fs ,db key-buf (1- key-len) value-buf (1- value-len) ,@rest)))

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
    (apply #'set/fs db key-buf (1- key-len) value-buf (1- value-len) rest)))

(defun add (db key value)
  "Corresponds to kcdbadd. A wrapper of SET."
  (set db key value :method :add))

(defun replace (db key value)
  "Corresponds to kcdbreplace. A wrapper of SET."
  (set db key value :method :replace))

(defun append (db key value)
  "Corresponds to kcdbappend. A wrapper of SET."
  (set db key value :method :append))

(defun begin-transaction (db &key (blocking-p t) physical-p)
  (if (zerop (funcall (if blocking-p #'kcdbbegintran #'kcdbbegintrantry)
                      db (convert-to-foreign physical-p :boolean)))
      (error "Can't begin a transaction. (~a)" (kcdbemsg db))
      t))

(defun end-transaction (db &key (commit-p t))
  (if (zerop (kcdbendtran db (convert-to-foreign commit-p :boolean)))
      (error "Can't end the current transaction. (~a)" (kcdbemsg db))
      t))

(defmacro with-transaction ((db &rest args) &body body)
  (with-gensyms (commit-p)
    (once-only (db)
      `(let ((,commit-p nil))
         (begin-transaction ,db ,@args)
         (unwind-protect (prog1 (progn ,@body) (setf ,commit-p t))
           (end-transaction ,db :commit-p ,commit-p))))))
