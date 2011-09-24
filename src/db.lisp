(in-package :kc.db)

(deftype octet ()
  "An octet. An alias of (unsigned-byte 8)."
  '(unsigned-byte 8))

(defun new ()
  "Creates a polymorphic database object and returns it. The object must be
released with DELETE when it's no longer in use."
  (kcdbnew))

(defun delete (db)
  "Destroys the database object DB."
  (kcdbdel db))

(defun open (db path
             &key
             (direction :input)      ; :input, :output, :io, :probe
             (if-exists :append)     ; :overwrite, :append
             (if-does-not-exist (cond ((or (eq direction :probe)
                                           (eq direction :input)
                                           (eq if-exists :overwrite)
                                           (eq if-exists :append))
                                       :error)
                                      (t :create)))
             (error-p t)
             (auto-transaction-p nil)
             (auto-sync-p nil)
             (lock-p t)
             (block-p t)
             (repair-p t))
  "Opens the database file specified by PATH and associates it with the database
object DB."
  (let* ((mode (logand (case direction
                         (:input +open-mode-reader+)
                         (:output +open-mode-writer+)
                         (:io #.(logand +open-mode-reader+ +open-mode-writer+))
                         (t 0))
                       (if (eq if-exists :overwrite) +open-mode-truncate+ 0)
                       (if (eq if-does-not-exist :create) +open-mode-create+ 0)
                       (if auto-transaction-p +open-mode-auto-tran+ 0)
                       (if auto-sync-p +open-mode-auto-sync+ 0)
                       (if lock-p 0 +open-mode-no-lock+)
                       (if block-p 0 +open-mode-try-lock+)
                       (if repair-p 0 +open-mode-no-repair+))))
    (with-foreign-string (p path)
      (let ((r (kcdbopen db p mode)))
        (if (and (zerop r) error-p)
            (error "~a" (kcdbemsg db))
            (translate-from-foreign r :boolean))))))

(defun close (db)
  "Closes the database file associated with the database object DB. Returns T if
succeed, or NIL otherwise."
  (translate-from-foreign (kcdbclose db) :boolean))

(defmacro with-db ((db filespec &rest args) &body body)
  (once-only (filespec)
    `(let ((,db (new)))
       (unwind-protect (when (open ,db ,filespec ,@args)
                         (unwind-protect (progn ,@body)
                           (close ,db)))
         (delete ,db)))))

(defun x->foreign-string (x)
  (etypecase x
    (string (foreign-string-alloc x))))

(declaim (inline foreign-string->string))
(defun foreign-string->string (fs)
  (foreign-string-to-lisp fs))

(defun foreign-string->octets (fs len)
  (do* ((octets (make-array len :element-type 'octet))
        (n 0 (1+ n))
        (octet (mem-aref fs :uchar n) (mem-aref fs :uchar n)))
       ((zerop octet) octets)
    (setf (aref octets n) octet)))

(defun get/fs (db key-buf key-len &key (string-p t))
  "Retrieves the value of a record with a foreign string key."
  (with-foreign-object (value-len 'size_t)
    (let ((value-ptr (kcdbget db key-buf key-len value-len)))
      (if (null-pointer-p value-ptr)
          nil
          (unwind-protect
               (if string-p
                   (foreign-string->string value-ptr)
                   (foreign-string->octets value-ptr
                                           (mem-ref value-len 'size_t)))
            (kcfree value-ptr))))))

(defun get (db key &rest rest)
  "Retrieves the value of a record."
  (multiple-value-bind (key-buf key-len) (x->foreign-string key)
    (unwind-protect (apply #'get/fs db key-buf key-len rest)
      (foreign-string-free key-buf))))
