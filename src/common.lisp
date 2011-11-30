(in-package :kc.common)

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

(defcenum merge-mode
  :set
  :add
  :replace
  :append)

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

(defmacro with-kcmalloced-pointer ((var form) &body body)
  `(let ((,var ,form))
     (unwind-protect (progn ,@body)
       (kcfree ,var))))

(declaim (inline string->foreign-string))
(defun string->foreign-string (string)
  (foreign-string-alloc string :null-terminated-p nil))

(defun octets->foreign-string (octets)
  (let* ((len (length octets))
         (fs (foreign-alloc :uint8 :count len)))
    (values (dotimes (n len fs)
              (setf (mem-aref fs :uint8 n) (aref octets n)))
            len)))

(defun x->foreign-string (x)
  (typecase x
    (string
     (string->foreign-string x))
    ((or octets simple-octets)
     (octets->foreign-string x))
    (t
     (kc.ext:x->foreign-string x))))

(declaim (inline foreign-string->string))
(defun foreign-string->string (fs len)
  (foreign-string-to-lisp fs :count len))

(defun foreign-string->octets (fs len)
  (do* ((octets (make-array len :element-type 'octet))
        (n 0 (1+ n))
        (octet (mem-aref fs :uint8 n) (mem-aref fs :uint8 n)))
       ((= n len) octets)
    (setf (aref octets n) octet)))

(defun foreign-string->x (type fs len)
  (case type
    (:string
     (foreign-string->string fs len))
    (:octets
     (foreign-string->octets fs len))
    (t
     (kc.ext:foreign-string->x type fs len))))

(defun set-method->ffi-symbol (method)
  (find-symbol (format nil "KCDB~a" method) :kc.ffi))
