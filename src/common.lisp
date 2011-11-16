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

(defun set-method->ffi-symbol (method)
  (find-symbol (format nil "KCDB~a" method) :kc.ffi))
