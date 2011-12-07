(in-package :kc.conv)

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
