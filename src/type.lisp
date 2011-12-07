(in-package :kc.type)

;;;; Lisp Types

(deftype octet ()
  "An octet. An alias of (unsigned-byte 8)."
  '(unsigned-byte 8))

(deftype octets (&optional (size '*))
  "A vector of octet."
  `(vector octet ,size))

(deftype simple-octets (&optional (size '*))
  "A simple-array of octet."
  `(simple-array octet (,size)))

;;;; CFFI Types

(defcenum error-code
  :success
  :not-implemented
  :invalid-operation
  :no-repository
  :no-permission
  :broken-file
  :record-duplication
  :no-record
  :logical-inconsistency
  :system-error
  (:miscellaneous-error 15))

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
