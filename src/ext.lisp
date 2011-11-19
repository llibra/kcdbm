(in-package :kc.ext)

(defgeneric x->foreign-string (x)
  (:documentation "A foreign string converter for user definition data
structures."))

(defgeneric foreign-string->x (type fs len)
  (:documentation "A foreign string converter for user definition data
structures."))
