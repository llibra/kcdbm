(defpackage :kyoto-cabinet.asdf (:use :cl :asdf))
(in-package :kyoto-cabinet.asdf)

(defsystem :kcdbm
  :version "0.0"
  :author "Manabu Takayama <learn.libra@gmail.com>"
  :license "MIT License"
  :depends-on (:alexandria :cffi)
  :serial t
  :components ((:file "packages")
               (:file "ffi")
               (:file "db")))
