(in-package :cl-user)
(defpackage :kyoto-cabinet.asdf (:use :cl :asdf))
(in-package :kyoto-cabinet.asdf)

(defsystem :kcdbm
  :version "0.0"
  :author "Manabu Takayama <learn.libra@gmail.com>"
  :license "MIT License"
  :depends-on (:alexandria :cl-ppcre :cffi)
  :components ((:module "src"
                        :serial t
                        :components ((:file "packages")
                                     (:file "ffi")
                                     (:file "ext")
                                     (:file "type")
                                     (:file "var")
                                     (:file "conv")
                                     (:file "util")
                                     (:file "err")
                                     (:file "db")
                                     (:file "cur")))))

(defsystem :kcdbm-test
  :depends-on (:kcdbm :fiveam)
  :components ((:module "t"
                        :serial t
                        :components ((:file "packages")
                                     (:file "suites")
                                     (:file "db")
                                     (:file "cur")))))
