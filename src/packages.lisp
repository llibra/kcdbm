(in-package :cl-user)

(defpackage :kyoto-cabinet.ffi.core
  (:nicknames :kc.ffi.core)
  (:use :cl :cffi)
  (:export :size_t

           :+open-mode-reader+ :+open-mode-writer+ :+open-mode-create+
           :+open-mode-truncate+ :+open-mode-auto-tran+ :+open-mode-auto-sync+
           :+open-mode-no-lock+ :+open-mode-try-lock+ :+open-mode-no-repair+

           :kcfree :kcecodename :kcdbnew :kcdbdel :kcdbopen :kcdbclose
           :kcdbecode :kcdbemsg :kcdbaccept :kcdbacceptbulk :kcdbiterate
           :kcdbscanpara :kcdbset :kcdbadd :kcdbreplace :kcdbappend :kcdbincrint
           :kcdbincrdouble :kcdbcas :kcdbremove :kcdbget :kcdbgetbuf :kcdbseize
           :kcdbsetbulk :kcdbremovebulk :kcdbgetbulk :kcdbsync :kcdboccupy
           :kcdbcopy :kcdbbegintran :kcdbbegintrantry :kcdbendtran :kcdbclear
           :kcdbdumpsnap :kcdbloadsnap :kcdbcount :kcdbsize :kcdbpath
           :kcdbstatus :kcdbmatchprefix :kcdbmatchregex :kcdbmerge))

(defpackage :kyoto-cabinet.ffi.cursor
  (:nicknames :kc.ffi.cur)
  (:use :cl :cffi :kc.ffi.core)
  )

(defpackage :kyoto-cabinet.ffi.index-database
  (:nicknames :kc.ffi.idx)
  (:use :cl :cffi :kc.ffi.core)
  )

(defpackage :kyoto-cabinet.common
  (:nicknames :kc.common)
  (:documentation "Contains common parts of the whole of KCDBM except the FFI
layer. Types, utilities, etc..")
  (:use :cl :cffi :kc.ffi.core)
  (:export :open-mode

           :octet :octets :simple-octets

           :with-allocated-foreign-string :with-allocated-foreign-strings

           :string->foreign-string :octets->foreign-string :x->foreign-string
           :foreign-string->string :foreign-string->octets

           :set-method->ffi-symbol))

(defpackage :kyoto-cabinet.database.foreign-string
  (:nicknames :kc.db.fs)
  (:documentation "Contains low-level database APIs. The APIs in this package
directly accept a foreign string of CFFI. They exist for speed.")
  (:use :cl :cffi :kc.ffi.core :kc.common)
  (:shadow :get :set)
  (:import-from :alexandria :once-only)
  (:export :accept :get :set))

(defpackage :kyoto-cabinet.database
  (:nicknames :kc.db)
  (:documentation "Contains high-level database APIs. The APIs in this package
convert various data automatically.")
  (:use :cl :cffi :kc.ffi.core :kc.common)
  (:shadow :delete :open :close :get :set :replace :append)
  (:import-from :alexandria :with-gensyms :once-only)
  (:export :new :delete :open :close :accept :with-db :get :set :add :replace
           :append :begin-transaction :end-transaction :with-transaction))

(defpackage :kyoto-cabinet.extension
  (:nicknames :kc.ext)
  (:use :cl)
  (:export :x->foreign-string))
