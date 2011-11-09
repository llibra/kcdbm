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

(defpackage :kyoto-cabinet.database
  (:nicknames :kc.db)
  (:use :cl :cffi :kc.ffi.core)
  (:import-from :alexandria :with-gensyms :once-only)
  (:shadow :delete :open :close :get :set :replace :append)
  (:export :octet :new :delete :open :close :with-db :get/fs :get :set/fs :set
           :add :replace :append :begin-transaction :end-transaction
           :with-transaction))

(defpackage :kyoto-cabinet.extension
  (:nicknames :kc.ext)
  (:use :cl)
  (:export :x->foreign-string))
