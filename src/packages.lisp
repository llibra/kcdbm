(in-package :cl-user)

(defpackage :kyoto-cabinet.ffi.types
  (:nicknames :kc.ffi.type)
  (:use :cl :cffi)
  (:export :size_t :kcvisitfull :kcvisitempty :kcfileproc :kcdb :kccur :kcstr
           :kcrec))

(defpackage :kyoto-cabinet.ffi.variables
  (:nicknames :kc.ffi.var)
  (:use :cl :cffi)
  ;; Constants
  (:export :+int64-min+)
  ;; Error Codes
  (:export :+kcesuccess+ :+kcenoimpl+ :+kceinvalid+ :+kcenorepos+ :+kcenoperm+
           :+kcebroken+ :+kceduprec+ :+kcenorec+ :+kcelogic+ :+kcesystem+
           :+kcemisc+)
  ;; Open Modes
  (:export :+kcoreader+ :+kcowriter+ :+kcocreate+ :+kcotruncate+ :+kcoautotran+
           :+kcoautosync+ :+kconolock+ :+kcotrylock+ :+kconorepair+)
  ;; Merge Modes
  (:export :+kcmset+ :+kcmadd+ :+kcmreplace+ :+kcmappend+)
  ;; Variables
  (:export :+kcversion+ :+kcvisnop+ :+kcvisremove+))

(defpackage :kyoto-cabinet.ffi.common
  (:nicknames :kc.ffi.common)
  (:use :cl :cffi)
  (:export :kcfree :kcecodename))

(defpackage :kyoto-cabinet.ffi.database
  (:nicknames :kc.ffi.db)
  (:use :cl :cffi :kc.ffi.type)
  (:export :kcdbnew :kcdbdel :kcdbopen :kcdbclose :kcdbecode :kcdbemsg
           :kcdbaccept :kcdbacceptbulk :kcdbiterate :kcdbscanpara :kcdbset
           :kcdbadd :kcdbreplace :kcdbappend :kcdbincrint :kcdbincrdouble
           :kcdbcas :kcdbremove :kcdbget :kcdbgetbuf :kcdbseize :kcdbsetbulk
           :kcdbremovebulk :kcdbgetbulk :kcdbsync :kcdboccupy :kcdbcopy
           :kcdbbegintran :kcdbbegintrantry :kcdbendtran :kcdbclear
           :kcdbdumpsnap :kcdbloadsnap :kcdbcount :kcdbsize :kcdbpath
           :kcdbstatus :kcdbmatchprefix :kcdbmatchregex :kcdbmerge))

(defpackage :kyoto-cabinet.ffi.cursor
  (:nicknames :kc.ffi.cur)
  (:use :cl :cffi :kc.ffi.type)
  (:export :kcdbcursor :kccurdel :kccuraccept :kccursetvalue :kccurremove
           :kccurgetkey :kccurgetvalue :kccurget :kccurseize :kccurjump
           :kccurjumpkey :kccurjumpback :kccurjumpbackkey :kccurstep
           :kccurstepback :kccurdb :kccurecode :kccuremsg))

(defpackage :kyoto-cabinet.ffi.index-database
  (:nicknames :kc.ffi.idx)
  (:use :cl :cffi :kc.ffi.type)
  )

(defpackage :kyoto-cabinet.ffi
  (:nicknames :kc.ffi)
  (:use :kc.ffi.type :kc.ffi.var :kc.ffi.common :kc.ffi.db :kc.ffi.cur
        :kc.ffi.idx)
  ;; Types
  (:export :size_t :kcvisitfull :kcvisitempty :kcfileproc :kcdb :kccur :kcstr
           :kcrec)
  ;; Constants
  (:export :+int64-min+)
  ;; Error Codes
  (:export :+kcesuccess+ :+kcenoimpl+ :+kceinvalid+ :+kcenorepos+ :+kcenoperm+
           :+kcebroken+ :+kceduprec+ :+kcenorec+ :+kcelogic+ :+kcesystem+
           :+kcemisc+)
  ;; Open Modes
  (:export :+kcoreader+ :+kcowriter+ :+kcocreate+ :+kcotruncate+ :+kcoautotran+
           :+kcoautosync+ :+kconolock+ :+kcotrylock+ :+kconorepair+)
  ;; Merge Modes
  (:export :+kcmset+ :+kcmadd+ :+kcmreplace+ :+kcmappend+)
  ;; Variables
  (:export :+kcversion+ :+kcvisnop+ :+kcvisremove+)
  ;; Common Functions
  (:export :kcfree :kcecodename)
  ;; Database Functions
  (:export :kcdbnew :kcdbdel :kcdbopen :kcdbclose :kcdbecode :kcdbemsg
           :kcdbaccept :kcdbacceptbulk :kcdbiterate :kcdbscanpara :kcdbset
           :kcdbadd :kcdbreplace :kcdbappend :kcdbincrint :kcdbincrdouble
           :kcdbcas :kcdbremove :kcdbget :kcdbgetbuf :kcdbseize :kcdbsetbulk
           :kcdbremovebulk :kcdbgetbulk :kcdbsync :kcdboccupy :kcdbcopy
           :kcdbbegintran :kcdbbegintrantry :kcdbendtran :kcdbclear
           :kcdbdumpsnap :kcdbloadsnap :kcdbcount :kcdbsize :kcdbpath
           :kcdbstatus :kcdbmatchprefix :kcdbmatchregex :kcdbmerge)
  ;; Cursor Functions
  (:export :kcdbcursor :kccurdel :kccuraccept :kccursetvalue :kccurremove
           :kccurgetkey :kccurgetvalue :kccurget :kccurseize :kccurjump
           :kccurjumpkey :kccurjumpback :kccurjumpbackkey :kccurstep
           :kccurstepback :kccurdb :kccurecode :kccuremsg))

(defpackage :kyoto-cabinet.extension
  (:nicknames :kc.ext)
  (:use :cl)
  (:export :x->foreign-string :foreign-string->x))

(defpackage :kyoto-cabinet.types
  (:nicknames :kc.type)
  (:use :cl :cffi)
  ;; Lisp Types
  (:export :octet :octets :simple-octets)
  ;; CFFI Types
  (:export :error-code :open-mode :merge-mode))

(defpackage :kyoto-cabinet.variables
  (:nicknames :kc.var)
  (:use :cl :cffi)
  (:export :*null-pointer*))

(defpackage :kyoto-cabinet.conversion
  (:nicknames :kc.conv)
  (:use :cl :cffi :kc.type)
  (:export :x->foreign-string :foreign-string->x))

(defpackage :kyoto-cabinet.utilities
  (:nicknames :kc.util)
  (:use :cl :cffi :kc.ffi)
  (:export :with-allocated-foreign-string :with-allocated-foreign-strings
           :with-kcmalloced-pointer :aif/ptr :it :set-method->ffi-symbol))

(defpackage :kyoto-cabinet.error
  (:nicknames :kc.err)
  (:use :cl)
  (:shadow :error)
  (:export :error :path :code :message))

(defpackage :kyoto-cabinet.database.base
  (:nicknames :kc.db.base)
  (:use :cl :cffi :kc.ffi :kc.type :kc.var :kc.conv :kc.util)
  (:shadow :error :delete :open :close :set :remove :get :set :count :merge)
  (:import-from :alexandria :once-only)
  (:import-from :ppcre :split)
  (:export :error :path :new :delete :open :close :error-code :accept :iterate
           :scan-in-parallel :set :increment :increment/double :cas :remove :get
           :get/buffer :synchronize :occupy :copy :begin-transaction
           :end-transaction :clear :dump-snapshot :load-snapshot :count :size
           :status :merge))

(defpackage :kyoto-cabinet.database.low-level
  (:nicknames :kc.db.low)
  (:documentation "Contains low-level database APIs. The APIs in this package
directly accept a foreign string of CFFI.")
  (:use :kc.db.base)
  (:export :accept :iterate :scan-in-parallel :get :get/buffer :set :increment
           :increment/double :cas :remove :synchronize :occupy))

(defpackage :kyoto-cabinet.database
  (:nicknames :kc.db)
  (:documentation "Contains high-level database APIs. The APIs in this package
convert various data automatically.")
  (:use :cl :cffi :kc.ffi :kc.type :kc.var :kc.conv :kc.util :kc.db.base)
  (:shadow :set :increment :cas :replace :append :remove :get)
  (:import-from :alexandria :with-gensyms :once-only)
  (:shadowing-import-from :kc.db.base :error :delete :open :close :count :merge)
  (:export :error :path :new :delete :open :close :with-db :set :add :replace
           :append :increment :cas :remove :get :seize :copy :begin-transaction
           :end-transaction :with-transaction :clear :dump-snapshot
           :load-snapshot :count :size :status :merge))
