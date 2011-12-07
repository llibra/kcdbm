(in-package :cl-user)

(defpackage :kyoto-cabinet.ffi.common
  (:nicknames :kc.ffi.common)
  (:use :cl :cffi)
  (:export :size_t :kcvisitfull :kcvisitempty :kcfileproc :kcdb :kcstr :kcrec

           :+int64-min+

           :+kcesuccess+ :+kcenoimpl+ :+kceinvalid+ :+kcenorepos+ :+kcenoperm+
           :+kcebroken+ :+kceduprec+ :+kcenorec+ :+kcelogic+ :+kcesystem+
           :+kcemisc+

           :+kcoreader+ :+kcowriter+ :+kcocreate+ :+kcotruncate+ :+kcoautotran+
           :+kcoautosync+ :+kconolock+ :+kcotrylock+ :+kconorepair+

           :+kcmset+ :+kcmadd+ :+kcmreplace+ :+kcmappend+

           :+kcvisnop+ :+kcvisremove+

           :+kcversion+

           :kcfree :kcecodename))

(defpackage :kyoto-cabinet.ffi.database
  (:nicknames :kc.ffi.db)
  (:use :cl :cffi :kc.ffi.common)
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
  (:use :cl :cffi :kc.ffi.common)
  )

(defpackage :kyoto-cabinet.ffi.index-database
  (:nicknames :kc.ffi.idx)
  (:use :cl :cffi :kc.ffi.common)
  )

(defpackage :kyoto-cabinet.ffi
  (:nicknames :kc.ffi)
  (:use :cl :cffi :kc.ffi.common :kc.ffi.db :kc.ffi.cur :kc.ffi.idx)
  (:export :size_t :kcvisitfull :kcvisitempty :kcfileproc :kcdb :kcstr :kcrec

           :+int64-min+

           :+kcesuccess+ :+kcenoimpl+ :+kceinvalid+ :+kcenorepos+ :+kcenoperm+
           :+kcebroken+ :+kceduprec+ :+kcenorec+ :+kcelogic+ :+kcesystem+
           :+kcemisc+

           :+kcoreader+ :+kcowriter+ :+kcocreate+ :+kcotruncate+ :+kcoautotran+
           :+kcoautosync+ :+kconolock+ :+kcotrylock+ :+kconorepair+

           :+kcmset+ :+kcmadd+ :+kcmreplace+ :+kcmappend+

           :+kcvisnop+ :+kcvisremove+

           :+kcversion+

           :kcfree :kcecodename

           :kcdbnew :kcdbdel :kcdbopen :kcdbclose :kcdbecode :kcdbemsg
           :kcdbaccept :kcdbacceptbulk :kcdbiterate :kcdbscanpara :kcdbset
           :kcdbadd :kcdbreplace :kcdbappend :kcdbincrint :kcdbincrdouble
           :kcdbcas :kcdbremove :kcdbget :kcdbgetbuf :kcdbseize :kcdbsetbulk
           :kcdbremovebulk :kcdbgetbulk :kcdbsync :kcdboccupy :kcdbcopy
           :kcdbbegintran :kcdbbegintrantry :kcdbendtran :kcdbclear
           :kcdbdumpsnap :kcdbloadsnap :kcdbcount :kcdbsize :kcdbpath
           :kcdbstatus :kcdbmatchprefix :kcdbmatchregex :kcdbmerge))

(defpackage :kyoto-cabinet.extension
  (:nicknames :kc.ext)
  (:use :cl)
  (:export :x->foreign-string :foreign-string->x))

(defpackage :kyoto-cabinet.common
  (:nicknames :kc.common)
  (:documentation "Contains common parts of the whole of KCDBM except the FFI
layer. Types, utilities, etc..")
  (:use :cl :cffi :kc.ffi)
  (:export :error-code :open-mode :merge-mode

           :octet :octets :simple-octets

           :with-allocated-foreign-string :with-allocated-foreign-strings
           :with-kcmalloced-pointer

           :string->foreign-string :octets->foreign-string :x->foreign-string
           :foreign-string->string :foreign-string->octets :foreign-string->x

           :set-method->ffi-symbol

           :aif/ptr :it

           :*null-pointer*))

(defpackage :kyoto-cabinet.database.low-level
  (:nicknames :kc.db.low)
  (:documentation "Contains low-level database APIs. The APIs in this package
directly accept a foreign string of CFFI. They exist for speed.")
  (:use :cl :cffi :kc.ffi :kc.common)
  (:shadow :get :remove :set)
  (:import-from :alexandria :once-only)
  (:import-from :cl-adt :ematch)
  (:export :error-message :accept :iterate :scan-in-parallel :get :get/buffer
           :set :increment :increment/double :cas :remove :synchronize :occupy))

(defpackage :kyoto-cabinet.database
  (:nicknames :kc.db)
  (:documentation "Contains high-level database APIs. The APIs in this package
convert various data automatically.")
  (:use :cl :cffi :kc.ffi :kc.common)
  (:shadow :append :close :count :delete :get :merge :open :remove :replace
           :set)
  (:import-from :alexandria :with-gensyms :once-only)
  (:import-from :ppcre :split)
  (:import-from :kc.db.low :error-message)
  (:import-from :cl-adt :match :ematch)
  (:export :new :delete :open :close :error-message :error-code :with-db :get
           :seize :set :add :replace :append :increment :cas :remove :copy
           :begin-transaction :end-transaction :with-transaction :path :clear
           :dump-snapshot :load-snapshot :count :size :status :merge))
