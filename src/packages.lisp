(in-package :cl-user)

(defpackage :kyoto-cabinet.ffi.common
  (:nicknames :kc.ffi.common)
  (:use :cl :cffi)
  (:export :size_t :kcvisitfull :kcvisitempty :kcfileproc :kcdb :kcstr :kcrec

           :+kcesuccess+ :+kcenoimpl+ :+kceinvalid+ :+kcenorepos+ :+kcenoperm+
           :+kcebroken+ :+kceduprec+ :+kcenorec+ :+kcelogic+ :+kcesystem+
           :+kcemisc+

           :+kcoreader+ :+kcowriter+ :+kcocreate+ :+kcotruncate+ :+kcoautotran+
           :+kcoautosync+ :+kconolock+ :+kcotrylock+ :+kconorepair+

           :+kcmset+ :+kcmadd+ :+kcmreplace+ :+kcmappend+

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

           :+kcesuccess+ :+kcenoimpl+ :+kceinvalid+ :+kcenorepos+ :+kcenoperm+
           :+kcebroken+ :+kceduprec+ :+kcenorec+ :+kcelogic+ :+kcesystem+
           :+kcemisc+

           :+kcoreader+ :+kcowriter+ :+kcocreate+ :+kcotruncate+ :+kcoautotran+
           :+kcoautosync+ :+kconolock+ :+kcotrylock+ :+kconorepair+

           :+kcmset+ :+kcmadd+ :+kcmreplace+ :+kcmappend+

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
  (:export :open-mode

           :octet :octets :simple-octets

           :with-allocated-foreign-string :with-allocated-foreign-strings

           :string->foreign-string :octets->foreign-string :x->foreign-string
           :foreign-string->string :foreign-string->octets :foreign-string->x

           :set-method->ffi-symbol))

(defpackage :kyoto-cabinet.foreign-string.database
  (:nicknames :kc.fs.db)
  (:documentation "Contains low-level database APIs. The APIs in this package
directly accept a foreign string of CFFI. They exist for speed.")
  (:use :cl :cffi :kc.ffi :kc.common)
  (:shadow :get :set)
  (:import-from :alexandria :once-only)
  (:export :error-message :accept :get :set))

(defpackage :kyoto-cabinet.database
  (:nicknames :kc.db)
  (:documentation "Contains high-level database APIs. The APIs in this package
convert various data automatically.")
  (:use :cl :cffi :kc.ffi :kc.common)
  (:shadow :append :close :count :delete :get :open :replace :set)
  (:import-from :alexandria :with-gensyms :once-only)
  (:import-from :kc.fs.db :error-message)
  (:import-from :cl-adt :match :ematch)
  (:export :new :delete :open :close :error-message :accept :with-db :get :seize
           :set :add :replace :append :begin-transaction :end-transaction
           :with-transaction :path :clear :count))
