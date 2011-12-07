(in-package :cl-user)

(defpackage :kyoto-cabinet.ffi.types
  (:nicknames :kc.ffi.type)
  (:use :cl :cffi)
  (:export :size_t :kcvisitfull :kcvisitempty :kcfileproc :kcdb :kcstr :kcrec))

(defpackage :kyoto-cabinet.ffi.variables
  (:nicknames :kc.ffi.var)
  (:use :cl :cffi)
  (:export :+int64-min+

           :+kcesuccess+ :+kcenoimpl+ :+kceinvalid+ :+kcenorepos+ :+kcenoperm+
           :+kcebroken+ :+kceduprec+ :+kcenorec+ :+kcelogic+ :+kcesystem+
           :+kcemisc+

           :+kcoreader+ :+kcowriter+ :+kcocreate+ :+kcotruncate+ :+kcoautotran+
           :+kcoautosync+ :+kconolock+ :+kcotrylock+ :+kconorepair+

           :+kcmset+ :+kcmadd+ :+kcmreplace+ :+kcmappend+

           :+kcvisnop+ :+kcvisremove+

           :+kcversion+))

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
  )

(defpackage :kyoto-cabinet.ffi.index-database
  (:nicknames :kc.ffi.idx)
  (:use :cl :cffi :kc.ffi.type)
  )

(defpackage :kyoto-cabinet.ffi
  (:nicknames :kc.ffi)
  (:use :kc.ffi.type :kc.ffi.var :kc.ffi.common :kc.ffi.db :kc.ffi.cur
        :kc.ffi.idx)
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

(defpackage :kyoto-cabinet.types
  (:nicknames :kc.type)
  (:use :cl :cffi)
  (:export ;; Lisp types
           :octet :octets :simple-octets
           ;; CFFI types
           :error-code :open-mode :merge-mode))

(defpackage :kyoto-cabinet.variables
  (:nicknames :kc.var)
  (:use :cl :cffi)
  (:export :*null-pointer*))

(defpackage :kyoto-cabinet.conversion
  (:nicknames :kc.conv)
  (:use :cl :cffi :kc.type)
  (:export :string->foreign-string :octets->foreign-string :x->foreign-string
           :foreign-string->string :foreign-string->octets :foreign-string->x))

(defpackage :kyoto-cabinet.utilities
  (:nicknames :kc.util)
  (:use :cl :cffi :kc.ffi)
  (:export :with-allocated-foreign-string :with-allocated-foreign-strings
           :with-kcmalloced-pointer :aif/ptr :it :set-method->ffi-symbol))

(defpackage :kyoto-cabinet.database.base
  (:nicknames :kc.db.base)
  (:use :cl :cffi :kc.ffi :kc.type :kc.var :kc.conv :kc.util)
  (:shadow :delete :open :close :set :remove :get :set :count :merge)
  (:import-from :alexandria :once-only)
  (:import-from :ppcre :split)
  (:import-from :cl-adt :match :ematch)
  (:export :new :delete :error-message :path :open :close :error-code :accept
           :iterate :scan-in-parallel :set :increment :increment/double :cas
           :remove :get :get/buffer :synchronize :occupy :copy
           :begin-transaction :end-transaction :clear :dump-snapshot
           :load-snapshot :count :size :status :merge))

(defpackage :kyoto-cabinet.database.low-level
  (:nicknames :kc.db.low)
  (:documentation "Contains low-level database APIs. The APIs in this package
directly accept a foreign string of CFFI. They exist for speed.")
  (:use :kc.db.base)
  (:export :accept :iterate :scan-in-parallel :get :get/buffer :set :increment
           :increment/double :cas :remove :synchronize :occupy))

(defpackage :kyoto-cabinet.database
  (:nicknames :kc.db)
  (:documentation "Contains high-level database APIs. The APIs in this package
convert various data automatically.")
  (:use :cl :cffi :kc.ffi :kc.type :kc.var :kc.conv :kc.util)
  (:shadow :set :replace :append :remove :get)
  (:import-from :alexandria :with-gensyms :once-only)
  (:import-from :kc.db.base :new :error-code :error-message :copy
                :begin-transaction :end-transaction :clear :dump-snapshot
                :load-snapshot :size :path :status)
  (:shadowing-import-from :kc.db.base :delete :open :close :count :merge)
  (:export :new :delete :open :close :with-db :error-code :error-message :set
           :add :replace :append :increment :cas :remove :get :seize :copy
           :begin-transaction :end-transaction :with-transaction :clear
           :dump-snapshot :load-snapshot :count :size :path :status :merge))
