(defpackage :kyoto-cabinet.ffi.core
  (:nicknames :kc.ffi.core)
  (:use :cl :cffi)
  (:export :kcdbnew :kcdbdel :kcdbopen :kcdbclose :kcdbemsg))
(in-package :kc.ffi.core)

(define-foreign-library libkyotocabinet
  (:unix "libkyotocabinet.so"))

(use-foreign-library libkyotocabinet)

;;;; Types

;;; It's klugy and not a right way to assume that the size of size_t equals
;;; the pointer's one. But it's an easy way and mostly correct.
(defctype size_t
    #.(ecase (foreign-type-size :pointer)
        (8 :uint64)
        (4 :uint32)))

(defcstruct kcdb
  "C wrapper of polymorphic database."
  (db :pointer))

;;;; Variables

(defcvar ("KCVERSION" +kcversion+) :string
  "The package version.")

(defcvar ("KCVISNOP" +kcvisnop+) :pointer
  "Special pointer for no operation by the visiting function.")

(defcvar ("KCVISREMOVE" +kcvisremove+) :pointer
  "Special pointer to remove the record by the visiting function.")

;;;; Functions

(defcfun "kcfree" :void
  "Release a region allocated in the library."
  (ptr :pointer))

(defcfun "kcecodename" :string
  "Get the readable string of an error code."
  (code :int32))

(defcfun "kcdbnew" (:pointer kcdb)
  "Create a polymorphic database object.")

(defcfun "kcdbdel" :void
  "Destroy a database object."
  (db (:pointer kcdb)))

(defcfun "kcdbopen" :boolean
  "Open a database file."
  (db (:pointer kcdb))
  (path :string)
  (mode :uint32))

(defcfun "kcdbclose" :boolean
  "Close the database file."
  (db (:pointer kcdb)))

(defcfun "kcdbecode" :int32
  "Get the code of the last happened error."
  (db (:pointer kcdb)))

(defcfun "kcdbemsg" :string
  "Get the supplement message of the last happened error."
  (db (:pointer kcdb)))

;kcdbaccept
;kcdbacceptbulk
;kcdbiterate
;kcdbscanpara

(defcfun "kcdbset" :boolean
  "Set the value of a record."
  (db (:pointer kcdb))
  (kbuf (:pointer :char))
  (ksiz size_t)
  (vbuf (:pointer :char))
  (vsiz size_t))

(defcfun "kcdbadd" :boolean
  "Add a record."
  (db (:pointer kcdb))
  (kbuf (:pointer :char))
  (ksiz size_t)
  (vbuf (:pointer :char))
  (vsiz size_t))

(defcfun "kcdbreplace" :boolean
  "Replace the value of a record."
  (db (:pointer kcdb))
  (kbuf (:pointer :char))
  (ksiz size_t)
  (vbuf (:pointer :char))
  (vsiz size_t))

(defcfun "kcdbappend" :boolean
  "Append the value of a record."
  (db (:pointer kcdb))
  (kbuf (:pointer :char))
  (ksiz size_t)
  (vbuf (:pointer :char))
  (vsiz size_t))

(defcfun "kcdbincrint" :int64
  "Add a number to the numeric value of a record."
  (db (:pointer kcdb))
  (kbuf (:pointer :char))
  (ksiz size_t)
  (num :int64)
  (orig :int64))

(defcfun "kcdbincrdouble" :double
  "Add a number to the numeric value of a record."
  (db (:pointer kcdb))
  (kbuf (:pointer :char))
  (ksiz size_t)
  (num :double)
  (orig :double))

(defcfun "kcdbcas" :boolean
  "Perform compare-and-swap."
  (db (:pointer kcdb))
  (kbuf (:pointer :char))
  (ksiz size_t)
  (nvbuf (:pointer :char))
  (nvsiz size_t)
  (ovbuf (:pointer :char))
  (ovsiz size_t))

(defcfun "kcdbremove" :boolean
  "Remove a record."
  (db (:pointer kcdb))
  (kbuf (:pointer :char))
  (ksiz size_t))

(defcfun "kcdbget" (:pointer :char)
  "Retrieve the value of a record."
  (db (:pointer kcdb))
  (kbuf (:pointer :char))
  (ksiz size_t)
  (sp (:pointer size_t)))

(defcfun "kcdbgetbuf" :int32
  "Retrieve the value of a record."
  (db (:pointer kcdb))
  (kbuf (:pointer :char))
  (ksiz size_t)
  (vbuf (:pointer :char))
  (max size_t))

(defcfun "kcdbseize" (:pointer :char)
  "Retrieve the value of a record and remove it atomically."
  (db (:pointer kcdb))
  (kbuf (:pointer :char))
  (ksiz size_t)
  (sp (:pointer size_t)))

(defcfun "kcdbsetbulk" :int64
  "Store records at once."
  (db (:pointer kcdb))
  (recs (:pointer kcrec))
  (rnum size_t)
  (atomic :boolean))

(defcfun "kcdbremovebulk" :int64
  "Remove records at once."
  (db (:pointer kcdb))
  (keys (:pointer kcstr))
  (knum size_t)
  (atomic :boolean))

(defcfun "kcdbgetbulk" :int64
  "Retrieve records at once."
  (db (:pointer kcdb))
  (keys (:pointer kcstr))
  (knum size_t)
  (recs (:pointer kcrec))
  (atomic :boolean))

(defcfun "kcdbsync" :boolean
  "Synchronize updated contents with the file and the device."
  (db (:pointer kcdb))
  (hard :boolean)
  (proc kcfileproc)
  (opq :pointer))

(defcfun "kcdboccupy" :boolean
  "Occupy database by locking and do something meanwhile."
  (db (:pointer kcdb))
  (writable :boolean)
  (proc kcfileproc)
  (opq :pointer))
  
(defcfun "kcdbcopy" :boolean
  "Create a copy of the database file."
  (db (:pointer kcdb))
  (dest :string))

(defcfun "kcdbbegintran" :boolean
  "Begin transaction."
  (db (:pointer kcdb))
  (hard :boolean))

(defcfun "kcdbbegintrantry" :boolean
  "Try to begin transaction."
  (db (:pointer kcdb))
  (hard :boolean))
  
(defcfun "kcdbendtran" :boolean
  "End transaction."
  (db (:pointer kcdb))
  (commit :boolean))

(defcfun "kcdbclear" :boolean
  "Remove all records."
  (db (:pointer kcdb)))

(defcfun "kcdbdumpsnap" :boolean
  "Dump records into a file."
  (db (:pointer kcdb))
  (dest :string))

(defcfun "kcdbloadsnap" :boolean
  "Load records from a file."
  (db (:pointer kcdb))
  (src :string))

(defcfun "kcdbcount" :int64
  "Get the number of records."
  (db (:pointer kcdb)))

(defcfun "kcdbsize" :int64
  "Get the size of the database file."
  (db (:pointer kcdb)))

(defcfun "kcdbpath" :string
  "Get the path of the database file."
  (db (:pointer kcdb)))

(defcfun "kcdbstatus" :string
  "Get the miscellaneous status information."
  (db (:pointer kcdb)))

(defcfun "kcdbmatchprefix" :int64
  "Get keys matching a prefix string."
  (db (:pointer kcdb))
  (prefix :string)
  (strary (:pointer (:pointer :char)))
  (max size_t))
  
(defcfun "kcdbmatchregex" :int64
  "Get keys matching a regular expression string."
  (db (:pointer kcdb))
  (regex :string)
  (strary (:pointer (:pointer :char)))
  (max size_t))

(defcfun "kcdbmerge" :boolean
  "Merge records from other databases."
  (db (:pointer kcdb))
  (srcary (:pointer (:pointer kcdb)))
  (srcnum size_t)
  (mode :uint32))

(defcfun "kcdbcursor" (:pointer kccur)
  "Create a polymorphic cursor object."
  (db (:pointer kcdb)))

(defcfun "kccurdel" :void
  "Destroy a cursor object."
  (cur (:pointer kccur)))

(defcfun "kccuraccept" :boolean
  "Accept a visitor to the current record."
  (cur (:pointer kccur))
  (fullproc kcvisitfull)
  (opq :pointer)
  (writable :boolean)
  (step :boolean))

(defcfun "kccursetvalue" :boolean
  "Set the value of the current record."
  (cur (:pointer kccur))
  (vbuf (:pointer :char))
  (vsiz size_t)
  (step :boolean))

(defcfun "kccurremove" :boolean
  "Remove the current record."
  (cur (:pointer kccur)))

(defcfun "kccurgetkey" (:pointer :char)
  "Get the key of the current record."
  (cur (:pointer kccur))
  (sp (:pointer size_t))
  (step :boolean))

(defcfun "kccurgetvalue" (:pointer :char)
  "Get the value of the current record."
  (cur (:pointer kccur))
  (sp (:pointer size_t))
  (step :boolean))

(defcfun "kccurget" (:pointer :char)
  "Get a pair of the key and the value of the current record."
  (cur (:pointer kccur))
  (ksp (:pointer size_t))
  (vbp (:pointer (:pointer :char)))
  (vsp (:pointer size_t))
  (step :boolean))

(defcfun "kccurseize" (:pointer :char)
  "Get a pair of the key and the value of the current record and remove it
atomically."
  (cur (:pointer kccur))
  (ksp (:pointer size_t))
  (vbp (:pointer (:pointer :char)))
  (vsp (:pointer size_t)))

(defcfun "kccurjump" :boolean
  "Jump the cursor to the first record for forward scan."
  (cur (:pointer kccur)))

(defcfun "kccurjumpkey" :boolean
  "Jump the cursor to a record for forward scan."
  (cur (:pointer kccur))
  (kbuf (:pointer :char))
  (ksiz size_t))

(defcfun "kccurjumpback" :boolean
  "Jump the cursor to the last record for backward scan."
  (cur (:pointer kccur)))

(defcfun "kccurjumpbackkey" :boolean
  "Jump the cursor to a record for backward scan."
  (cur (:pointer kccur))
  (kbuf (:pointer :char))
  (ksiz size_t))

(defcfun "kccurstep" :boolean
  "Step the cursor to the next record."
  (cur (:pointer kccur)))

(defcfun "kccurstepback" :boolean
  "Step the cursor to the previous record."
  (cur (:pointer kccur)))

(defcfun "kccurdb" (:pointer kcdb)
  "Get the database object."
  (cur (:pointer kccur)))

(defcfun "kccurecode" :int32
  "Get the code of the last happened error."
  (cur (:pointer kccur)))

(defcfun "kccuremsg" :string
  "Get the supplement message of the last happened error."
  (cur (:pointer kccur)))

(defcfun "kcidxnew" (:pointer kcidx)
  "Create an index database object.")

(defcfun "kcidxdel" :void
  "Destroy a database object."
  (idx (:pointer kcidx)))
