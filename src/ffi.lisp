(in-package :kc.ffi.common)

(define-foreign-library libkyotocabinet
  (:unix "libkyotocabinet.so.15"))

(use-foreign-library libkyotocabinet)

;;; It's klugy and not a right way to assume that the size of size_t equals
;;; the pointer's one. But it's an easy way and mostly correct.
(defctype size_t
    #.(ecase (foreign-type-size :pointer)
        (8 :uint64)
        (4 :uint32)))

(defctype kcvisitfull :pointer
  "Call back function to visit a full record.")

(defctype kcvisitempty :pointer
  "Call back function to visit an empty record.")

(defctype kcfileproc :pointer)

(defcstruct kcdb
  "C wrapper of polymorphic database."
  (db :pointer))

(defcstruct kcstr
  "Binary string of byte array."
  (buf (:pointer :char))
  (size size_t))

(defcstruct kcrec
  "Key-Value record."
  (key kcstr)
  (value kcstr))

(defconstant +int64-min+
  #.(- (expt 2 (1- 64)))
  "The minimum of 64-bit signed integer. This is the same as INT64_MIN defined
in stdint.h.")

;;; Error Codes
(defconstant +kcesuccess+   0)
(defconstant +kcenoimpl+    1)
(defconstant +kceinvalid+   2)
(defconstant +kcenorepos+   3)
(defconstant +kcenoperm+    4)
(defconstant +kcebroken+    5)
(defconstant +kceduprec+    6)
(defconstant +kcenorec+     7)
(defconstant +kcelogic+     8)
(defconstant +kcesystem+    9)
(defconstant +kcemisc+      15)

;;; Open Modes
(defconstant +kcoreader+    #b000000001)
(defconstant +kcowriter+    #b000000010)
(defconstant +kcocreate+    #b000000100)
(defconstant +kcotruncate+  #b000001000)
(defconstant +kcoautotran+  #b000010000)
(defconstant +kcoautosync+  #b000100000)
(defconstant +kconolock+    #b001000000)
(defconstant +kcotrylock+   #b010000000)
(defconstant +kconorepair+  #b100000000)

(defcvar ("KCVERSION" +kcversion+) (:pointer :char)
  "The package version.")

(defcvar ("KCVISNOP" +kcvisnop+) (:pointer :char)
  "Special pointer for no operation by the visiting function.")

(defcvar ("KCVISREMOVE" +kcvisremove+) (:pointer :char)
  "Special pointer to remove the record by the visiting function.")

(defcfun "kcfree" :void
  "Release a region allocated in the library."
  (ptr :pointer))

(defcfun "kcecodename" (:pointer :char)
  "Get the readable string of an error code."
  (code :int32))

(in-package :kc.ffi.db)

(defcfun "kcdbnew" (:pointer kcdb)
  "Create a polymorphic database object.")

(defcfun "kcdbdel" :void
  "Destroy a database object."
  (db (:pointer kcdb)))

(defcfun "kcdbopen" :int32
  "Open a database file."
  (db (:pointer kcdb))
  (path (:pointer :char))
  (mode :uint32))

(defcfun "kcdbclose" :int32
  "Close the database file."
  (db (:pointer kcdb)))

(defcfun "kcdbecode" :int32
  "Get the code of the last happened error."
  (db (:pointer kcdb)))

(defcfun "kcdbemsg" (:pointer :char)
  "Get the supplement message of the last happened error."
  (db (:pointer kcdb)))

(defcfun "kcdbaccept" :int32
  "Accept a visitor to a record."
  (db (:pointer kcdb))
  (kbuf (:pointer :char))
  (ksiz size_t)
  (fullproc kcvisitfull)
  (emptyproc kcvisitempty)
  (opq :pointer)
  (writable :int32))

(defcfun "kcdbacceptbulk" :int32
  "Accept a visitor to multiple records at once."
  (db (:pointer kcdb))
  (keys (:pointer kcstr))
  (knum size_t)
  (fullproc kcvisitfull)
  (emptyproc kcvisitempty)
  (opq :pointer)
  (writable :int32))

(defcfun "kcdbiterate" :int32
  "Iterate to accept a visitor for each record."
  (db (:pointer kcdb))
  (fullproc kcvisitfull)
  (opq :pointer)
  (writable :int32))

(defcfun "kcdbscanpara" :int32
  "Scan each record in parallel."
  (db (:pointer kcdb))
  (fullproc kcvisitfull)
  (opq :pointer)
  (thnum size_t))

(defcfun "kcdbset" :int32
  "Set the value of a record."
  (db (:pointer kcdb))
  (kbuf (:pointer :char))
  (ksiz size_t)
  (vbuf (:pointer :char))
  (vsiz size_t))

(defcfun "kcdbadd" :int32
  "Add a record."
  (db (:pointer kcdb))
  (kbuf (:pointer :char))
  (ksiz size_t)
  (vbuf (:pointer :char))
  (vsiz size_t))

(defcfun "kcdbreplace" :int32
  "Replace the value of a record."
  (db (:pointer kcdb))
  (kbuf (:pointer :char))
  (ksiz size_t)
  (vbuf (:pointer :char))
  (vsiz size_t))

(defcfun "kcdbappend" :int32
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

(defcfun "kcdbcas" :int32
  "Perform compare-and-swap."
  (db (:pointer kcdb))
  (kbuf (:pointer :char))
  (ksiz size_t)
  (nvbuf (:pointer :char))
  (nvsiz size_t)
  (ovbuf (:pointer :char))
  (ovsiz size_t))

(defcfun "kcdbremove" :int32
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
  (atomic :int32))

(defcfun "kcdbremovebulk" :int64
  "Remove records at once."
  (db (:pointer kcdb))
  (keys (:pointer kcstr))
  (knum size_t)
  (atomic :int32))

(defcfun "kcdbgetbulk" :int64
  "Retrieve records at once."
  (db (:pointer kcdb))
  (keys (:pointer kcstr))
  (knum size_t)
  (recs (:pointer kcrec))
  (atomic :int32))

(defcfun "kcdbsync" :int32
  "Synchronize updated contents with the file and the device."
  (db (:pointer kcdb))
  (hard :int32)
  (proc kcfileproc)
  (opq :pointer))

(defcfun "kcdboccupy" :int32
  "Occupy database by locking and do something meanwhile."
  (db (:pointer kcdb))
  (writable :int32)
  (proc kcfileproc)
  (opq :pointer))
  
(defcfun "kcdbcopy" :int32
  "Create a copy of the database file."
  (db (:pointer kcdb))
  (dest (:pointer :char)))

(defcfun "kcdbbegintran" :int32
  "Begin transaction."
  (db (:pointer kcdb))
  (hard :int32))

(defcfun "kcdbbegintrantry" :int32
  "Try to begin transaction."
  (db (:pointer kcdb))
  (hard :int32))
  
(defcfun "kcdbendtran" :int32
  "End transaction."
  (db (:pointer kcdb))
  (commit :int32))

(defcfun "kcdbclear" :int32
  "Remove all records."
  (db (:pointer kcdb)))

(defcfun "kcdbdumpsnap" :int32
  "Dump records into a file."
  (db (:pointer kcdb))
  (dest (:pointer :char)))

(defcfun "kcdbloadsnap" :int32
  "Load records from a file."
  (db (:pointer kcdb))
  (src (:pointer :char)))

(defcfun "kcdbcount" :int64
  "Get the number of records."
  (db (:pointer kcdb)))

(defcfun "kcdbsize" :int64
  "Get the size of the database file."
  (db (:pointer kcdb)))

(defcfun "kcdbpath" (:pointer :char)
  "Get the path of the database file."
  (db (:pointer kcdb)))

(defcfun "kcdbstatus" (:pointer :char)
  "Get the miscellaneous status information."
  (db (:pointer kcdb)))

(defcfun "kcdbmatchprefix" :int64
  "Get keys matching a prefix string."
  (db (:pointer kcdb))
  (prefix (:pointer :char))
  (strary (:pointer (:pointer :char)))
  (max size_t))
  
(defcfun "kcdbmatchregex" :int64
  "Get keys matching a regular expression string."
  (db (:pointer kcdb))
  (regex (:pointer :char))
  (strary (:pointer (:pointer :char)))
  (max size_t))

(defcfun "kcdbmerge" :int32
  "Merge records from other databases."
  (db (:pointer kcdb))
  (srcary (:pointer (:pointer kcdb)))
  (srcnum size_t)
  (mode :uint32))

(in-package :kc.ffi.cur)

;(defcfun "kcdbcursor" (:pointer kccur)
;  "Create a polymorphic cursor object."
;  (db (:pointer kcdb)))

;(defcfun "kccurdel" :void
;  "Destroy a cursor object."
;  (cur (:pointer kccur)))

;(defcfun "kccuraccept" :boolean
;  "Accept a visitor to the current record."
;  (cur (:pointer kccur))
;  (fullproc kcvisitfull)
;  (opq :pointer)
;  (writable :boolean)
;  (step :boolean))

;(defcfun "kccursetvalue" :boolean
;  "Set the value of the current record."
;  (cur (:pointer kccur))
;  (vbuf (:pointer :char))
;  (vsiz size_t)
;  (step :boolean))

;(defcfun "kccurremove" :boolean
;  "Remove the current record."
;  (cur (:pointer kccur)))

;(defcfun "kccurgetkey" (:pointer :char)
;  "Get the key of the current record."
;  (cur (:pointer kccur))
;  (sp (:pointer size_t))
;  (step :boolean))

;(defcfun "kccurgetvalue" (:pointer :char)
;  "Get the value of the current record."
;  (cur (:pointer kccur))
;  (sp (:pointer size_t))
;  (step :boolean))

;(defcfun "kccurget" (:pointer :char)
;  "Get a pair of the key and the value of the current record."
;  (cur (:pointer kccur))
;  (ksp (:pointer size_t))
;  (vbp (:pointer (:pointer :char)))
;  (vsp (:pointer size_t))
;  (step :boolean))

;(defcfun "kccurseize" (:pointer :char)
;  "Get a pair of the key and the value of the current record and remove it
;atomically."
;  (cur (:pointer kccur))
;  (ksp (:pointer size_t))
;  (vbp (:pointer (:pointer :char)))
;  (vsp (:pointer size_t)))

;(defcfun "kccurjump" :boolean
;  "Jump the cursor to the first record for forward scan."
;  (cur (:pointer kccur)))

;(defcfun "kccurjumpkey" :boolean
;  "Jump the cursor to a record for forward scan."
;  (cur (:pointer kccur))
;  (kbuf (:pointer :char))
;  (ksiz size_t))

;(defcfun "kccurjumpback" :boolean
;  "Jump the cursor to the last record for backward scan."
;  (cur (:pointer kccur)))

;(defcfun "kccurjumpbackkey" :boolean
;  "Jump the cursor to a record for backward scan."
;  (cur (:pointer kccur))
;  (kbuf (:pointer :char))
;  (ksiz size_t))

;(defcfun "kccurstep" :boolean
;  "Step the cursor to the next record."
;  (cur (:pointer kccur)))

;(defcfun "kccurstepback" :boolean
;  "Step the cursor to the previous record."
;  (cur (:pointer kccur)))

;(defcfun "kccurdb" (:pointer kcdb)
;  "Get the database object."
;  (cur (:pointer kccur)))

;(defcfun "kccurecode" :int32
;  "Get the code of the last happened error."
;  (cur (:pointer kccur)))

;(defcfun "kccuremsg" :string
;  "Get the supplement message of the last happened error."
;  (cur (:pointer kccur)))

(in-package :kc.ffi.idx)

;(defcfun "kcidxnew" (:pointer kcidx)
;  "Create an index database object.")

;(defcfun "kcidxdel" :void
;  "Destroy a database object."
;  (idx (:pointer kcidx)))
