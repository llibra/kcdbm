(in-package :cl-user)

(defpackage :kyoto-cabinet.test
  (:nicknames :kc.test)
  (:use :cl)
  (:import-from :alexandria :once-only)
  (:export :all :db :cur :idx))
