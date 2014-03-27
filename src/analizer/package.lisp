(defpackage mycprof
  (:use :cl :elf :esrap)
  (:shadowing-import-from :cl :type)
  (:export :report))
