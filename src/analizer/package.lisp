(defpackage vsanalizer
  (:use #:cl #:elf #:esrap)
  (:shadowing-import-from :cl :type)
  (:export #:report #:analize
           #:report-entry
           #:report-entry-id
           #:report-entry-self
           #:report-entry-cumul
           #:report-entry-known
           #:report-entry-fn-name
           #:report-entry-obj-name))
