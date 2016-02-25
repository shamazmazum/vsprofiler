(defpackage vsanalizer
  (:use #:cl #:elf #:esrap)
  (:shadowing-import-from :cl :type)
  (:export #:call-graph #:flat-report #:graphviz-report #:histogram-report
           #:strip-unknown

           #:graph-node
           #:graph-node-self
           #:graph-node-cumul
           #:graph-node-fn-name
           #:graph-node-obj-name

           #:*threads*))
