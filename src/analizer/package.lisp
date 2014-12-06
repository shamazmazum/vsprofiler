(defpackage vsanalizer
  (:use #:cl #:elf #:esrap)
  (:shadowing-import-from :cl :type)
  (:export #:call-graph #:flat-report #:graphviz-report

           #:graph-node
           #:graph-node-self
           #:graph-node-cumul
           #:graph-node-fn-name
           #:graph-node-obj-name))
