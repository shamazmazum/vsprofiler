(in-package :vsanalizer)

;; Just in case...
(defmacro defvar-unbound (var-name &optional documentation)
  "Helper macro to define unbound variable with documentation"
  `(progn
     (defvar ,var-name)
     ,@(if documentation
           (list `(setf (documentation ',var-name 'variable) ,documentation)))))

(defvar-unbound *func-table*
    "Hash table for parsed elf files")

(defun address-inside-p (address region)
  (declare (type named-region region))
  (and
   (>= address (named-region-start region))
   (<  address (named-region-end   region))))

(defun address-container (procmap address)
  "Finds an object file mapped to the address.
   Also returns a begining of the memory region
   the file is mapped to as an additional value"
  (let ((entry (find address procmap :test #'address-inside-p)))
    (and entry
         (values
          (named-region-start entry)
          (named-region-name entry)))))

#+(or bsd linux)
(defun libraryp (path)
  "Is the filename designates a library?"
  (search ".so" path))

(defun address=>func-name (procmap address &optional (func-table *func-table*))
  "Accepts a process map PROCMAP and an ADDRESS and returns three values:
   1) a begining of the function the ADDRESS belongs to (or just the ADDRESS
   if function is not present in symbol table)
   2) name of the function
   3) name of an object file which contains the function
   4) T if function is present in symbol table, NIL otherwise"
  (let ((fn-start address)
        (fn-name "<Unknown function>")
        fn-obj known)
    (multiple-value-bind (reg-start path)
        (address-container procmap address)
      (when path
        (setq fn-obj path)
        (multiple-value-bind (funcs were-scanned)
            (gethash reg-start func-table)
          (let* ((libraryp (libraryp path))
                 (funcs (if were-scanned funcs
                            (setf (gethash reg-start func-table)
                                  (get-funcs (read-elf path)
                                             :dynamicp libraryp))))
                 (named-function
                  (find (if libraryp (- address reg-start) address)
                        funcs :test #'address-inside-p)))

            (if named-function (setq known t
                                     fn-start (+ (if libraryp reg-start 0)
                                                 (named-region-start named-function))
                                     fn-name (named-region-name named-function)))))))
    (values fn-start
            fn-name
            fn-obj
            known)))

(defstruct graph-node
  "A node of call graph"
  (id    0   :type address)
  (self  0   :type address)
  (cumul 0   :type address)
  (known nil :type boolean)
  fn-name
  obj-name)

(defun call-graph (samples-name procmap-name)
  "Processes output of C runtime library and returns a call graph"
  (let ((samples (read-samples samples-name))
        (procmap (read-procmap procmap-name))
        (*func-table* (make-hash-table)))
    (labels ((populate-graph (subgraph sample)
               (if sample
                   (let ((caller-sample (car sample))
                         (callee-samples (cdr sample)))
                     (multiple-value-bind (caller-id caller-name caller-obj-name caller-known)
                         (address=>func-name procmap caller-sample)

                       (let ((caller-subtree (find caller-id subgraph :key (lambda (subtree)
                                                                                   (graph-node-id (car subtree))))))
                               (if caller-subtree
                                   (let ((caller (car caller-subtree)))
                                     (incf (graph-node-cumul caller))
                                     (incf (graph-node-self caller) (if callee-samples 0 1))
                                     (cons (cons caller (populate-graph (cdr caller-subtree) callee-samples))
                                           (remove caller-subtree subgraph)))
                                   (let ((caller (make-graph-node :id caller-id
                                                                  :self (if callee-samples 0 1)
                                                                  :cumul 1
                                                                  :known caller-known
                                                                  :fn-name caller-name
                                                                  :obj-name caller-obj-name)))
                                     (cons
                                      (cons caller (populate-graph nil callee-samples))
                                      subgraph))))))
                   subgraph)))
      (reduce (lambda (subgraph sample)
                (populate-graph subgraph (reverse sample)))
              samples :initial-value nil))))

(defun flat-report (call-graph &key (sorting-method :self)
                                 strip-unknown (stream *standard-output*) &allow-other-keys)
  "Prints a flat report. SORTING-METHOD may be :SELF or :CUMUL
   and determines according to which slot in a GRAPH-NODE struct
   an entry will be sorted."
  (declare (type (member :self :cumul) sorting-method)
           (type boolean strip-unknown))
  (let (report-list)
    (labels ((populate-list (subtree)
               (when subtree
                 (let ((caller  (car subtree))
                       (callees (cdr subtree)))
                   (let ((caller% (find (graph-node-id caller) report-list :key #'graph-node-id)))
                     ;; If there is a node with such id in the report already, sum the results, otherwise
                     ;; add it to the report
                     (cond
                       (caller%
                        (incf (graph-node-self  caller%)
                              (graph-node-self  caller))
                        (incf (graph-node-cumul caller%)
                              (graph-node-cumul caller)))
                       (t (push caller report-list))))
                   (mapc #'populate-list callees))))

             (print-entry (entry)
               (format stream "~&~10d ~13d ~24@a ~s~%"
                       (graph-node-self entry)
                       (graph-node-cumul entry)
                       (graph-node-fn-name entry)
                       (graph-node-obj-name entry))))

      (mapc #'populate-list call-graph)
      (format stream "~&      Self         Cumul                    Name        Object file~%")
      (mapc #'print-entry
            (sort (if strip-unknown
                      (remove nil report-list :key #'graph-node-known)
                      report-list)
                  #'>
                  :key (cond
                         ((eq :cumul sorting-method) #'graph-node-cumul)
                         ((eq :self  sorting-method) #'graph-node-self))))))
  t)

(defun graphviz-report (call-graph &key (stream *standard-output*) &allow-other-keys)
  "Prints a report in DOT langauge understandable by graphviz."
  (format stream "digraph call_graph {~%")
  (labels ((print-callees (caller-sym callees)
             (loop for callee in callees
                   ;; We use (gensym) here to distinguish two different paths ending up in the same callee
                   for callee-sym = (gensym) do
                  (format stream "~A -> ~A~%"
                          caller-sym callee-sym)
                  ;; Descend the tree
                  (print-caller (car callee) (cdr callee) callee-sym)))
           (print-caller (caller callees caller-sym)
             (when caller
               (format stream "~A[label=\"~A\\nself=~D\\ncumul=~D\"]~%"
                         caller-sym
                         (graph-node-fn-name caller)
                         (graph-node-self caller)
                         (graph-node-cumul caller))
               (print-callees caller-sym callees))))
    (write-string "ROOT[shape=\"rectangle\"]" stream)
    (terpri stream)
    (print-callees :root call-graph))
  (format stream "}~%"))
