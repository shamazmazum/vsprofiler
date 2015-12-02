(in-package :vsanalizer)

(defparameter *unknown-function-name* "<Unknown function>"
  "Bogus name of an unknown function")

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
         (values t
          (named-region-start entry)
          (named-region-name entry)))))

(defun address=>func-name (procmap address &optional (func-table (if (boundp '*func-table*)
                                                                     *func-table* (make-hash-table))))
  "Accepts a process map PROCMAP and an ADDRESS and returns three values:
   1) a begining of the function the ADDRESS belongs to (or just the ADDRESS
   if function is not present in symbol table)
   2) name of an object file which contains the function (or nil if not known)
   3) name of the function (or nil if not known)"
  (multiple-value-bind (known reg-start path)
      (address-container procmap address)
    (if known
        (multiple-value-bind (funcs were-scanned)
            (gethash reg-start func-table)
          (let* ((funcs (if were-scanned funcs
                            (setf (gethash reg-start func-table)
                                  (get-funcs (read-elf path) reg-start))))
                 (named-function
                  (find address funcs :test #'address-inside-p)))

            (if named-function
                (values (named-region-start named-function) ; Function entry point
                        path                                    ; Path to object file
                        (named-region-name named-function))     ; Function name
                (values address path))))
        address)))

(defstruct graph-node
  "A node of call graph"
  (id    0   :type address)
  (self  0   :type address)
  (cumul 0   :type address)
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
                     (multiple-value-bind (caller-id caller-obj-name caller-name)
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
                                                                  :fn-name caller-name
                                                                  :obj-name caller-obj-name)))
                                     (cons
                                      (cons caller (populate-graph nil callee-samples))
                                      subgraph))))))
                   subgraph)))
      (reduce (lambda (subgraph sample)
                (populate-graph subgraph (reverse sample)))
              samples :initial-value nil))))

(defun strip-unknown (call-graph)
  "Strip call graph of unknown functions"
  (labels ((collect-known (known subtree)
             (let ((caller  (car subtree))
                   (callees (strip-unknown (cdr subtree))))
               (if (graph-node-fn-name caller)
                   (cons (cons caller callees) known)
                   (append callees known)))))
    (if call-graph
        (reduce #'collect-known call-graph :initial-value nil))))

(defun flat-report (call-graph &key (sorting-method :self)
                                 (stream *standard-output*) &allow-other-keys)
  "Prints a flat report. SORTING-METHOD may be :SELF or :CUMUL
   and determines according to which slot in a GRAPH-NODE struct
   an entry will be sorted."
  (declare (type (member :self :cumul) sorting-method))
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
                       (or (graph-node-fn-name entry) *unknown-function-name*)
                       (graph-node-obj-name entry))))

      (mapc #'populate-list call-graph)
      (format stream "~&      Self         Cumul                    Name        Object file~%")
      (mapc #'print-entry
            (sort report-list
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
                         (or (graph-node-fn-name caller) *unknown-function-name*)
                         (graph-node-self caller)
                         (graph-node-cumul caller))
               (print-callees caller-sym callees))))
    (write-string "ROOT[shape=\"rectangle\"]" stream)
    (terpri stream)
    (print-callees :root call-graph))
  (format stream "}~%"))
