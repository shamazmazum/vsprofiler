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

;; Flat report without building the call graph is the only
;; kind of report currently supported.
(defstruct report-entry
  "Structure used by reporter to represent a function"
  (self  0   :type address)
  (cumul 0   :type address)
  (known nil :type boolean)
  fn-name
  obj-name)

(defun analize (samples-name procmap-name)
  "Processes output of C runtime library and returns a report"
  (let ((samples (read-samples samples-name))
        (procmap (read-procmap procmap-name))
        (*func-table* (make-hash-table))
        (report (make-hash-table)))
    
    (labels ((populate-report (address &key on-top-p)
               (multiple-value-bind (id fn-name obj-name known)
                   (address=>func-name procmap address)
                 (let ((rep-entry (gethash id report)))
                   (cond
                     (rep-entry
                      (incf (report-entry-cumul rep-entry))
                      (if on-top-p (incf (report-entry-self rep-entry))))
                     (t
                      (setf (gethash id report)
                            (make-report-entry :self (if on-top-p 1 0)
                                               :cumul 1
                                               :fn-name fn-name
                                               :obj-name obj-name
                                               :known known)))))))
             
             (analize-backtrace (backtrace)
               (populate-report (car backtrace) :on-top-p t)
               (mapc #'populate-report (cdr backtrace))))

      (mapc #'analize-backtrace samples)
      report)))

(defun get-entries-list (report strip-unknown)
  (let (entries-list)
    (with-hash-table-iterator (get-entry report)
      (labels ((get-entries ()
                 (tagbody loop1%
                    (multiple-value-bind (val id entry)
                        (get-entry)
                      (declare (ignore id))
                      (when val
                        (if (or (not strip-unknown)
                                (report-entry-known entry))
                            (push entry entries-list))
                        (go loop1%))))))
        (get-entries)))
    entries-list))

;; FIXME: primitive table printer with fixed length of fiedls
(defun report (report &key (sorting-method :self)
                           strip-unknown)
  "Prints the report. SORTING-METHOD may be :SELF or :CUMUL
   and determines according to which slot in a report entry
   report will be sorted."
  (declare (type (member :cumul :self) sorting-method))

  (format t "~&      Self         Cumul                    Name        Object file~%")
  (let ((entries (get-entries-list report strip-unknown)))
    (flet ((print-entry (entry)
             (format t "~&~10d ~13d ~24@a ~s~%"
                     (report-entry-self entry)
                     (report-entry-cumul entry)
                     (report-entry-fn-name entry)
                     (report-entry-obj-name entry))))
    (mapc #'print-entry
     (sort entries #'>
           :key
           (cond
             ((eq :cumul sorting-method) #'report-entry-cumul)
             ((eq :self  sorting-method) #'report-entry-self))))))
  t)
