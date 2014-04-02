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

(defun address-container (procmap address)
  "Finds an object file mapped to the address.
   Also returns a begining of the memory region
   the file is mapped to as an additional value"
  (flet ((resides-in-entry-p (address entry)
           (and
            (>= address (named-region-start entry))
            (<  address (named-region-end entry)))))
    (let ((entry (find address procmap :test #'resides-in-entry-p)))
      (and entry
           (values
            (named-region-start entry)
            (named-region-name entry))))))

#+(or bsd linux)
(defun libraryp (path)
  "Is the filename designates a library?"
  (search ".so" path))

(defun address=>func-name (procmap address &optional (func-table *func-table*))
  "Accepts a process map PROCMAP and an ADDRESS and returns three values:
   1) a begining of the function the ADDRESS belongs to (or just the ADDRESS
   if function is not present in symbol table)
   2) String representation of the function
   3) T if function is present in symbol table, NIL otherwise"
  (multiple-value-bind (reg-start path)
      (address-container procmap address)
    (if path
        (multiple-value-bind (funcs were-scanned)
            (gethash reg-start func-table)
          (let* ((libraryp (libraryp path))
                 (funcs (if were-scanned funcs
                            (setf (gethash reg-start func-table)
                                  (get-funcs (read-elf path)
                                             :dynamicp libraryp))))
                 (named-function
                  (flet ((address-inside (addr% func)
                           (and (>= addr% (named-region-start func))
                                (<  addr% (named-region-end func)))))
                    (find (if libraryp (- address reg-start) address)
                          funcs :test #'address-inside))))
          
            (if named-function
                (return-from address=>func-name
                  (values (+ (if libraryp reg-start 0)
                             (named-region-start named-function))
                          (format nil "~A in ~A"
                                  (named-region-name named-function)
                                  path)
                          t)))))))
  (values address
          (format nil "<Unknown function at address ~X>" address)
          nil))

;; Flat report without building the call graph is the only
;; kind of report currently supported.
(defstruct report-entry
  "Structure used by reporter to represent a function"
  (id    0   :type address)
  (self  0   :type address)
  (cumul 0   :type address)
  (known nil :type boolean)
  name)

(defun analize (samples-name procmap-name)
  "Processes output of C runtime library and returns a report"
  (let ((samples (read-samples samples-name))
        (procmap (read-procmap procmap-name))
        (*func-table* (make-hash-table))
        report)
    
    (labels ((populate-report (address &key on-top-p)
               (multiple-value-bind (id name known)
                   (address=>func-name procmap address)
                 (let ((rep-entry (find id report
                                        :test #'(lambda (id rep-entry) (= id (report-entry-id rep-entry))))))
                   (cond
                     (rep-entry
                      (incf (report-entry-cumul rep-entry))
                      (if on-top-p (incf (report-entry-self rep-entry))))
                     (t
                      (push (make-report-entry :id id
                                               :self (if on-top-p 1 0)
                                               :cumul 1
                                               :name name
                                               :known known)
                            report))))))
             
             (analize-backtrace (backtrace)
               (populate-report (car backtrace) :on-top-p t)
               (mapc #'populate-report (cdr backtrace))))

      (mapc #'analize-backtrace samples)
      report)))

(defun report (report &key (sorting-method :self)
                           strip-unknown)
  "Prints the report. SORTING-METHOD may be :SELF or :CUMUL
   and determines according to which slot in a report entry
   report will be sorted."
  (declare (type (member :cumul :self) sorting-method))
  (let* ((slot-reader (cond
                        ((eq :cumul sorting-method) #'report-entry-cumul)
                        ((eq :self sorting-method) #'report-entry-self)))
         (report 
          (flet ((sorting-pred (re1 re2)
                   (> (funcall slot-reader re1)
                      (funcall slot-reader re2))))
            (sort report #'sorting-pred))))
    (if strip-unknown (remove-if-not #'report-entry-known report) report)))
