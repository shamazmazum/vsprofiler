(in-package :vsanalizer)

(defvar *func-table*)

;; Search in unsorted map is O(n)
;; Another way (not implemented here) is to sort and search
;; as O(log n)
(defun address-container (procmap address)
  "Find to which object file the address is mapped"
  (flet ((resides-in-entry-p (address entry)
           (and
            (>= address (named-region-start entry))
            (<  address (named-region-end entry)))))
    (let ((entry (find address procmap :test #'resides-in-entry-p)))
      (and entry
           (values
            (named-region-start entry)
            (- address (named-region-start entry))
            (named-region-name entry))))))

#+(or bsd linux)
(defun libraryp (path)
  (search ".so" path))

(defun address=>func-name (procmap address &optional (func-table *func-table*))
  (multiple-value-bind (reg-start offset path)
      (address-container procmap address)
    (if path
        (let* ((libraryp (libraryp path))
               (funcs (or (gethash path func-table)
                          (setf (gethash path func-table)
                                (get-funcs (read-elf path)
                                           :dynamic libraryp))))
               (addr (if libraryp offset address))
               (named-function
                (flet ((address-inside (addr% func)
                         (and (>= addr% (named-region-start func))
                              (<  addr% (named-region-end func)))))
                  (find addr funcs :test #'address-inside))))
          
          (if named-function
              (return-from address=>func-name
                (values (+ (if libraryp reg-start 0)
                           (named-region-start named-function))
                        (format nil "~A in ~A"
                                (named-region-name named-function)
                                path)))))))
  (values address
          (format nil "<Unknown function at address ~X>" address)))

;; Flat report without building the call graph is the only
;; kind of report currently supported.
(defstruct report-entry
  (id    0 :type address)
  (self  0 :type address)
  (cumul 0 :type address)
  name)

(defun report (samples-name procmap-name)
  (let ((samples (read-samples samples-name))
        (procmap (read-procmap procmap-name))
        (*func-table* (make-hash-table :test #'equal))
        report)
    
    (labels ((populate-report (address &key on-top-p)
               (multiple-value-bind (id name)
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
                                               :name name)
                            report))))))
             
             (analize-backtrace (backtrace)
               (populate-report (car backtrace) :on-top-p t)
               (mapc #'populate-report (cdr backtrace))))

      (mapc #'analize-backtrace samples)
      report)))
