(in-package :vsanalizer)

;; Find in unsorted map is O(n)
;; Another way (not implemented here) is to sort and search
;; as O(log n)

(defvar *func-table*)

(defun address-container (procmap address)
  "Find to which object file the address is mapped"
  (flet ((resides-in-entry-p (address entry)
           (and
            (>= address (procmap-entry-start entry))
            (<  address (procmap-entry-end entry)))))
    (let ((entry (find address procmap :test #'resides-in-entry-p)))
      (cond
        (entry
         (if (not (member :exec (procmap-entry-access entry)))
               (error "Address ~X lays in NX area" address))
         (values
          (- address (procmap-entry-start entry))
          (procmap-entry-path entry)))))))

#+(or bsd linux)
(defun libraryp (path)
  (search ".so" path))

(defun address=>func-name (path address offset)
  (let* ((libraryp (libraryp path))
         (funcs (or (gethash path *func-table*)
                    (setf (gethash path *func-table*)
                          (get-funcs (read-elf path)
                                     :dynamic libraryp))))
         (addr (if libraryp offset address))
         (func-entry
          (flet ((address-inside (adr func)
                   (and (>= adr (function-entry-start func))
                        (<  adr (function-entry-end func)))))
            (find (if libraryp offset address) funcs :test #'address-inside))))
    
    (if func-entry (function-entry-name func-entry) (format nil "<Unknown function at address ~X>" addr))))

(defstruct report-entry
  name
  file
  count)

(defun report-entry-eql (re1 re2)
  (and (string= (report-entry-name re1)
                (report-entry-name re2))
       (string= (report-entry-file re1)
                (report-entry-file re2))))

(defun address=>report-entry (procmap address)
  (multiple-value-bind (file-offset path)
      (address-container procmap address)
    (let ((func-name (address=>func-name path address file-offset)))
      (make-report-entry :name func-name :file path :count 1))))

(defun report (samples-name procmap-name)
  (let ((samples (with-open-file (in samples-name)
                   (parse-stream in 'hex-number)))
        (procmap (with-open-file (in procmap-name)
                   (parse-stream in 'procmap-entry-rule)))
        (*func-table* (make-hash-table :test #'equal))
        report)
    
    (flet ((populate-report (address)
             (let* ((rep-entry (address=>report-entry procmap address))
                    (rep-entry% (find rep-entry report :test #'report-entry-eql)))
               (if rep-entry% (incf (report-entry-count rep-entry%))
                   (push rep-entry report))))
           (report-entry-> (re1 re2)
             (> (report-entry-count re1)
                (report-entry-count re2))))
      (mapc #'populate-report samples)
      (sort report #'report-entry->))))
