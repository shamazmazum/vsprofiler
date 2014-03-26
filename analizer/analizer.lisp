(in-package :mycprof)

;; Find in unsorted map is O(n)
;; Another way (not implemented here) is to sort and search
;; as O(log n)

(defvar *elf-table*)

(defun address-container (procmap address)
  "Find to which object file the address is mapped"
  (flet ((resides-in-entry-p (entry)
           (multiple-value-bind (start end)
               (entry-data entry)
             (and
              (>= address start)
              (< address end)))))
    (let* ((entries (remove-if-not #'resides-in-entry-p procmap))
           (entry (first entries)))

      (cond
        ((= (length entries) 0) nil)
        ((= (length entries) 1)
         (multiple-value-bind (start end access path)
             (entry-data entry)
           (declare (ignore end))
           (if (not (member :exec access))
               (error "Address ~X lays in NX area" address))
           (values
            (- address start)
            path)))
        (t
         (error "Overlapped procmap entries for address ~X" address))))))

(defun library-p (path)
  (search ".so" path))

(defun address-func (path address dynamicp)
  (let* ((elf-obj (or (gethash path *elf-table*)
                      (setf (gethash path *elf-table*)
                            (read-elf path))))
         (funcs (get-funcs elf-obj :dynamic dynamicp)))
    (getf
     (flet ((address-inside (adr func)
              (and (>= adr (getf func :start))
                   (< adr (getf func :end)))))
       (find address funcs :test #'address-inside))
     :name)))

(defun address=>report-entry (procmap address)
  (multiple-value-bind (file-offset path)
      (address-container procmap address)
    (let* ((dynamicp (library-p path))
           (funcname (or (address-func
                          path
                          (if dynamicp file-offset address)
                          dynamicp)
                         "(Unknown function)")))
      (list :filename path :funcname funcname))))

(defun report (samples-name procmap-name)
  (let ((samples (with-open-file (in samples-name)
                   (parse-stream in 'hex-number)))
        (procmap (with-open-file (in procmap-name)
                   (parse-stream in 'procmap-entry)))
        (*elf-table* (make-hash-table :test #'equal))
        report)
    
    (labels ((cmp-entry (e1 e2) (equal e1 (cdr e2)))
             (populate-report (address)
               (let* ((rep-entry (address=>report-entry procmap address))
                      (rep-entry% (find rep-entry report :test #'cmp-entry)))
                 (if rep-entry% (incf (car rep-entry%))
                     (push (cons 1 rep-entry) report)))))
      (mapc #'populate-report samples)
      report)))
