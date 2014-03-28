(in-package :vsanalizer)

(defun string-at-offset (table offset)
  (let ((end-pos (position 0 table :start offset)))
    (map 'string #'code-char (subseq table offset end-pos))))

(defstruct function-entry
  (start 0 :type (integer 0))
  (end   0 :type (integer 0))
  name)

(defun get-funcs (elf-obj &key dynamic)
  (let ((string-table (named-section elf-obj
                                     (if dynamic ".dynstr" ".strtab")))
        (symtable (named-section elf-obj
                                 (if dynamic ".dynsym" ".symtab"))))
    (if (and string-table
             symtable)
        (flet ((cons-entry (sym)
                 (make-function-entry :start (value sym)
                                      :end (+ (value sym) (size sym))
                                      :name (string-at-offset (data string-table) (name sym)))))
          (mapcar #'cons-entry (data symtable))))))
