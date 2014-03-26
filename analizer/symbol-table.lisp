(in-package :mycprof)

(defun string-at-offset (table offset)
  (let ((end-pos (position 0 table :start offset)))
    (map 'string #'code-char (subseq table offset end-pos))))

(defun get-func-syms (elf-obj &key dynamic)
  (let ((syms (data (named-section elf-obj
                                   (if dynamic ".dynsym" ".symtab")))))
    (remove-if-not #'(lambda (sym)
                       (eq :func (elf:type sym))) syms)))

(defun get-funcs (elf-obj &key dynamic)
  (let ((funcsym (get-func-syms elf-obj :dynamic dynamic))
        (string-table (data (named-section elf-obj
                                           (if dynamic ".dynstr" ".strtab")))))
    (flet ((cons-entry (sym)
             (list :name (string-at-offset string-table (name sym))
                   :start (value sym)
                   :end (+ (value sym) (size sym)))))
      (mapcar #'cons-entry funcsym))))
