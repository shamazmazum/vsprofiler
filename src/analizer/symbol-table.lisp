(in-package :vsanalizer)

(defun sym-name (str-table symbol)
  "Retrieves the name of a SYMBOL from the string table STR-TABLE"
  (let ((table-data (data str-table))
        (beg-pos (name symbol)))
    (map 'string #'code-char (subseq table-data beg-pos
                                     (position 0 table-data :start beg-pos)))))

(defun text-section-idx (elf-obj)
  "Returns postion of .text section in section header table"
  (position ".text" (sections elf-obj) :key #'name :test #'string=))

(defun get-funcs (elf-obj &optional (base 0))
  "Returns a list of functions (as named regions) from ELF-OBJ."
  (let* ((dynamicp (eq :shared-object (elf:type (header elf-obj))))
         (string-table-name (if dynamicp ".dynstr" ".strtab"))
         (symbol-table-name (if dynamicp ".dynsym" ".symtab"))
         (string-table (named-section elf-obj string-table-name))
         (symbol-table (named-section elf-obj symbol-table-name))
         (base (if dynamicp base 0))
         (text-section-idx (text-section-idx elf-obj)))
    (if (and string-table
             symbol-table)
        (flet ((cons-entry (sym)
                 (make-named-region :start (+ (value sym) base)
                                    :end (+ (value sym) (size sym) base)
                                    :name (sym-name string-table sym)))
               (present-function-p (sym)
                 (and
                  (= (shndx sym) text-section-idx)
                  (eq (elf:type sym) :func))))

          (mapcar #'cons-entry (remove-if-not #'present-function-p (data symbol-table)))))))
