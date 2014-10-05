(in-package :vsanalizer)

(defun sym-name (str-table symbol)
  "Retrieves the name of a SYMBOL from the string table STR-TABLE"
  (let ((table-data (data str-table))
        (beg-pos (name symbol)))
    (map 'string #'code-char (subseq table-data beg-pos
                                     (position 0 table-data :start beg-pos)))))

(defun text-section-idx (elf-obj)
  "Returns postion of .text section in section header table"
  (flet ((text-section-p (sec)
           (string= (name sec) ".text")))
    (position-if #'text-section-p (sections elf-obj))))

(defun get-funcs (elf-obj &key dynamicp)
  "Returns a list of functions (as named regions) from ELF-OBJ.
   DYNAMICP specifies the symbol table to be used."
  (let ((string-table (named-section elf-obj
                                     (if dynamicp ".dynstr" ".strtab")))
        (symtable (named-section elf-obj
                                 (if dynamicp ".dynsym" ".symtab")))
        (text-section-idx (text-section-idx elf-obj)))
    (if (and string-table
             symtable)
        (flet ((cons-entry (sym)
                 (make-named-region :start (value sym)
                                    :end (+ (value sym) (size sym))
                                    :name (sym-name string-table sym)))
               (present-function-p (sym)
                 (and
                  (= (shndx sym) text-section-idx)
                  (eq (elf:type sym) :func))))

          (mapcar #'cons-entry (remove-if-not #'present-function-p (data symtable)))))))
