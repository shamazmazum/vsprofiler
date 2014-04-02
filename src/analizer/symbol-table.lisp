(in-package :vsanalizer)

(defun string-at-offset (table offset)
  "Retrieves a string from the string table TABLE
   starting from offset OFFSET"
  (let ((end-pos (position 0 table :start offset)))
    (map 'string #'code-char (subseq table offset end-pos))))

(defun get-funcs (elf-obj &key dynamicp)
  "Returns a list of functions (as named regions) from ELF-OBJ.
   DYNAMICP specifies the symbol table to be used."
  (let ((string-table (named-section elf-obj
                                     (if dynamicp ".dynstr" ".strtab")))
        (symtable (named-section elf-obj
                                 (if dynamicp ".dynsym" ".symtab"))))
    (if (and string-table
             symtable)
        (flet ((cons-entry (sym)
                 (make-named-region :start (value sym)
                                    :end (+ (value sym) (size sym))
                                    :name (string-at-offset (data string-table) (name sym)))))
          (mapcar #'cons-entry (data symtable))))))
