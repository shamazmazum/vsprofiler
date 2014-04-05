(with-open-file (in "os-name.lisp-expr")
  (loop for os-feature = (read in nil)
        while os-feature
        do
        (pushnew os-feature *features*)))

(require 'asdf)
(asdf:load-system :elf)
(asdf:load-system :esrap)

(load "package.lisp")
(load "input-parser.lisp")
(load "symbol-table.lisp") ; Depends on named-region
(load "analizer.lisp") ; Must be the last
