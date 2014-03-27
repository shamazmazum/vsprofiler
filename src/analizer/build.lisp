;; Tool for building analizer

(with-open-file (in "os-name.lisp-expr")
  (loop for os-feature = (read in nil)
        while os-feature
        do
        (pushnew os-feature *features*)))

(require 'asdf)
(asdf:load-system :elf)
(asdf:load-system :esrap)

(load "package.lisp")
(load "procmap-parser.lisp")
(load "symbol-table.lisp")
(load "analizer.lisp")

(defun analizer-impl ()
  (let ((args #+sbcl (cdr sb-ext:*posix-argv*)
              #+clisp ext:*args*))
    (cond
      ((= (length args) 2)
       (print (vsanalizer:report (nth 0 args)
                                 (nth 1 args)))
       (terpri))
      (t
       (format t "Usage: vsanalizer prof.smpl prof.map~%"))))
  #+clisp (ext:quit 0))

#+sbcl
(defun make-image ()
  (sb-ext:save-lisp-and-die "vsanalizer"
                            :executable t
                            :toplevel #'analizer-impl))

#+clisp
(defun make-image ()
  (ext:saveinitmem "vsanalizer"
                   :executable t
                   :norc t
                   :quiet t
                   :init-function #'analizer-impl))

(make-image)
