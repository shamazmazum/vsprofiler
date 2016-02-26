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
(load "input-parser.lisp")
(load "symbol-table.lisp") ; Depends on named-region
(load "analizer.lisp") ; Must be the last for analizer
(load "tests.lisp") ; Finally, the tests

(defpackage vsanalizer-builder
  (:use :cl :vsanalizer))
(in-package :vsanalizer-builder)

(defparameter +name+ "vsanalizer")

(defun get-argv ()
  #+sbcl (cdr sb-ext:*posix-argv*)
  #-sbcl (error "GET-ARGV is not implemented"))

(defun parse-argv (args spec)
  (let ((option-specs
         (mapcar (lambda (spec)
                   (concatenate 'string "--"
                                (string-downcase (symbol-name (car spec)))))
                 spec))
        parameters options optional-parameters)
    (loop for arg = (pop args)
          while arg do
         (let ((spec-pos (position arg option-specs :test #'string=)))
           (if spec-pos
               ;; This is an option
               (let ((spec (nth spec-pos spec)))
                 (destructuring-bind (option . argp) spec
                   (if argp ; Additional argument is required
                       (progn
                         (push (pop args) optional-parameters)
                         (push option optional-parameters))
                       (push option options))))
               (push arg parameters))))
    (values options optional-parameters (reverse parameters))))

(defun print-usage ()
  (format t "Usage:~%")
  (format t "vsanalizer flat [--sorting-method cumul|self] [--strip-unknown] prof.smpl prof.map~%")
  (format t "vsanalizer graph [--strip-unknown] prof.smpl prof.map~%")
  (format t "vsanalizer histogram <func-name> prof.smpl prof.map~%")

  #+clisp (ext:quit 0)
  #+sbcl (sb-ext:exit))

(defun do-histogram-report (args)
  (multiple-value-bind (options optional-parameters parameters)
      (parse-argv args nil)
    (if (or options optional-parameters
            (/= (length parameters) 3))
        (print-usage))
    (histogram-report (second parameters)
                      (third parameters)
                      (first parameters))
    #+clisp (ext:quit 0)))

(defun do-flat-report (args)
  (multiple-value-bind (options optional-parameters parameters)
      (parse-argv args '((:sorting-method . t)
                         (:strip-unknown . nil)))
    (let ((sorting-method
           (let ((sorting-method-string (getf optional-parameters :sorting-method)))
             (if sorting-method-string
                 (intern
                  (string-upcase sorting-method-string)
                  (find-package :keyword))
                 :self))))
      (if (or (/= (length parameters) 2)
              (not (member sorting-method '(:cumul :self))))
          (print-usage))
      (let ((graph (call-graph
                    (first parameters)
                    (second parameters))))
        (if (member :strip-unknown options)
            (setq graph (strip-unknown graph)))
        (flat-report graph :sorting-method sorting-method))))
  (format t "Thread profiled ~d~%" (length *threads*))
  #+clisp (ext:quit 0))

(defun do-graph-report (args)
  (multiple-value-bind (options optional-parameters parameters)
      (parse-argv args '((:strip-unknown . nil)))
    (if (or (/= (length parameters) 2)
            optional-parameters)
        (print-usage))
    (let ((graph (call-graph
                  (first parameters)
                  (second parameters))))
      (if (member :strip-unknown options)
          (setq graph (strip-unknown graph)))
      (graphviz-report graph)))
  #+clisp (ext:quit 0))

(defun do-test (args)
  (multiple-value-bind (options optional-parameters parameters)
      (parse-argv args nil)
    (if (or (/= (length parameters) 2)
            optional-parameters options)
        (print-usage))
    (let ((graph (call-graph
                  (first parameters)
                  (second parameters))))
      (vsanalizer-test:run-tests graph)))
  #+clisp (ext:quit 0))

(defun analizer-impl ()
  (let ((command (car (get-argv)))
        (args (cdr (get-argv))))
    (cond
      ((string= command "flat")
       (do-flat-report args))
      ((string= command "graph")
       (do-graph-report args))
      ((string= command "histogram")
       (do-histogram-report args))
      ((string= command "test")
       (do-test args))
      (t (print-usage)))))

#+sbcl
(defun make-image ()
  (sb-ext:save-lisp-and-die +name+
                            :executable t
                            :toplevel #'analizer-impl))

#+clisp
(defun make-image ()
  (ext:saveinitmem +name+
                   :executable t
                   :norc t
                   :quiet t
                   :init-function #'analizer-impl))

(make-image)
