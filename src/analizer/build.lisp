;; Tool for building analizer
(with-open-file (in "os-name.lisp-expr")
  (loop for os-feature = (read in nil)
        while os-feature
        do
        (pushnew os-feature *features*)))

(require 'asdf)
(asdf:load-system :elf)
(asdf:load-system :esrap)
(asdf:load-system :apply-argv)

(load "package.lisp")
(load "input-parser.lisp")
(load "symbol-table.lisp") ; Depends on named-region
(load "analizer.lisp") ; Must be the last for analizer
(load "tests.lisp") ; Finally, the tests

(defparameter +name+ "vsanalizer.bin")

(defun print-usage ()
  (format t "Usage: vsanalizer [--sorting-method cumul|self] [--strip-unknown t|nil] [--report flat|graph] prof.smpl prof.map~%")
#+clisp (ext:quit 0)
#+sbcl (sb-ext:quit))

(defun analizer-impl ()
  (let ((args (apply-argv:get-argv)))
    (if (or (< (length (car args)) 2)
            (> (length (cdr args)) 7))
        (print-usage))
    (destructuring-bind ((samples-name procmap-name &rest invalid) &rest options)
        (apply-argv:parse-argv args)
      (if invalid (print-usage))
      (let ((call-graph (vsanalizer:call-graph samples-name procmap-name))
            (report-func #'vsanalizer:flat-report)
            report-args)

        (macrolet ((if-option ((var option) &body body)
                     `(let ((,var (getf options ,option)))
                        (if ,var (progn ,@body)))))
          (if-option (sorting-method :sorting-method)
                     (push (cond
                             ((string-equal sorting-method "self") :self)
                             ((string-equal sorting-method "cumul") :cumul)
                             (t (error "Wrong sorting method")))
                           report-args)
                     (push :sorting-method report-args))

          (if-option (report :report)
                     (setq report-func
                           (cond
                             ((string-equal report "flat") #'vsanalizer:flat-report)
                             ((string-equal report "graph") #'vsanalizer:graphviz-report)
                             ((string-equal report "test") #'vsanalizer-test:run-tests)
                             (t (error "Wrong report type")))))
          (if-option (strip-unknown :strip-unknown)
                     (setq call-graph (vsanalizer:strip-unknown call-graph))))

        (apply report-func call-graph report-args))))
  (format t "Thread profiled ~d~%" (length vsanalizer:*threads*))
  #+clisp (ext:quit 0))

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
