;; Tool for building analizer
(load "prereq.lisp")

(defun print-usage ()
  (format t "Usage: vsanalizer [--sorting-method cumul|self] [--strip-unknown t|nil] [--output filename] prof.smpl prof.map flat|graph~%")
#+clisp (ext:quit 0)
#+sbcl (sb-ext:quit))

(defun analizer-impl ()
  (let ((args (apply-argv:parse-argv (apply-argv:get-argv))))
    (if (or (/= (length (car args)) 3)
            (> (length (cdr args)) 6))
        (print-usage))
    (destructuring-bind ((samples-name procmap-name report-type) &rest options)
        args
      (let ((call-graph (vsanalizer:call-graph samples-name procmap-name))
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

          (if-option (strip-unknown :strip-unknown)
                     (push (string-equal "t" strip-unknown) report-args)
                     (push :strip-unknown report-args))

          (if-option (output :output)
                     (push (open output
                                 :direction :output
                                 :if-does-not-exist :create
                                 :if-exists :supersede) report-args)
                     (push :output report-args))

          (unwind-protect
               (apply (cond
                        ((string= report-type "flat")  #'vsanalizer:flat-report)
                        ((string= report-type "graph") #'vsanalizer:graphviz-report)
                        (t (error "Report type must be 'flat' or 'graph'~%")))
                      call-graph report-args)
            (if-option (output :output)
                       (close (getf report-args :output))))))))
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
