;; Tool for building analizer
(load "prereq.lisp")

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
                             (t (error "Wrong report type")))))
          (if-option (strip-unknown :strip-unknown)
                     (setq call-graph (vsanalizer:strip-unknown call-graph))))

        (apply report-func call-graph report-args))))
  (format t "Thread profiled ~d~%" (length vsanalizer:*threads*))
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
