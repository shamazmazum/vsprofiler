;; Tool for building analizer
(load "prereq.lisp")

(defun analizer-impl ()
  (let ((args #+sbcl (cdr sb-ext:*posix-argv*)
              #+clisp ext:*args*))
    (cond
      ((and (>= (length args) 3)
            (< (length args) 8))
       (let ((report (vsanalizer:call-graph (nth 0 args)
                                            (nth 1 args)))
             (report-type (nth 2 args))
             report-args
             (output *standard-output*))
         (loop for i from 3 below (length args) by 2 do
              (cond
                ((and (string= "--sorting-method" (nth i args))
                      (string= "self" (nth (1+ i) args)))
                 (setq report-args (nconc (list :sorting-method :self) report-args)))
                
                ((and (string= "--sorting-method" (nth i args))
                      (string= "cumul" (nth (1+ i) args)))
                 (setq report-args (nconc (list :sorting-method :cumul) report-args)))
                
                ((and (string= "--strip-unknown" (nth i args))
                      (string= "t" (nth (1+ i) args)))
                 (setq report-args (nconc (list :strip-unknown t) report-args)))
                ((string= "--output" (nth i args))
                 (setq output (open (nth (1+ i) args)
                                    :direction :output
                                    :if-does-not-exist :create
                                    :if-exists :supersede)))
                (t
                 (format t "Cannot understand option ~A. Invoke vsanalizer without parameters to see usage~%"
                         (nth i args)))))
         (unwind-protect
              (apply (cond
                       ((string= report-type "flat")  #'vsanalizer:flat-report)
                       ((string= report-type "graph") #'vsanalizer:graphviz-report)
                       (t (error "Report type must be 'flat' or 'graph'~%")))
                     report :stream output report-args)
           (close output))))
      (t
       (format t "Usage: vsanalizer prof.smpl prof.map flat|graph [--sorting-method cumul|self] [--strip-unknown t|nil] [--output filename]~%"))))
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
