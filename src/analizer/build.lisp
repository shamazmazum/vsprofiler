;; Tool for building analizer
(load "prereq.lisp")

(defun analizer-impl ()
  (let ((args #+sbcl (cdr sb-ext:*posix-argv*)
              #+clisp ext:*args*))
    (cond
      ((>= (length args) 2)
       (let ((report (vsanalizer:analize (nth 0 args)
                                         (nth 1 args)))
             report-args)
         (loop for i from 2 below (length args) by 2 do
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
                (t
                 (format t "Cannot understand option ~A. Invoke vsanalizer without parameters to see usage~%"
                         (nth i args)))))
         (print (apply #'vsanalizer:report report report-args))
         (terpri)))
      (t
       (format t "Usage: vsanalizer prof.smpl prof.map [--sorting-method cumul|self] [--strip-unknown t|nil]~%"))))
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
