(load "prereq.lisp")

(defpackage vsanalizer-test
  (:use #:cl #:vsanalizer))
(in-package :vsanalizer-test)

(defun test-graph (graph)
  (labels ((print-total (node)
             (when node
               (let ((caller (car node))
                     (callees (mapcar #'car (cdr node))))
                 (format t "(C-S)(~A)~{ - C(~A)~} = ~D~%"
                         (graph-node-fn-name caller)
                         (mapcar #'graph-node-fn-name callees)
                         (- (graph-node-cumul caller)
                            (graph-node-self caller)
                            (reduce #'+ callees :key #'graph-node-cumul))))
               (mapc #'print-total (cdr node)))))
    (mapc #'print-total graph)))

(defun run-tests ()
  (let ((graph (call-graph "../runtime/prof.smpl" "../runtime/prof.map")))
    (flat-report graph)
    (terpri)
    (test-graph graph)))

(run-tests)
#+clisp
(ext:quit 0)
