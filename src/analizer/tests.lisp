(defpackage vsanalizer-test
  (:use #:cl #:vsanalizer)
  (:export #:run-tests))
(in-package :vsanalizer-test)

(defun test-graph (graph)
  (labels ((print-total (node)
             (when node
               (let* ((caller (car node))
                      (callees (mapcar #'car (cdr node)))
                      (diff (- (graph-node-cumul caller)
                               (graph-node-self caller)
                               (reduce #'+ callees :key #'graph-node-cumul))))
                 (format t "~C[~Dm(C-S)(~A)~{ - C(~A)~} = ~D~C[0m~%"
                         #\Esc (if (= diff 0) 32 31)
                         (graph-node-fn-name caller)
                         (mapcar #'graph-node-fn-name callees)
                         diff #\Esc))
               (mapc #'print-total (cdr node)))))
    (mapc #'print-total graph)))

(defun run-tests (graph &key &allow-other-keys)
  (test-graph graph)
  (terpri)
  (flat-report graph))
