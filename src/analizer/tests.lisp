(load "prereq.lisp")

(defpackage vsanalizer-test
  (:use #:cl #:vsanalizer))
(in-package :vsanalizer-test)

(defun find-with-name (name report)
  (find name report
        :test #'(lambda (name re)
                  (string= name (report-entry-fn-name re)))))

(defun test-tree (rep tree)
  (if tree
      (let ((callees (mapcar #'(lambda (re) (find-with-name (car re) rep)) (cdr tree)))
            (caller  (find-with-name (car tree) rep)))
        (format t "(C-S)(~A) - ~{C(~A)~^ - ~} = ~D~%"
                (report-entry-fn-name caller)
                (mapcar #'report-entry-fn-name callees)
                (- (report-entry-cumul caller)
                   (report-entry-self caller)
                   (reduce #'(lambda (acc x)
                               (+ acc (report-entry-cumul x)))
                           callees :initial-value 0)))

        (mapc #'(lambda (callee) (test-tree rep callee)) (cdr tree))))t
        t)

(defun run-tests ()
  (let ((report (analize "../runtime/prof.smpl" "../runtime/prof.map")))
    (print report)
    (terpri)
    (test-tree report
               '("main" ("get_value" ("factor")) ("crc8")))))

(run-tests)
#+clisp
(ext:quit 0)
