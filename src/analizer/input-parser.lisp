;; Might be OS dependent
(in-package :vsanalizer)

(defun hex-digit-char-p (char)
  (or (digit-char-p char)
      (find char "abcdef")))

(defrule hex-digit (hex-digit-char-p character))
(defrule dec-digit (digit-char-p character))
(defrule hex-number (and "0" "x" (+ hex-digit))
  (:lambda (list)
    (parse-integer (text (cddr list)) :radix 16)))
(defrule dec-number (and (? "-") (+ dec-digit))
  (:lambda (list)
    (parse-integer (text list) :radix 10)))
(defrule access-type (and (or "-" "r") (or "-" "w") (or "-" "x"))
  (:destructure (read write exec)
                (let (access)
                  (if (string/= "-" read) (push :read access))
                  (if (string/= "-" write) (push :write access))
                  (if (string/= "-" exec) (push :exec access))
                  access)))
(defrule string (+ (alphanumericp character))
  (:function text))
(defrule path (+ (or "-" "." "/" (alphanumericp character)))
  (:function text))

(deftype address () '(integer 0))
(defstruct named-region
  (start 0 :type address)
  (end   0 :type address)
  name)

(defun skip-spaces (list)
  (remove-if #'(lambda (str) (equal " " str)) list))

#+(or dragonfly freebsd)
(defun make-procmap-entry (list &aux (w/o-spaces (skip-spaces list)))
  (cons
   (nth 5 w/o-spaces)
   (make-named-region :start  (nth 0  w/o-spaces)
                      :end    (nth 1  w/o-spaces)
                      :name   (nth 12 w/o-spaces))))

#+dragonfly
(defrule procmap-entry-rule (and hex-number " "
                                 hex-number " "
                                 dec-number " "
                                 dec-number " "
                                 hex-number " "
                                 access-type " "
                                 dec-number " "
                                 dec-number " "
                                 hex-number " "
                                 string  " "
                                 string " "
                                 string " "
                                 path)
  (:function make-procmap-entry))

#+freebsd
(defrule procmap-entry-rule (and hex-number " "
                                 hex-number " "
                                 dec-number " "
                                 dec-number " "
                                 hex-number " "
                                 access-type " "
                                 dec-number " "
                                 dec-number " "
                                 hex-number " "
                                 string  " "
                                 string " "
                                 string " "
                                 path " "
                                 string " "
                                 dec-number)
  (:function make-procmap-entry))

;; FIXME: 0x0 here must correspond with SAMPLE_TERM in C code
(defrule sample-rule (and (* (and hex-number " ")) "0x0")
  (:lambda (list)
    (mapcar #'first (car (butlast list)))))

(defun parse-stream (stream rule)
  (declare (type stream stream))
  (loop for line = (read-line stream nil)
        while line
        collect (parse rule line)))

(defun read-procmap (procmap-name)
  (with-open-file (in procmap-name)
    (flet ((executablep (entry)
             (find :exec (car entry))))
      (mapcar #'cdr
              (remove-if-not #'executablep
                             (parse-stream in 'procmap-entry-rule))))))

(defun read-samples (samples-name)
  (with-open-file (in samples-name)
    (parse-stream in 'sample-rule)))
