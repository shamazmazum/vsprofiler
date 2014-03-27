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

(defstruct (procmap-entry
             (:constructor make-procmap-entry%))
  (start 0 :type (integer 0))
  (end   0 :type (integer 0))
  access
  path)

(defun skip-spaces (list)
  (remove-if #'(lambda (str) (equal " " str)) list))

#+(or dragonfly freebsd)
(defun make-procmap-entry (list &aux (wo-spaces (skip-spaces list)))
  (make-procmap-entry% :start  (nth 0 wo-spaces)
                       :end    (nth 1 wo-spaces)
                       :access (nth 5 wo-spaces)
                       :path   (nth 12 wo-spaces)))

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

(defun parse-stream (stream rule)
  (declare (type stream stream))
  (loop for line = (read-line stream nil)
        while line
        collect (parse rule line)))
