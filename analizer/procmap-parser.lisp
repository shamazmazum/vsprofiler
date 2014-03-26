;; Might be OS dependant
(in-package :mycprof)

(defun hex-digit-char-p (char)
  (or (digit-char-p char)
      (find char "abcdef")))

(defrule hex-digit (hex-digit-char-p character))
(defrule dec-digit (digit-char-p character))
(defrule hex-number (and "0" "x" (+ hex-digit))
  (:lambda (list)
    (parse-integer (text (cddr list)) :radix 16)))
(defrule dec-number (+ dec-digit)
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

(defrule procmap-entry (and hex-number " "
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
  (:lambda (list)
    (loop for x in list
          when (not (equalp x " "))
          collect x)))

(defun parse-stream (stream rule)
  (declare (type stream stream))
  (loop for line = (read-line stream nil)
        while line
        collect (parse rule line)))

(defun entry-data (entry)
  (values (nth 0  entry)   ; Start
          (nth 1  entry)   ; End
          (nth 5  entry)   ; Access
          (nth 12 entry))) ; Path
