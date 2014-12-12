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
#+netbsd
(defrule hex-number2  (+ hex-digit)
  (:lambda (list)
    (parse-integer (text  list) :radix 16)))
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

#+(or freebsd dragonfly)
(defrule copy-on-write (or "COW" "NCOW"))
#+netbsd
(defrule copy-on-write (or "p" "s"))
(defrule needs-copy (or "NC" "NNC"))
(defrule cred (or "CH" "NCH"))
#+freebsd
(defrule obj-type (or "default" "vnode" "swap" "device" "none"))
#+dragonfly
(defrule obj-type (or "default" "vnode" "swap" "device" "mgtdevice" "none"))

;; FIXME: any better ideas how to parse paths?
(defun allowed-in-path (char)
  (and (>  (char-code char) 32)
       (<= (char-code char) 127)))
(defrule path (+ (allowed-in-path character))
  (:function text))

(defun skip-spaces (list)
  (remove-if #'(lambda (str) (equal " " str)) list))

(deftype address () '(integer 0))
(defstruct named-region
  "Specifies named memory region (such as function or mapped file)"
  (start 0 :type address)
  (end   0 :type address)
  name)

(defstruct (procmap-entry
             (:include named-region)
             (:constructor make-procmap-entry%))
  "Actually this is a named region with access rules"
  (access nil :type list))

#+(or freebsd dragonfly)
(defun make-procmap-entry (list &aux (w/o-spaces (skip-spaces list)))
  (make-procmap-entry% :start  (nth 0  w/o-spaces)
                       :end    (nth 1  w/o-spaces)
                       :name   (nth 12 w/o-spaces)
                       :access (nth 5  w/o-spaces)))
#+netbsd
(defun make-procmap-entry (list &aux (w/o-spaces (skip-spaces list)))
  (make-procmap-entry% :start  (nth 0  w/o-spaces)
                       :end    (nth 2  w/o-spaces)
                       :name   (nth 11 w/o-spaces)
                       :access (nth 3  w/o-spaces)))

#+(or freebsd dragonfly)
(defrule procmap-entry-rule #.`(and hex-number " "
                                    hex-number " "
                                    dec-number " "
                                    dec-number " "
                                    (or hex-number
                                        dec-number) " "
                                    access-type " "
                                    dec-number " "
                                    dec-number " "
                                    hex-number " "
                                    copy-on-write  " "
                                    needs-copy " "
                                    obj-type " "
                                    path
                                    #+freebsd
                                    ,@'(" " cred
                                        " " dec-number))
         (:function make-procmap-entry))

;; NetBSD without linux mode
#+netbsd
(defrule procmap-entry-rule (and hex-number "-" hex-number " "
                                 access-type
                                 copy-on-write " "
                                 hex-number2 " "
                                 hex-number2 ":" hex-number2 " "
                                 dec-number (+ " ") (? path))
  (:function make-procmap-entry))

;; FIXME: 0x0 here must correspond with SAMPLE_TERM in C code
(defrule sample-rule (and (* (and hex-number " ")) "0x0")
  (:lambda (list)
    (mapcar #'first (car (butlast list)))))

(defun parse-stream (stream rule)
  "Parse an input stream line-by-line, applying the rule"
  (declare (type stream stream))
  (loop for line = (read-line stream nil)
        while line
        collect (parse rule line)))

(defun read-procmap (procmap-name)
  "Parse procmap file with the name PROCMAP-NAME"
  (with-open-file (in procmap-name)
    (flet ((executablep (entry)
             (find :exec (procmap-entry-access entry))))
      (remove-if-not #'executablep
                     (parse-stream in 'procmap-entry-rule)))))

#|(defun read-samples (samples-name)
  "Parse file of samples with the name SAMPLES-NAME"
  (with-open-file (in samples-name)
    (parse-stream in 'sample-rule)))|#

;; Non-esrap sample reader (It turns out that esrap is too slow and a bottleneck)
(defun split-sequence (sequence delimiter &optional res end)
  "Break SEQUENCE to a list of subsequences delimited by DELIMITER"
  (let ((pos (position delimiter sequence :from-end t :end end)))
    (if pos
        (split-sequence sequence delimiter (cons (subseq sequence (1+ pos) end) res)
                        pos)
      (cons (subseq sequence 0 end) res))))

(defun parse-c-hex (string)
  "Parse a hex number printed by C library"
  (parse-integer (subseq string 2)
                 :radix #x10))

(defun read-samples (samples-name)
  "Parse file of samples with the name SAMPLES-NAME"
  (with-open-file (in samples-name)
    (loop for line = (read-line in nil)
          while line collect
          ;; 'The last' on next line is 0x0, so skip it
          (mapcar #'parse-c-hex (butlast (split-sequence line #\Space))))))
