(defpackage aoc2015-day10
  (:use common-lisp)
  (:export :part_a :part_b)
  )

(in-package :aoc2015-day10)

(defvar *input*)
(setq *input* 1113122113)

(declaim (ftype (function (fixnum) cons) number-to-list))
(defun number-to-list (nr)
  (map 'list #'digit-char-p (prin1-to-string nr)))

(declaim (ftype (function (cons) cons) group))
(defun group (list)
  (flet ((take-same (item)
           (loop while (and list (eql (first list) item))
                 collect (pop list))))
    (loop while list
          collect (take-same (first list)))))

(declaim (ftype (function (cons) cons) expand))
(defun expand (nr)
  (mapcan (lambda (i) (list (length i) (first i))) (group nr)))


(declaim (ftype (function (fixnum &key (:times integer) (:print-all boolean)) string) iterate_a))
(defun iterate_a (nr &key (times 40) (print-all nil))
  (let ((l (number-to-list nr)))
    (dotimes (i times)
      (setq l (expand l))
      (when print-all (format t "~&~{~a~}" l))
      )
    (format nil "~{~a~}" l)))

(assert
 (equal "312211" (iterate_a 1 :times 5)))

(defun part_a (input)
   (format t "~&Part 1: ~a" (length(iterate_a input))))

(defun part_b (input)
   (format t "~&Part 2: ~a"(length(iterate_a input :times 50))))

(format t "~&Day 10~%")
(part_a *input*)
(part_b *input*)
(terpri)
