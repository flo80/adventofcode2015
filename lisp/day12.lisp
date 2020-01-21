(defpackage aoc2015-day12
  (:use common-lisp #:alexandria #:jonathan)
  (:export :part_a :part_b)
  )

(in-package :aoc2015-day12)

(defconstant *input* (alexandria:read-file-into-string "./input/day12"))


(defun part_a (input)
  "manually parse string"
  (let ((curr nil)
        (numbers '(0)))
    (loop for c across input
          do (cond
               ((digit-char-p c) (push c curr))
               ((eq #\- c) (push c curr))
               (t (if (not (null curr))
                      (progn
                        (push (parse-integer (coerce (reverse curr) 'string)) numbers)
                        (setf curr nil))))))
    (if (not (null curr))
        (push (parse-integer (coerce (reverse curr) 'string)) numbers ))
    (apply #'+ numbers)))


(assert (eq (part_a "[1,2,3]") 6))
(assert (eq (part_a "{\"a\":2,\"b\":4}") 6))
(assert (eq (part_a "[[[3]]]") 3))
(assert (eq (part_a "{\"a\":{\"b\":4},\"c\":-1}") 3))
(assert (eq (part_a "{\"a\":[-1,1]}") 0))
(assert (eq (part_a "[-1,{\"a\":1}]") 0))
(assert (eq (part_a "[]") 0))
(assert (eq (part_a "{}") 0))


(defun part_a-with-parser (input)
  (get-value (jonathan:parse input)))

(defun part_b (input)
  (get-value (jonathan:parse input :as :hash-table) :skip-red t))

(assert (eq (part_a "[1,2,3]") 6))
(assert (eq (part_b "[1,{\"c\":\"red\",\"b\":2},3]") 4))
(assert (eq (part_b "{\"d\":\"red\",\"e\":[1,2,3,4],\"f\":5}") 0))
(assert (eq (part_b "[1,\"red\",5]") 6))

(defun get-value (obj &key (skip-red nil))
  "iterate through the json decoding"
  ;(format t "~&~a" obj)
  (flet ((gv (l) (get-value l :skip-red skip-red)))
    (cond
      ((typep obj 'number) obj)
    ((typep obj 'string) 0)
    ((typep obj 'KEYWORD) 0)
    ((typep obj 'hash-table)
     (if skip-red
         (cond
           ; check if there is a red
           ((some (lambda (x) (equal x "red")) (hash-table-values obj)) 0)
           (t (apply #'+ (mapcar #'gv (hash-table-values obj)))))
         (apply #'+ (mapcar #'gv (hash-table-values obj)))))
    ((typep obj 'list) (apply #'+ (mapcar #'gv obj)))
    ; catch unknown types for debugging
    (t (progn(format t "~&~a ~a" obj (type-of obj))) nil)
    )))

(format t "~&Day 12~%")
(format t "~&Part 1: ~a" (part_a *input*))
(format t "~&Part 2: ~a" (part_b *input*))
(terpri)


