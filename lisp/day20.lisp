(defpackage aoc2015-day20
  (:use common-lisp )
  (:export :part_a :part_b)
  )

(in-package :aoc2015-day20)

(defconstant *input* 36000000)


;; factor solution is very slow
(defun factors (n &aux (lows '()) (highs '()))
  (do ((limit (1+ (isqrt n))) (factor 1 (1+ factor)))
      ((= factor limit)
       (when (= n (* limit limit))
         (push limit highs))
       (remove-duplicates (nreconc lows highs)))
    (multiple-value-bind (quotient remainder) (floor n factor)
      (when (zerop remainder)
        (push factor lows)
        (push quotient highs)))))


(defun part_a-factor (input)
  (let ((curr 0)
        (i 0))
    (loop while (< curr input)
          do (progn
               (incf i)
               (setf curr (* 10 (apply #'+ (factors i))))))
    i))


(defun part_b-factor (input)
  (let ((curr 0)
        (i 0))
    (loop while (< curr input)
          do (progn
               (incf i)
               (setf curr (* 11 (apply #'+ (remove-if (lambda (x) (< x (/ i 50))) (factors i)))))))
    i))


(defun part_a (input)
  (let ((houses (make-array (+ 1 (floor input 10)))))
    (loop for i from 1 upto (floor input 10)
          do (loop for j from i upto (floor input 10) by i
                   do (setf (aref houses j) (+ (aref houses j) (* 10 i)))))
    (loop for i from  1 upto (floor input 10)
          when (>= (aref houses i) input) return i)))


(defun part_b (input)
  (let ((houses (make-array (+ 1 (floor input 10)))))
    (loop for i from 1 upto (floor input 10)
          do (loop for j from i upto (min (* 50 i) (floor input 10)) by i
                   do (setf (aref houses j) (+ (aref houses j) (* 11 i)))))
    (loop for i from  1 upto (floor input 10)
          when (>= (aref houses i) input) return i)))


(format t "~&Day 20~%")
(format t "~&Part 1: ~a" (part_a *input*))
(format t "~&Part 2: ~a" (part_b *input*))
(terpri)


