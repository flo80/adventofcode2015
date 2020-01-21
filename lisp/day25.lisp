(defpackage aoc2015-day25
  (:use common-lisp )
  (:export :part_a :part_b)
  )

(in-package :aoc2015-day25)


"Column and row from input"
(defconstant *input* '(3029 2947))
(defconstant starting-value 20151125)
(defconstant multiplier 252533)
(defconstant mod-value 33554393)

(defun sum-up (nr)
  (/ (* nr (+ 1 nr)) 2))

;  ($row + $col - 2) * ($row + $col - 1) / 2 + $col - 1;
(defun pos (col row)
  (+
   (sum-up (+ row col -2))
    col
   )
  )


; https://www.cliki.net/EXPT-MOD
(defun expt-mod (n exponent modulus)
  "As (mod (expt n exponent) modulus), but more efficient."
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  (loop with result = 1
        for i of-type fixnum from 0 below (integer-length exponent)
        for sqr = n then (mod (* sqr sqr) modulus)
        when (logbitp i exponent) do
          (setf result (mod (* result sqr) modulus))
        finally (return result)))


(defun test()
  (loop for y from 1 upto 6
        do (loop for x from 1 upto 6
                 do (format t "~9d " (part_a (list x y))))
           (terpri)))

"  |    1         2         3         4         5         6
---+---------+---------+---------+---------+---------+---------+
 1 | 20151125  18749137  17289845  30943339  10071777  33511524
 2 | 31916031  21629792  16929656   7726640  15514188   4041754
 3 | 16080970   8057251   1601130   7981243  11661866  16474243
 4 | 24592653  32451966  21345942   9380097  10600672  31527494
 5 |    77061  17552253  28094349   6899651   9250759  31663883
 6 | 33071741   6796745  25397450  24659492   1534922  27995004
"

(defun part_a (input)
  (let* ((col (car input))
         (row (cadr input))
         (exp (- (pos col row) 1)))
    (mod
     (*
     starting-value
     (expt-mod multiplier exp mod-value))
     mod-value)
  ))

(format t "~&Day 25~%")
(format t "~&Part 1: ~a" (part_a *input*))
(terpri)
