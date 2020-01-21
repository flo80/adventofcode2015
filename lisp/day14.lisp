(defpackage aoc2015-day14
  (:use common-lisp #:alexandria #:serapeum)
  (:export :part_a :part_b)
  )

(in-package :aoc2015-day14)

(defconstant *input* (alexandria:read-file-into-string "./input/day14"))

(defun distance-traveled (deer total-time)
  (let ((iterations 0)
        (time-left  0)
        (speed        (first  deer))
        (time-running (second deer))
        (time-resting (third  deer)))
    (multiple-value-bind (iterations time-left) (floor total-time (+ time-running time-resting))
      (+
       ; distance for full cycles
       (* iterations (* speed time-running))
       ; distance in leftover time
       (* speed (min time-running time-left))))))

(assert (eq (distance-traveled '(14 10 127) 1000) 1120))
(assert (eq (distance-traveled '(16 11 162) 1000) 1056))


(defun parse-input (input)
  (let ((lines (serapeum:lines input)))
    (mapcan
     (lambda (line )
       (list
        (mapcar #'parse-integer
                (remove-if-not
                 (lambda (x) (every #'digit-char-p x))
                 (serapeum:words line)))))
     lines)))

(defun part_a (input)
  (let ((reindeers (parse-input input)))
    (apply #'max
           (mapcar
            (lambda (deer) (distance-traveled deer 2503)) reindeers))))

(defun part_b (input)
  (let* ((reindeers (parse-input input))
         (results (make-array (length reindeers) :element-type 'integer)))
    (loop for time from 1 upto 2503
          do (let* ((step-results (mapcar (lambda (deer) (distance-traveled deer time)) reindeers))
                    (max (apply #'max step-results))
                    (winner (mapcar (lambda (x) (eq x max)) step-results)))
               (loop for d from 0 to (length reindeers)
                     do (if (pop winner) (incf (aref results d))))))
    (apply #'max (coerce results 'list))))


(format t "~&Day 14~%")
(format t "~&Part 1: ~a" (part_a *input*))
(format t "~&Part 2: ~a" (part_b *input*))
(terpri)
