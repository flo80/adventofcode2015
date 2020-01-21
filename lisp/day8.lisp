(defpackage aoc2015-day8
  (:use common-lisp #:serapeum #"alexandria")
  (:export :part_a :part_b)
  )

(in-package :aoc2015-day8)

(defconstant *input* (alexandria:read-file-into-string "./input/day8"))


(defun decode (input)
  (let ((res nil)
        (temp (coerce input 'list)))
    (loop while (> (length temp) 0)
          do (cond
               ; single "
               ((equal #\" (car temp)) (pop temp))
               ; encoded chars
               ((equal #\\ (car temp))
                (case (second temp)
                  ; encoded \
                  (#\\ (progn (push #\\ res)(pop temp) (pop temp)))
                  ; encoded "
                  (#\" (progn (push #\" res)(pop temp) (pop temp)))
                  ; encoded char
                  (#\x (progn (push #\@ res)(pop temp) (pop temp) (pop temp) (pop temp)))))
               (t (push (pop temp) res))))
    (reverse res)))

(defun encode (input)
  (let ((res '(#\"))
        (temp (coerce input 'list)))

    (loop while (> (length temp) 0)
          do (cond
               ((equal #\" (car temp)) (pop temp) (push #\\ res) (push #\" res))
               ((equal #\\ (car temp)) (pop temp) (push #\\ res) (push #\\ res))
               ((equal #\newline (car temp)) (pop temp) (push #\" res) (push #\newline res) (push #\" res))
               (t (push (pop temp) res))
              )
          )
    (push #\" res)
    (reverse res)))


(defun part_a (input)
  (- (length input) (length (decode input))))

(defun part_b (input)
  (- (length (encode input)) (length input)))



(format t "~&Day 8~%")
(format t "~&Part 1: ~a" (part_a *input*))
(format t "~&Part 2: ~a" (part_b *input*))
(terpri)


