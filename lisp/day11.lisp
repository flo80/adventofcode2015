(defpackage aoc2015-day11
  (:use common-lisp #:serapeum)
  (:export :part_a :part_b)
  )

(in-package :aoc2015-day11)

(defconstant *input* "cqjxjnds")

(defun next-codepoint (char) (code-char (1+ (char-code char))))


(defun next-pw (password)
  (let ((done nil))
    (coerce
     (reverse
      (loop for c across (reverse password)
            collect (if (not done)
                        (cond
                          ((eq c #\z) #\a)
                          (t (progn
                               (setf done t)
                               (next-codepoint c)
                               )))
                        c  ))) 'string )))

(assert (equal "xy" (next-pw "xx")))
(assert (equal "ya" (next-pw "xz")))

(defun is-valid-pw-p (password)
  (and (has-straight-p password)
       (valid-chars-p password)
       (min-two-pairs-p password)))

(defun has-straight-p (password)
  "Passwords must include one increasing straight of at least three letters, like abc, bcd, cde, and so on, up to xyz. They cannot skip letters; abd doesn't count."
  (let ((curr nil)
        (longest 0))
    (loop for c across password
          do (cond 
               ;; nothing in curr
               ((null curr) (push c curr))
               ;; increasing sequence
               ((eq (char-code c) (+ 1(char-code (car curr)))) (push c curr))
               ;; sequence not increasing
               (t (progn
                    (setf longest
                          (max longest (length curr)))
                    (setf curr nil)
                    (push c curr)))
               )
          )
    ;; if loop ended compare last saved curr
    (setf longest
          (max longest (length curr)))
    (>= longest 3)))

(defun valid-chars-p (password)
  "Passwords may not contain the letters i, o, or l, as these letters can be mistaken for other characters and are therefore confusing."
  (not (or (find #\i password)
      (find #\l password)
      (find #\o password))))

(defun min-two-pairs-p (password)
  "Passwords must contain at least two different, non-overlapping pairs of letters, like aa, bb, or zz."
  (>=
   (length
    (remove-if-not
     (lambda (x) (>= x 2))
     (mapcar #'length
             (serapeum:nub(serapeum:runs password)))))
   2))

(defun iterate (input)
  (let ((pw (next-pw input)))
    (loop while (not (is-valid-pw-p pw))
          do (setf pw (next-pw pw)))
    pw))


(assert (equal (iterate "abcdefgh") "abcdffaa"))
(assert (equal (iterate "ghijklmn") "ghjaabcc"))

(defun part_a (input)
  (format t "~&Part 1: ~a" (iterate *input*)))


(defun part_b (input)
  (format t "~&Part 2: ~a" (iterate(iterate *input*))))

(format t "~&Day 11~%")
(part_a *input*)
(part_b *input*)
