(defpackage aoc2015-day9
  (:use common-lisp #:alexandria #:serapeum)
  (:export :part_a :part_b)
  )

(in-package :aoc2015-day9)

(defconstant *input* (alexandria:read-file-into-string "./input/day9"))
(defvar *distances* (make-hash-table :test #'equal))
(defvar *min-dist* 999999999999)
(defvar *max-dist* 0)

(defun parse-input (input)
  (loop for line in (lines input)
        for (a _ b d) = (words line)
        for dist = (parse-integer d)
        do (progn
             (setf (gethash (cons a b) *distances*) dist)
             (setf (gethash (cons b a) *distances*) dist))))


;; from https://www.reddit.com/r/adventofcode/comments/3w192e/day_9_solutions/cxteymh/
(defun crawl (currentnode listofnodes distsum)
  (let ((dist 0))
    (loop for node in listofnodes
          do (progn
               (when currentnode
                 (setq dist (+ distsum (gethash (cons currentnode node) *distances*))))
               (when (eql 1 (length listofnodes))
                 (progn
                   (setf *min-dist* (min dist *min-dist*))
                   (setf *max-dist* (max dist *max-dist*))
                   (return-from crawl 0)))
               (crawl node (remove node listofnodes :test #'string=) dist)))))


(defun calc (input)
  (parse-input input)
  (setf *min-dist* 999999999999999999)
  (setf *max-dist* 0)
  (let ((nodes (serapeum:nub (mapcar #'car (hash-table-keys *distances*)))))
    (crawl nil nodes 0)))


(format t "~&Day 9~%")
(calc *input*)
(format t "~&Part 1: ~a" *min-dist*)
(format t "~&Part 2: ~a" *max-dist*)
(terpri)
