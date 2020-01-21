(defpackage aoc2015-day23
  (:use common-lisp #:alexandria #:serapeum)
  (:export :part_a :part_b)
  )

(in-package :aoc2015-day23)

(defconstant *input* (alexandria:read-file-into-string "./input/day23"))

(defun parse-input (input)
  (let* ((lines (lines input))
         (len (length lines))
         (program (make-array len :element-type 'string :initial-contents lines)))
    program
    ))


"Instructions
hlf r sets register r to half its current value, then continues with the next instruction.
tpl r sets register r to triple its current value, then continues with the next instruction.
inc r increments register r, adding 1 to it, then continues with the next instruction.
jmp offset is a jump; it continues with the instruction offset away relative to itself.
jie r, offset is like jmp, but only jumps if register r is even (jump if even).
jio r, offset is like jmp, but only jumps if register r is 1 (jump if one, not odd).
"

(defconst *regnames* '("a" "b"))
(defvar *instructions* (make-hash-table :test #'equal))
(defun create-instructions ()
  (setf (gethash "hlf" *instructions*) (op-hlf))
  (setf (gethash "tpl" *instructions*) (op-tpl))
  (setf (gethash "inc" *instructions*) (op-inc))
  (setf (gethash "jmp" *instructions*) (op-jmp))
  (setf (gethash "jie" *instructions*) (op-jie))
  (setf (gethash "jio" *instructions*) (op-jio))
  )


(defun get-jmp (param)
  (parse-integer (car (last param))))

(defun get-reg-nr (param)
  (position (car param) *regnames* :test #'equal))

(defun op-hlf ()
  (lambda (param registers ip)
    (let ((reg (get-reg-nr param)))
      (setf (aref registers reg) (floor (aref registers reg) 2))
      (+ 1 ip))))

(defun op-tpl ()
  (lambda (param registers ip)
    (let ((reg (get-reg-nr param)))
      (setf (aref registers reg) (* (aref registers reg) 3))
      (+ 1 ip))))

(defun op-inc ()
  (lambda (param registers ip)
    (let ((reg (get-reg-nr param)))
      (incf (aref registers reg))
      (+ 1 ip))))

(defun op-jmp ()
  (lambda (param registers ip)
    (+ (get-jmp param) ip)))

(defun op-jie ()
  (lambda (param registers ip)
    (let ((rval (aref registers (position (car param) *regnames* :test #'equal)))
          (dest (get-jmp param)))
      (+ ip
         (if (evenp rval) dest 1)))))

(defun op-jio ()
  (lambda (param registers ip)
    (let ((rval (aref registers (position (car param) *regnames* :test #'equal)))
          (dest (get-jmp param)))
      (+ ip
         (if (eq 1 rval) dest 1)))))


(defun process-program (program &key (initial-registers '(0 0)))
  (create-instructions)
  (loop
    with registers = (make-array 2 :element-type 'integer :initial-contents initial-registers)
    for ip = 0 then (funcall op param registers ip)
    until (>= ip (length program))
    for opcode = (serapeum:split-sequence-if (lambda (c) (or (eq c #\space) (eq c #\,))) (aref program ip))
    for instr  = (car opcode)
    for param  = (cdr opcode)
    for op     = (gethash instr *instructions*)
    ;do (format t "~&~a \"~a\": '~a' ~a ~a " ip opcode instr param registers)
    finally (return registers)
    ))


(defun part_a(input)
  (aref (process-program (parse-input input)) 1))

(defun part_b(input)
  (aref (process-program (parse-input input) :initial-registers '(1 0)) 1))


(format t "~&Day 23~%")
(format t "~&Part 1: ~a" (part_a *input*))
(format t "~&Part 2: ~a" (part_b *input*))
(terpri)
