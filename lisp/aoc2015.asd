;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:aoc2015
  (:use :cl :asdf :ql))

(in-package :aoc2015)

(defsystem aoc2015
  :name "Advent of Code 2015"
  :version "0.0.0"
  :author "Florian Lloyd-Pötscher"

  :components ((:file "common")
               (:file "day10")
               (:file "day11")
               (:file "day12"))
  )
