;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #aoc-asd
  (:use :cl :asdf))

(defsystem aoc
  :name "Advent of Code 2021"
  :serial t
  :depends-on ("str" "alexandria"))
