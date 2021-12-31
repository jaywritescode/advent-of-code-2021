Common Lisp notes
-----------------

A template for arranging these scripts:

``` common-lisp
;;; load dependencies
(eval-when (:compile-toplevel :load-toplevel :execute)
    (ql:quickload "alexandria")
    (ql:quickload "serapeum")
    ;; etc... each call to ql:quickload can only load one dependency
    )

;;; part one
(defun solve-part-one (puzzle-input)
    "driver function for part one of the problem"
    ;; code here )

;;; part two
(defun solve-part-two (puzzle-input)
    "driver function for part two of the problem"
    ;; code here )

;;; main function + parse input
(defun parse-line (line)
    ;; create domain-specific data structure from line )

(defun puzzle-name (filename &optional solve-part-one)
  (let ((puzzle-input (mapcar #'parse-line (uiop:read-file-lines filename))))
    (if solve-part-one
        (solve-part-one puzzle-input)
        (solve-part-two puzzle-input))))
```

This avoids using `defvar` or `defparameter` in the script itself, and therefore avoids polluting the REPL with a global variable.
