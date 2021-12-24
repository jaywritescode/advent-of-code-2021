;;; binary arithmetic
(defun num-bits (puzzle-input)
  (length (format nil "~B" (reduce #'max puzzle-input))))

(defun get-nth-bit (n i)
  "Gets the nth bit of non-negative integer i."
  (if (zerop (logand i (ash 1 n))) 0 1))

;;; part one
(defun solve-part-one (puzzle-input)
  (let* ((gamma-rate (gamma-rate puzzle-input))
         (epsilon-rate (logxor gamma-rate
                               (1- (ash 1 (num-bits puzzle-input))))))
    (* gamma-rate epsilon-rate)))

(defun gamma-rate-nth-bit (puzzle-input n)
  (let ((ones-count
          (count 1 (mapcar #'(lambda (i) (get-nth-bit n i)) puzzle-input))))
    (if (> ones-count (/ (length puzzle-input) 2)) 1 0)))

(defun gamma-rate (puzzle-input)
  (let ((result 0))
    (dotimes (i (num-bits puzzle-input) result)
      (incf result (ash (gamma-rate-nth-bit puzzle-input i) i)))))

;;; parse problem statement
(defun parse-line (line) (read-from-string (str:concat "#B" line)))

(defun binary-diagnostic (filename)
  (let ((puzzle-input (mapcar #'parse-line (uiop:read-file-lines filename))))
    (solve-part-one puzzle-input)))
