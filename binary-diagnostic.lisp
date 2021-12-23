;; part one
(defun num-bits (puzzle-input)
  (length (format nil "~B" (reduce #'max puzzle-input))))

(defun get-nth-bit (n i)
  "Gets the nth bit of non-negative integer i."
  (if (zerop (logand i (ash 1 n))) 0 1))

(defun solve-part-one (puzzle-input)
  (* (gamma-rate puzzle-input) (epsilon-rate puzzle-input)))

(defun gamma-rate (puzzle-input)
  (let ((result 0))
    (dotimes (i (num-bits puzzle-input) result)
      (when (plusp (gamma-rate-nth-bit puzzle-input i))
        (incf result (ash 1 i))))))

(defun gamma-rate-nth-bit (puzzle-input n)
  (let ((running-total 0))
    (dolist (number puzzle-input (if (plusp running-total) 1 0))
      (if (plusp (get-nth-bit n number))
          (1+ running-total)
          (1- running-total)))))

(defun epsilon-rate (puzzle-input)
  (let ((result 0))
    (dotimes (i (num-bits puzzle-input) result)
      (when (plusp (epsilon-rate-nth-bit puzzle-input i))
        (incf result (ash 1 i))))))

(defun parse-line (line) (read-from-string (str:concat "#B" line)))


(defun binary-diagnostic (filename)
  (let ((puzzle-input (mapcar #'parse-line (uiop:read-file-lines filename))))
    (solve-part-one puzzle-input)))
