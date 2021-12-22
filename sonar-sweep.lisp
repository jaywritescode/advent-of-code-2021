;;; part one
(defun solve-part-one (input)
  (count t (map 'list #'< input (rest input))))

;;; part two
(defun triples (seq)
  (map 'list #'list seq (cdr seq) (cddr seq)))

(defun solve-part-two (input)
  (let ((window-sums (mapcar
                      (alexandria:curry #'reduce #'+)
                      (triples input))))
    (count t (map 'list #'< window-sums (rest window-sums)))))

;;; read and parse problem input

(defun parse-line (line) (parse-integer line))

(defun sonar-sweep (filename &optional solve-part-one)
  (let ((puzzle-input (mapcar #'parse-line (uiop:read-file-lines filename))))
    (if solve-part-one
        (solve-part-one puzzle-input)
        (solve-part-two puzzle-input))))
