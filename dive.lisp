(defun solve-part-one (puzzle-input)
  (let ((horizontal-pos 0)
        (depth 0))
    (dolist (command puzzle-input (* horizontal-pos depth))
      (let ((magnitude (alexandria:assoc-value command :magnitude)))
        (case (alexandria:assoc-value command :direction)
          (|forward| (incf horizontal-pos magnitude))
          (|up| (decf depth magnitude))
          (|down| (incf depth magnitude)))))))

(defun solve-part-two (puzzle-input)
  (let ((horizontal-pos 0)
        (depth 0)
        (aim 0))
    (dolist (command puzzle-input (* horizontal-pos depth))
      (let ((magnitude (alexandria:assoc-value command :magnitude)))
        (case (alexandria:assoc-value command :direction)
          (|forward| (setf horizontal-pos (+ horizontal-pos magnitude)
                           depth (+ depth (* aim magnitude))))
          (|up| (decf aim magnitude))
          (|down| (incf aim magnitude)))))))

(defun parse-line (line)
  (let ((command (str:words line)))
    (pairlis '(:direction :magnitude)
             (list (intern (first command)) (parse-integer (second command))))))

(defun dive (filename &optional solve-part-one)
  (let ((puzzle-input (mapcar #'parse-line (uiop:read-file-lines filename))))
    (if solve-part-one
        (solve-part-one puzzle-input)
        (solve-part-two puzzle-input))))
