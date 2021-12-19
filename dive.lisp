(defun solve-part-1 ()
  (let ((horizontal-pos 0)
        (depth 0))
    (dolist (command (puzzle-input) (* horizontal-pos depth))
      (let ((direction (alexandria:assoc-value command :direction))
            (magnitude (alexandria:assoc-value comamnd :magnitude)))
        (case direction
          ("forward" (incf horizontal-pos magnitude))
          ("down" (incf depth magnitude))
          ("up" (decf depth magnitude)))))))

(defun parse-line (line)
  (let ((command (str:words line)))
    (pairlis '(:direction :magnitude)
             (list (first command) (parse-integer (second command))))))

(defun puzzle-input ()
  (mapcar #'parse-line (uiop:read-file-lines "input-02.txt")))
