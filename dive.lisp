(ql:quickload "str")

(defun solve-part-1 ()
  (let ((horizontal-pos 0)
        (depth 0))
    (dolist (command (puzzle-input) (* horizontal-pos depth))
      (break)
      (multiple-value-bind (direction amount) command
        (case direction
          ("forward" (incf horizontal-pos amount))
          ("down" (incf depth amount))
          ("up" (decf depth amount)))))))

(defun parse-line (line)
  (let ((command (str:words line)))
    (pairlis (:direction :magnitude)
             ((first command) (parse-integer (second command))))))

(defun puzzle-input ()
  (mapcar #'parse-line (uiop:read-file-lines "input-02.txt")))
