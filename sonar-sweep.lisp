;;; part one
(defun sonar-sweep (input)
  (count t (map 'list #'< input (rest input))))

;;; part two
(defun triples (seq)
  (map 'list #'list seq (cdr seq) (cddr seq)))

(defun sonar-sweep-2 (input)
  ;; Is there a simpler way to write this mapcar?
  (let ((window-sums (mapcar #'(lambda (tuple) (reduce #'+ tuple))
                             (triples problem-input))))
    (count t (map 'list #'< window-sums (rest window-sums)))))

;;; good to remember
(defvar problem-input
  (mapcar #'parse-integer (uiop:read-file-lines "input-01.txt")))
