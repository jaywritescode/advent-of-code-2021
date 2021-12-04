(defun sonar-sweep-part-one (input)
  (count t (map 'list #'< input (rest input))))
