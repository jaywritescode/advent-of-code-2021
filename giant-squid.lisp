(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "alexandria")
  (ql:quickload "cl-utilities")
  (ql:quickload "str"))

;;; part one

(defun solve-part-one (bingo-balls boards)
  (let ((numbers-called (make-hash-table)))
    ;; define a lexical closure so we don't have to pass numbers-called around
    (flet ((winning-board-p (board)
             (some #'(lambda (line)
                       (every #'(lambda (n)
                                  (gethash n numbers-called)) line))
                   board))
           (all-not-called (board)
             (delete-if #'(lambda (n) (gethash n numbers-called))
                        (remove-duplicates (apply #'append board)))))
      (dolist (i bingo-balls)
        (setf (gethash i numbers-called) t)
        (alexandria:when-let ((winner (find-if #'winning-board-p boards)))
          (return (* i (reduce #'+ (all-not-called winner)))))))))

(defun solve (bingo-balls boards &optional solve-part-one)
  (let ((numbers-called (make-hash-table)))
    (flet ((winning-board-p (board)
             (some #'(lambda (line)
                       (every #'(lambda (number)
                                  (gethash number numbers-called)) line))
                   board))
           (all-not-called (board)
             (delete-if #'(lambda (number) (gethash number numbers-called))
                        (remove-duplicates (apply #'append board)))))
      (dolist (i bingo-balls)
        (format t "~s~%" i)
        (setf (gethash i numbers-called) t)
        (alexandria:when-let ((winner (find-if #'winning-board-p boards)))
          (if (or solve-part-one (every #'winning-board-p boards))
              (return (* i (reduce #'+ (all-not-called winner))))
              (setf boards (remove winner boards))))))))

;;; parse problem statement

(defun parse-board-line (line)
  (mapcar #'parse-integer (str:split-omit-nulls " " line )))

(defun parse-board (lines)
  (let* ((board-lines (mapcar #'parse-board-line lines))
         (board-columns (apply #'mapcar #'list board-lines)))
    (append board-lines board-columns)))

(defun parse-boards (lines)
  (mapcar #'parse-board (cl-utilities:split-sequence-if
                         #'(lambda (x) (zerop (length x))) lines)))

(defun parse-numbers (line)
  (mapcar #'parse-integer (str:split "," line)))

(defun parse-puzzle (lines)
  (values (parse-numbers (first lines)) (parse-boards (cddr lines))))

(defun giant-squid (filename &optional solve-part-one)
  (multiple-value-call #'solve
    (parse-puzzle (uiop:read-file-lines filename)) solve-part-one))
