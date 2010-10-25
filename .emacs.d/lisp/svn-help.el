;; oh yeah -- remove-if and remove-if-not
;; (defun filter (fn seq)
;;   "there is probably a function that does this already"
;;   (let ((res nil))
;; 	(dolist (elt seq)
;; 	  (when (funcall fn elt)
;; 		(setf res (cons elt res))))
;; 	(nreverse res)))

(defun string-join (separator seq)
  (mapconcat 'identity seq separator))

(defun get-line ()
  "Returns the contents of the current line"
  (save-excursion
	(move-beginning-of-line nil)
	(let ((beg (point)))
	  (move-end-of-line nil)
	  (buffer-substring beg (point)))))

(defun lines-to-list ()
  "Takes all lines in the buffer and returns them as a list"
  (save-excursion
	(let ((lines nil))
	  (goto-char (point-min))
	  (while (not (= (point) (point-max)))
		(setf lines (cons (get-line) lines))
		(forward-line))
	  (nreverse lines))))

(defun svn-commit-file-get-files ()
  (interactive)
  (save-excursion
	(goto-char (point-max))
	(newline 2)
	(insert
	 (string-join " "
				  (remove-if-not
				   #'(lambda (elt) (and (> (length elt) 0)
										(not (equalp " " (substring elt 0 1)))))
				   (lines-to-list))))))

