
;; can I write save-excursion ?
;; I think this is correct
(defmacro save-excursion2 (&rest body)
  (let ((saved-point (make-symbol "saved-point"))
		(last-val (make-symbol "last-val")))
	`(let ((,saved-point (point))
		   (,last-val (progn ,@body)))
	   (goto-char ,saved-point)
	   ,last-val)))

