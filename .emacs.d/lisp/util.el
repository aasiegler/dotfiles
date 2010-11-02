;; helper functions for more lisp


(defun stringi= (s1 s2)
  "String insensitive compare."
  (eq t (compare-strings s1 0 (length s1)
						 s2 0 (length s2)
						 t)))

