;;; font-select.el

;; Copyright (C) 2010 Aaron Siegler

;; Author: Aaron Siegler <aasiegler@gmail.com>
(eval-when-compile
  (require 'cl))

(defmacro return-if-errors (&rest body)
  "Like ignore-errors except returns  on success, nil on error"
  `(condition-case ex
	   (or (progn ,@body) t)
	 ('error nil)))

(defmacro set-font (&rest font)
  "Sets the first font in the parameter list that exists"
  `(or
	,@(loop for f in font
			collecting `(return-if-errors (set-frame-font ,f) ,f))))

