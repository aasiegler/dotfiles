;; this is from steve yegge's blog somewhere

(defun fix-amazon-url ()
  "Minimizes the Amazon URL under the point.  You can paste an Amazon
URL out of your browser, put the cursor in it somewhere, and invoke
this method to convert it."
  (interactive)
  (and (search-backward "http://www.amazon.com" (point-at-bol) t)
       (search-forward-regexp
		".+/\\([A-Z0-9]\\{10\\}\\)/[^[:space:]\"]+" (point-at-eol) t)
       (replace-match
		(concat "http://www.amazon.com/o/asin/"
				(match-string 1)
				(match-string 3)))))

;; wait this is an example of something that does not work well
;; er how does this work 






;; this version apparently works
;; (defun fix-amazon-url ()
;;   "Minimizes the Amazon URL under the point.  You can
;; paste an Amazon URL out of your browser, put the cursor
;; in it somewhere, and invoke this method to convert it."
;;   (interactive)
;;   (let ((case-fold-search t))
;;     (or
;;      (let (url start end asin)
;;        (and
;; 		;; search-backward-regexp acts a bit oddly, so split the search
;; 		(setq
;; 		 start
;; 		 (search-backward-regexp "http://www.amazon.com" (point-at-bol) t))

;; 		(> (skip-syntax-forward "^\\s-" (point-at-eol)) 0)
;; 		(setq end
;; 			  (if (eq (char-before (point)) ?\")
;; 				  (1- (point))
;; 				(point)))

;; 		(setq url (buffer-substring-no-properties start end))

;; 		(string-match "/\\([A-Z0-9]\\{10\\}\\)/" url)
;; 		(setq asin (match-string 1 url))

;; 		(progn
;; 		  (delete-region start end)
;; 		  (backward-char 1)
;; 		  (insert (concat "http://www.amazon.com/o/asin/" asin))
;; 		  (if (string-match "\"$" url) (insert "\""))
;; 		  t)))
;;      (message "Sorry, didn't find an Amazon URL under the point."))))
