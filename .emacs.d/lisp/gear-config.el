;;; does something good

(require 'assoc)

(defvar gear-config-names '("MC" "RE" "RG"))
(defvar gear-config-files '())
(defvar gear-config-keys '("host" "port" "database"))
(defvar gear-config-root nil)

;; (defun run ()
;;   (interactive)
;;   (save-excursion
;; 	(make-alists (gear-config-files))


(defun gear-conf-make-alists (files)
  "make an alist for every file, returns an alist (file . alist)"
  (cond ((null files) nil)
		(t (acons (first files) (file->alist (first files))
				  (make-alists (rest files))))))

;; this doesn't work: it leaves us back in a crappy state
(defun gear-conf-debug-log (message)
  (message (concat "gear-conf-" message)))
;;   (let ((bname "*dev-debug*")
;; 		(prev (buffer-name)))
;; 	(switch-to-buffer-other-window bname)
;; 	(switch-to-buffer bname)
;; 	(goto-char (point-max))
;; 	(insert message)
;; 	(insert "
;; ")
;; 	(previous-multiframe-window)))


;;	(switch-to-buffer prev)))

;; (defun gear-conf-match-string (n)
;;   (buffer-substring (match-beginning n) (match-end n)))

;; this almost works? print right thing, returns wrong thing
;; works unless you uncomment debug-log
(defun gear-conf-file->alist (file)
  "make a alist for one file - takes all the <add key=x value=y /> and such"
  (find-file file)
  (goto-char (point-min))
  (let ((pairs '()))
	(while (re-search-forward "<add\s+key=\"\\(.*?\\)\"\s+value=\"\\(.*?\\)\""
							  (point-max) t)
	  (let ((key (match-string-no-properties 1))
			(val (match-string-no-properties 2)))
		;; (gear-conf-debug-log (concat key "= " val))
		(setf pairs (acons key val pairs))))
	(reverse pairs)))




	  ;; (gear-conf-debug-log (match-string 1))
	  ;; (gear-conf-debug-log (match-string 2))

;; (defun check-key (key))

;; (defun get-values (key))

;; (defun set-value (key val))

;; (defun get-files ())


