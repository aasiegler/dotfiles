(eval-when-compile
  (require 'cl))

;; misc crap

(defun format-buffer ()
  "Format the whole buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

;; btw this is just revert-buffer
(defun reload-file ()
  (interactive)
  (find-file (buffer-name)))

;; random functions from the internet


;; base from http://www.bretthutley.com/programming/emacs/opening-a-cobjective-cc-header-file-in-emacs/
;; added c mode, fixed code style
(defun bh-choose-header-mode ()
  "Choose the mode for opening a name.h file by searching for a name.cpp or 
   name.m file"
  (interactive)
  (if (string-equal (substring (buffer-file-name) -2) ".h")
      (let ((dot-m-file (concat (substring (buffer-file-name) 0 -1) "m"))
            (dot-cpp-file (concat (substring (buffer-file-name) 0 -1) "cpp")))
        (cond ((file-exists-p dot-cpp-file)
               (c++-mode))
              ((file-exists-p dot-m-file)
               (ojbc-mode))
              (t (c-mode))))))

(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert
the character typed."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t                    (self-insert-command (or arg 1))) ))


(defun paranoid-exit-from-emacs()
  (interactive)
  (if (yes-or-no-p "Do you want to exit? ")
      (save-buffers-kill-emacs)))

(defun smart-split ()
  "Split the frame into 80-column sub-windows, and make sure no window has
   fewer than 80 columns."
  (interactive)
  (defun smart-split-helper (w)
    "Helper function to split a given window into two, the first of which has 
     80 columns."
    (if (> (window-width w) (* 2 81))
        (let ((w2 (split-window w 82 t)))
          (smart-split-helper w2))))
  (smart-split-helper nil))


(defvar g-html-charent-alist
  '(("&lt;" . "<")
    ("&gt;" . ">")
    ("&quot;" . "\"")
    ("&apos;" . "'") 
    ("&amp;" . "&"))
  "Alist of HTML character entities to unescape.")

;; this works on the whole buffer-I want it to work on a region
(defun g-html-unescape-buffer ()
  "Unescape HTML entities."
  (interactive)
  (declare (special g-html-charent-alist))
  (save-excursion
    (loop for entry in g-html-charent-alist
          do
          (let ((entity (car entry))
                (replacement (cdr entry)))
            (goto-char (point-min))
            (while (search-forward entity (point-max) t)
              (replace-match replacement))))))

(defun g-html-escape-buffer ()
  "Unescape HTML entities."
  (interactive)
  (declare (special g-html-charent-alist))
  (save-excursion
    (loop for entry in g-html-charent-alist
          do
          (let ((entity (cdr entry))
                (replacement (car entry)))
            (goto-char (point-min))
            (while (search-forward entity (point-max) t)
              (replace-match replacement))))))

;; this is crap
;; do I have to increase the endpoint every time I do a substitution?
;; I think I do
;; this is dumb... it relies on (& . &amp;) being last int he list
(defun g-html-escape-region ()
  "Unescape HTML entities."
  (interactive)
  (declare (special g-html-charent-alist))
  (save-excursion
    (let ((pt-a (point)))
      (exchange-point-and-mark)
      (let ((pt-b (point)))
        (let ((first-pt (min pt-a pt-b))
              (last-pt (max pt-a pt-b)))
          (goto-char first-pt)
          (loop for entry in (reverse g-html-charent-alist)
                do
                (let ((entity (cdr entry))
                      (replacement (car entry)))
                  (goto-char first-pt)
                  (while (search-forward entity last-pt t)
                    (setq last-pt (+ (- (length (car entry)) 1) last-pt))
                    (replace-match replacement)))))))))

;; frame- or window-resizing function
;; from http://dse.livejournal.com/67732.html. Resizes either frame or window
;; to 80 columns. If the window can be sized to 80 columns wide, without 
;; resizing the frame itself, it will resize the window. Otherwise, it will 
;; resize the frame. You can use a prefix argument to specify a 
;; different column width
(defun fix-frame-horizontal-size (width)
  "Set the frame's size to 80 (or prefix arg WIDTH) columns wide."
  (interactive "P")
  (if window-system
      (set-frame-width (selected-frame) (or width 80))
    (error "Cannot resize frame horizontally: is a text terminal")))

(defun fix-window-horizontal-size (width)
  "Set the window's size to 80 (or prefix arg WIDTH) columns wide."
  (interactive "P")
  (enlarge-window (- (or width 80) (window-width)) 'horizontal))

(defun fix-horizontal-size (width)
  "Set the window's or frame's width to 80 (or prefix arg WIDTH)."
  (interactive "P")
  (condition-case nil
      (fix-window-horizontal-size width)
    (error 
     (condition-case nil
         (fix-frame-horizontal-size width)
       (error
        (error "Cannot resize window or frame horizontally"))))))

(defun toggle-fullscreen (&optional f)
  (interactive)
  (let ((current-value (frame-parameter nil 'fullscreen)))
	(set-frame-parameter nil 'fullscreen
						 (if (equal 'fullboth current-value)
							 (if (boundp 'old-fullscreen) old-fullscreen nil)
						   (progn (setq old-fullscreen current-value)
								  'fullboth)))))


(defun prev-window (count)
  (interactive "p")
  (other-window (- count)))

;; FIXME if buffer has no file it'll assign it "" which may
;; match some patters - this is a bug
(defun kill-buffers-regexp (&optional bname pattern)
  "Kills all buffers matching pattern - all if not supplied. 
Applies pattern to file name, or buffer name if prefix arg
Ignores buffers that like *this*"
  (interactive "P\nsKill Pattern: ")
  (let ((killp (if (or (null pattern) (string= "" pattern))
				   ".*"
				 pattern)))
	(loop for b in (buffer-list)
		  when (let ((name (if bname (buffer-name b) (or (buffer-file-name b) ""))))
				 (and (string-match killp name)
					  (not (string-match "\\*.*\\*" name))))
		  do (kill-buffer b))))

(defun kill-buffers-mode (&optional mode)
  "Kills all buffers with mode m"
  (interactive "sMode Name: ")
  (loop for b in (buffer-list)
		when (string= (concat mode "-mode")
					  (buffer-local-value 'major-mode (get-buffer b)))
		do (kill-buffer b)))


(defun insert-literal-tab ()
  "Inserts a tab"
  (interactive)
  (insert ?\t))

;; TODO add mode map (bash dash etc go to sh-mode)
;; TODO docstring
;; (defun switch-mode-if-shebang ()
;;   (when (eq major-mode 'fundamental-mode)
;; 	(save-excursion
;; 	  (goto-char (point-min))
;; 	  (when (search-forward-regexp "^#!.*?\\([^ /]+\\)$" (point-at-eol) t)
;; 		(let ((mode (match-string-no-properties 1)))
;; 		  (eval (read (concat "(" mode "-mode)"))))))))

(defun switch-mode-if-shebang ()
  (interactive)
  (defun string->mode (s)
	(let ((sym (intern-soft (concat s "-mode"))))
	  (if (fboundp sym) sym nil)))
  (when (eq major-mode 'fundamental-mode)
	(save-excursion
	  (goto-char (point-min))
	  (when (search-forward-regexp "^#!.*?\\([^ /]+\\)$" (point-at-eol) t)
		(funcall (string->mode (match-string 1)))))))

;; for fun, the regexp can also be
;; (rx line-start 
;; 	"#!" (minimal-match (zero-or-more anything))
;; 	(group 
;; 	 (one-or-more (not (any " /"))))
;; 	line-end)

;; now with switch-mode-map
;; I suppose this could be a minor mode (?) I dunno

(defvar switch-mode-map '(("bash" . "sh") ("dash" . "sh") ("zsh" . "sh"))
  "alist of what mode we really want")

(defun switch-mode-tr (line)
  (or (cdr (assoc line switch-mode-map))
	  line))

(defun string->mode (s)
  (let ((sym (intern-soft (concat s "-mode"))))
	(if (fboundp sym) sym nil)))

;; (defun switch-mode-if-shebang ()
;;   (interactive)
;;   (when (eq major-mode 'fundamental-mode)
;; 	(save-excursion
;; 	  (goto-char (point-min))
;; 	  (when (search-forward-regexp "^#!.*?\\([^ /]+\\)$" (point-at-eol) t)
;; 		(funcall (string->mode (switch-mode-tr (match-string 1))))))))

(defun switch-mode-get-interpreter ()
  (save-excursion
	(goto-char (point-min))
	(if (search-forward-regexp "^#!.*?\\([^ /]+\\)$" (point-at-eol) t)
		(match-string 1)
	  nil)))

;; honestly this should probably do nothing if 
(defun switch-mode-if-shebang ()
  (interactive)
  (when (eq major-mode 'fundamental-mode)
	(funcall (or (string->mode (switch-mode-tr (switch-mode-get-interpreter)))
				 (lambda () )))))

(defun apropos-hook (pattern)
  "stupid thing that does apropos-variable with hook in there.
this doesn't really work"
  (interactive "sWork list: ")
  (apropos-variable (concat "hook " pattern) t))


  ;; (interactive (list (apropos-read-pattern
  ;; 		      (if (or current-prefix-arg apropos-do-all)
  ;; 			  "variable" "user option"))
  ;;                    current-prefix-arg))