;; misc crap

(defun format-buffer ()
  "Format the whole buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))

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
