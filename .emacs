;;; -*- lexical-binding: t -*-

;; TODO byte compile site-lisp directory

(require 'cl)

;; ";; -*- lexical-binding: t -*-"

(setf initial-scratch-message ";; You're a peach!

")

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/.emacs.d/elpa/package.el"))
  (package-initialize))


;;;------------------------------------------------------
;;; basic options
;;;------------------------------------------------------
(defvar mswindows-p 
  (not (null (string-match "windows" (symbol-name system-type)))))
(defvar cygwin-p
  (not (null (string-match "cygwin" (symbol-name system-type)))))
(defvar running-in-terminal-p
  (null window-system))

(defvar home (concat (expand-file-name "~") "/"))
(defvar emacs-home (concat home ".emacs.d/"))

;; (setq debug-on-error t)

(setq inhibit-splash-screen t)
(if (boundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
(menu-bar-mode -1)
(tool-bar-mode -1)

(column-number-mode t)

;; set the screen size to the whole thing if it's lappy
;; or just normal if not
;; (if (and (= 600 (display-pixel-height))
;; 		 (= 1024 (display-pixel-width)))	 
;; 	(setq default-frame-alist '((width . 126) (height . 36)))
;;   (setq default-frame-alist '((width . 80) (height . 60))))


(setq default-tab-width 4)
(transient-mark-mode t)
(show-paren-mode t)
(global-font-lock-mode t)

(fset 'yes-or-no-p 'y-or-n-p)
(defalias 'll-mode 'longlines-mode)
(iswitchb-mode 1)
(setq confirm-nonexistent-file-or-buffer nil)
(setq iswitchb-prompt-newbuffer nil)

(setq frame-title-format (concat  "%b - emacs@" system-name))

(when mswindows-p
  (setq python-python-command "C:/Python/Python27/python.exe"))

;; don't have to set as long as it's in $PATH
;; (setq ipython-command "/usr/bin/ipython")
;; note: start with C-c !
;; NOTE: do sys.path.append('/path/to/place') so that you can import modules

(unless (get-buffer "*eshell*")
  (eshell))

;; I want to do something so this works
;; probably like after-eval 
;; I do something similar later but it doesn't work
;; (setf eshell-path-env (concat eshell-path-env
;; 							  ";"
;; 							  "C:/Cygwin/bin"))
;; (setf eshell-path-env (concat eshell-path-env ";" "C:/Program Files (x86)/Mozilla Firefox/"))
;; (labels ((add-path (p)
;; 				   (setenv "PATH" (concat p ";" (getenv "PATH")))))
;;   (add-path "C:/Cygwin/bin")
;;   (add-path "C:/Util/gnutls-2.10.1/bin"))

(setf browse-url-firefox-program "C:/Program Files (x86)/Mozilla Firefox/firefox.exe")
(setf ispell-program-name "C:/Program Files (x86)/Aspell/bin/aspell.exe")

;; setting the PC keyboard's various keys to Super or Hyper
(setq w32-pass-lwindow-to-system nil
      w32-pass-rwindow-to-system nil
      w32-pass-apps-to-system nil
      w32-lwindow-modifier 'super ;; Left Windows key
      w32-rwindow-modifier 'super ;; Right Windows key
      w32-apps-modifier 'hyper) ;; Menu key


;;;------------------------------------------------------
;;; set-keys
;;;------------------------------------------------------

;; keybindings for anything 
;; (global-set-key [(super a)] 'anything)

(global-set-key (kbd "s-a") 'anything)
(global-set-key (kbd "s-x") 'anything-M-x)
(global-set-key (kbd "C-c b") 'anything)
(global-set-key (kbd "C-c C-m") 'anything)

(global-set-key (kbd "C-c k") 'kill-buffers-regexp)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "C-c u") 'undo-close-undo)

(global-set-key (kbd "C-c ?") 'ispell-word)

(global-set-key (kbd "C-c d d") #'base64-decode-region)
(global-set-key (kbd "C-c d e") #'base64-encode-region)


;; (global-set-key "\C-cl" 'org-store-link)
;; (global-set-key "\C-ca" 'org-agenda)
;; (global-set-key "\C-cb" 'org-iswitchb)

(global-set-key "\M-gg" 'goto-line)
(global-set-key "\C-x\C-m" 'execute-extended-command)
;; (global-set-key "\M-n" 'scroll-down-keep-cursor)
;; (global-set-key "\M-p" 'scroll-up-keep-cursor)

;; (global-set-key "%" 'goto-match-paren)

;; F4 is end-kbd-macro (or call last) in emacs22+
;; (global-set-key '[f4] 'speedbar-get-focus)

(global-set-key "\C-c\C-h" 'ff-find-other-file)

(global-set-key (kbd "C-x W") 'fix-horizontal-size)

(global-set-key "\M-n" 'other-window)
(global-set-key "\M-p" 'prev-window)

(global-set-key "\C-x\M-k" 'kill-buffers-regexp)
(global-set-key "\C-x\C-kr" 'kill-buffers-regexp)
(global-set-key "\C-x\C-km" 'kill-buffers-mode)

(global-set-key (kbd "<right>") 'windmove-right)
(global-set-key (kbd "<left>") 'windmove-left)
(global-set-key (kbd "<up>") 'windmove-up)
(global-set-key (kbd "<down>") 'windmove-down)

;; (global-unset-key (kbd "<left>"))
;; (global-unset-key (kbd "<right>"))
;; (global-unset-key (kbd "<up>"))
;; (global-unset-key (kbd "<down>"))

(global-set-key "\C-c\C-t" 'insert-literal-tab)

(global-set-key "\C-x\C-c" 'paranoid-exit-from-emacs)

(global-set-key '[f11] 'w32-fullscreen)

(global-set-key (kbd "C-c o") 'open-line-and-indent)



;;;------------------------------------------------------
;;; load/autoloading .el files
;;;------------------------------------------------------
;; (setq load-path (append (mapcar
;; 						 (lambda (p) (concat emacs-home p))
;; 						 '("lisp/"
;; 						   "site-lisp"
;; 						   "site-lisp/color-theme-6.6.0"
;; 						   "site-lisp/elib-1.0"
;; 						   "site-lisp/jdee-2.4.0.1/lisp"
;; 						   "site-lisp/slime"
;; 						   "site-lisp/slime/contrib"
;; 						   "site-lisp/tnt-2.6"
;; 						   "site-lisp/w3m"
;; 						   "site-lisp/darkroom"))
;; 						load-path))
(mapcar (lambda (p) (add-to-list 'load-path (concat emacs-home p)))
		'("dexrex"
		  "lisp/"
		  "site-lisp"
		  "site-lisp/color-theme-6.6.0"
		  "site-lisp/csharp"
		  "site-lisp/elib-1.0"
		  "site-lisp/jdee-2.4.0.1/lisp"
		  "site-lisp/slime"
		  "site-lisp/slime/contrib"
		  "site-lisp/tnt-2.6"
		  "site-lisp/w3m"
		  "site-lisp/darkroom"))

(unless mswindows-p
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/w3m"))

(load-library "util")
(load-library "misc")
(load-library "font-select")

(require 'ansi)
(require 'anything)
(require 'anything-config)
(require 'anything-match-plugin)
(require 'color-theme)
(require 'cltl2)
(require 'csharp-completion)
(require 'dx-template-mode)
(require 'fic-mode)
(require 'g-utils)
(require 'python-mode)
;; FIXME can't get this to work on windows
;;(require 'ipython)
(require 'rainbow-parens)
(require 'scratch)
(require 'slime)
(require 'slime-autoloads)
(require 'tnt)
(require 'uniquify)
(require 'w32-fullscreen)
(require 'w3m-load)
;; (require 'ess-site)



(autoload 'csharp-mode "csharp-mode.el" "Major mode for editing C# code." t)
(autoload 'erlang-mode "erlang.el")
(autoload 'flymake-php "flymake-php.el" "minor mode for catching php errors" t)
(autoload 'forth-mode "gforth.el")
(autoload 'forth-block-mode "gforth.el")
(autoload 'html-php-mode "html-php.el" "multi-mode for php and html" t)
(autoload 'intercal-mode "intercal.el" "Load intercal-mode" t)
(autoload 'jde-mode "jde.el" "Major mode for editing java" t)
(autoload 'js2-mode "js2.elc" "Major mode for editing javascrip" t) 
(autoload 'multi-mode "multi-mode.el" "multi-mode support" t)
(autoload 'nsi-mode "nsi-mode" "nsi editing mode." t)
(autoload 'php-mode "php-mode.el" "php mode" t)

(setq auto-mode-alist
	  (append '(("\\.js$" . js2-mode)
				("\\.cs$" . csharp-mode)
				("\\.fs$" . forth-mode)
				("\\.erl$" . erlang-mode)
				("\\.php$" . php-mode)
				("\\.i$" . intercal-mode)
				("\\.nsi$" . nsi-mode)
				("\\.java$" . java-mode)) ;; jde-mode but that don't work right now
			  auto-mode-alist))
;; ("\\.py$" . python-mode)


(set-font "consolas 10"
		  "inconsolata 11"
		  "Mono 10")
(color-theme-initialize)
(if running-in-terminal-p
	(progn
	  (color-theme-arjen)
	  (color-theme-arjen))
  (color-theme-tangotango))
;; arjen clarity sitaramv-solaris tangotango

(if mswindows-p
	(setq inferior-lisp-program "c:/Program Files (x86)/Steel Bank Common Lisp/1.0.37/sbcl.exe --noinform")
  (setq inferior-lisp-program "sbcl --noinform"))
;; or just slime-fancy I think
(slime-setup '(slime-repl slime-editing-commands))

;; cywgin stuff if windows (ahhh cygwin is terrible)
;; (when mswindows-p
;;   (progn
;; 	(setenv "PATH" (concat "c:/cygwin/bin;" (getenv "PATH")))
;; 	(setq exec-path (cons "c:/cygwin/bin/" exec-path))
;; 	(require 'cygwin-mount)
;; 	(cygwin-mount-activate)))

(setq org-agenda-files (list "~/org" "~/My Dropbox/org"))

;; remove system32 from exec path because fuck C:/Windows/system32/find.exe
(setf exec-path (remove-if (lambda (x) (stringi= "c:/windows/system32" x))
						   exec-path))

;; erc stuff 
;; TODO put this in another file (.emacs.d/lisp/erc-setup.el)
(erc-track-mode t)
(setf erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
								"324" "329" "332" "333" "353" "477"))
(setf erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))
(setf erc-log-channels-directory "~/.erc/logs/")
(setf erc-save-buffer-on-part t)

(require 'erc-join)
(require 'erc-truncate)
(erc-autojoin-mode 1)

(load-library "erc-setup")
;; this truncates the buffer down to 0 lines
;; (setf erc-truncate-buffer-on-save nil) 



;;;------------------------------------------------------
;;; shell (for jde)
;;;------------------------------------------------------
;; (setq shell-file-name "bash")
;; (setq shell-command-switch "-c")
;; (setq explicit-shell-file-name shell-file-name)
;; (setenv "SHELL" shell-file-name)
;; (setq explicit-sh-args '("-login" "-i"))
;; (if (boundp 'w32-quote-process-args)
;;   (setq w32-quote-process-args ?\")) ;; Include only for MS Windows.


;;;------------------------------------------------------
;;; offsets and stuff
;;;------------------------------------------------------
;; yo so to find what part you are in it's c-set-offset (C-c C-o)
;; so we shouldn't use add-hook
;; if we re-evaluate this buffer it'll re-add the hooks
;; we could do add-or-replace-hook and have defun'ed hooks instead of lambdas

;; elisp
(add-hook 'emacs-lisp-mode-hook 'turn-on-fic-mode)

;; c-mode stuff
(add-hook 'c-mode-common-hook 
		  (lambda ()
			(setq tab-width 4)
			(setq c-basic-offset 4)
			(c-set-offset 'substatement-open 0)
			(c-toggle-hungry-state t)))

(add-hook 'c-mode-common-hook 'turn-on-fic-mode)

;; c++-mode stuff
(add-hook 'c++-mode-hook
		  (lambda ()
			(setq tab-width 4)
			(setq c-basic-offset 4)))

(add-hook 'python-mode-hook
		  (lambda ()
			(setq indent-tabs-mode nil)
			(setq tab-width 4)))

;; php-mode stuff
;; making indents and tabs the same as everyone else
(add-hook 'php-mode-hook
		  (lambda ()
			(setq tab-width 4)
			(setq c-basic-offset 4)
			(c-set-offset 'topmost-intro-cont 4)
			(c-set-offset 'class-open 0)
			(c-set-offset 'inline-open 0)
			(c-set-offset 'substatement-open 0)
			(c-set-offset 'arglist-intro '+)))

;; sql-mode
(add-hook 'sql-mode-hook
		  (lambda ()
			;; tabs are spaces
			(setq indent-tabs-mode nil)))

;; java-mode
(add-hook 'java-mode-hook
		  (lambda ()
			;; basically for switch statement
			(c-set-offset 'substatement-open 0)
			(c-toggle-hungry-state t)))

;; this is all kind of shitty
(add-hook 'jde-mode-hook
		  (lambda ()
			(defun jde-debug-or-cont ()
			  "Double duty to either start or continue debugging"
			  (interactive)
			  (if (jde-debugger-running-p)
				  (jde-debug-cont)
				(jde-debug)))
			(define-key jde-mode-map '[f11] 'jde-debug-step-into)
			(define-key jde-mode-map '[f10] 'jde-debug-step-over)
			(define-key jde-mode-map '[S-f11] 'jde-debug-step-out)
			(define-key jde-mode-map '[f9] 'jde-debug-toggle-breakpoint)
			(define-key jde-mode-map '[f5] 'jde-debug-or-cont)
			(define-key jde-mode-map '[S-f5] 'jde-debug-quit)
			(setq iswitchb-buffer-ignore 
				  '("^ " 
					"^\\*[^.]*\\.[^.]*\\..*"
					"\\*JDE"
					"\\*jde"
					))))

;; so VS uses
;; F5        start debugging (also continue?)
;; S-F5      stop debugging
;; C-S-F5    restart debugging
;; F10       step over
;; F11       stop into
;; S-F11     step out


;; xml-mode (there is a sub-mode of this for xml but who knows how
;; to use it)
(add-hook 'sgml-mode-hook
		  (lambda ()
			(define-key sgml-mode-map "\C-c\C-c" 'comment-region)))


;; cs-mode
;; let's try to match what VS does so we can play nice
(add-hook 'csharp-mode-hook
		  (lambda ()
			(setq indent-tabs-mode nil)
			;; (local-set-key "\M-\\"   'cscomp-complete-at-point)
			;; (local-set-key "\M-\."   'cscomp-complete-at-point-menu)
			;; I'm not sure if these work at all
			(define-key csharp-mode-map (kbd "{") 'self-insert-command)
			(define-key csharp-mode-map (kbd "M-\\") 'cscomp-complete-at-point)
			(define-key csharp-mode-map (kbd "M-?") 'cscomp-complete-at-point-menu)
			(c-set-offset 'statement-cont 4)
			(c-set-offset 'arglist-cont-nonempty 4)
			(c-set-offset 'statement-block-intro 4)))

(add-hook 'lisp-mode-hook
		  (lambda ()
			(define-key lisp-mode-map "\C-c\C-v" 'cltl2-lookup)))

(add-hook 'lisp-interaction-mode-hook
		  (lambda () 
			(eldoc-mode)))

(add-hook 'emacs-lisp-mode-hook
		  (lambda ()
			(eldoc-mode)))

(add-hook 'find-file-hook 'bh-choose-header-mode)

;; set-buffer-file-coding-system
;; iso-latin-1-unix
;; utf-8-unix

(add-hook 'after-save-hook
		  'executable-make-buffer-file-executable-if-script-p)
(add-hook 'after-save-hook
		  'switch-mode-if-shebang)

(when mswindows-p
  (add-hook 'after-save-hook 'dos2unix-if-shebang))


;;;------------------------------------------------------
;;; variables!
;;;------------------------------------------------------
(defvar jdk-locations
  (if mswindows-p
	  `(("1.6.0" . ,(getenv "java_home"))
		("1.5.0" . "C:/somewhere"))
	'(quote (("1.5.0" . "/usr/lib/jvm/java-1.5.0-sun") 
			 ("1.6.0" . "/usr/lib/jvm/java-6-sun") 
			 ("gcj" . "/usr/lib/jvm/java-gcj")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(browse-url-browser-function (function browse-url-firefox))
 '(erc-max-buffer-size 20000)
 '(erc-modules (quote (autojoin button completion fill irccontrols list log match menu move-to-prompt netsplit networks noncommands readonly ring stamp track)))
 '(erc-truncate-mode t)
 '(fic-background-color "Black")
 '(fic-foreground-color "Red")
 '(fic-highlighted-words (quote ("FIXME" "TODO" "BUG" "KLUDGE" "HACK")))
 '(jde-compile-option-command-line-args (quote ("-Xlint")))
 '(jde-jdk (quote ("1.6.0")))
 '(jde-jdk-doc-url "http://download-llnw.oracle.com/javase/6/docs/api")
 '(jde-jdk-registry jdk-locations)
 '(safe-local-variable-values (quote ((lexical-binding . t))))
 '(tnt-auto-reconnect nil)
 '(uniquify-buffer-name-style (quote post-forward) nil (uniquify)))

;; FIXME make fic-mode work so I don't have to do this
;; FIXME eval-on-load ?
(modify-face 'font-lock-fic-face fic-foreground-color
			 fic-background-color nil t nil t nil nil)
(setf fic-search-list-re (regexp-opt fic-highlighted-words nil))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(twit-title-face ((t (:background "black" :underline "DeepSkyBlue"))))
 '(twit-zebra-1-face ((t (:background "gray30"))))
 '(twit-zebra-2-face ((t (:background "black")))))



;; (defvar imap-ssl-program '("openssl s_client -quiet -ssl3 -crlf -connect %s:%p"
;; 						   "openssl s_client -quiet -ssl2 -crlf -connect %s:%p"
;; 						   "s_client -quiet -ssl3 -crlf -connect %s:%p"
;; 						   "s_client -quiet -ssl2 -crlf -connect %s:%p")
;;   "A string, or list of strings, containing commands for SSL connections.
;; Within a string, %s is replaced with the server address and %p with
;; port number on server.  The program should accept IMAP commands on
;; stdin and return responses to stdout.  Each entry in the list is tried
;; until a successful connection is made.")
(defvar imap-log t)
(defvar imap-debug t)

(put 'narrow-to-region 'disabled nil)

;; I think something earlier on is clobbering this
;; even here it doesn't work
;; if I execute it in the scratch buffer it will though
(labels ((add-path (p)
				   (setenv "PATH" (concat p ";" (getenv "PATH")))))
  ;; (add-path "C:/Cygwin/bin")
  (add-path "C:/Util/gnutls-2.10.1/bin"))


;;; misc crap - move me
(add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)
;; holy crap this is good
(defun my-goto-match-beginning ()
  (when isearch-forward (goto-char isearch-other-end)))


;;; NOTES
;; to set emacs stuff in your files do this
;; -*- mode: org coding: UTF-8 -*-
;; this is called ?? something

;; useful commands that I don't remember yet
;; narrow / widen C-x n n|w

;; highlight-regexp M-s h r 
;; unhighlight-regexp M-s h u

;; wdired-change-to-wdired-mode
;; (rename files by directly editing in dired buffers)

