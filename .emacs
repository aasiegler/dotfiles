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
(defvar home (concat (expand-file-name "~") "/"))
(defvar emacs-home (concat home ".emacs.d/"))

(setq debug-on-error t)

(setq inhibit-splash-screen t)
(if (boundp 'scroll-bar-mode)
    (scroll-bar-mode -1))
(menu-bar-mode -1)
(tool-bar-mode -1)

(column-number-mode t)

;; set the screen size to the whole thing if it's lappy
;; or just normal if not
(if (and (= 600 (display-pixel-height))
		 (= 1024 (display-pixel-width)))	 
	(setq default-frame-alist '((width . 126) (height . 36)))
  (setq default-frame-alist '((width . 80) (height . 60))))


(setq default-tab-width 4)
(transient-mark-mode t)
(show-paren-mode t)
(global-font-lock-mode t)

(fset 'yes-or-no-p 'y-or-n-p)
(iswitchb-mode 1)

(setq frame-title-format (concat  "%b - emacs@" system-name))

(when mswindows-p
  (setq python-python-command "C:/Python/Python27/python.exe"))

;; I'd rather use zsh with both but it works poorly
;; in my setup
(if mswindows-p
	(unless (get-buffer "*eshell*")
	  (eshell))
  (unless (get-buffer "*ansi-term*")
	(ansi-term "zsh")))

;;;------------------------------------------------------
;;; set-keys
;;;------------------------------------------------------

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(global-set-key "\M-gg" 'goto-line)
(global-set-key "\C-x\C-m" 'execute-extended-command)
;; (global-set-key "\M-n" 'scroll-down-keep-cursor)
;; (global-set-key "\M-p" 'scroll-up-keep-cursor)

;; (global-set-key "%" 'goto-match-paren)

(global-set-key '[f4] 'speedbar-get-focus)
(global-set-key (kbd "C-c C-t") 'toggle-transparency)

(global-set-key "\C-c\C-h" 'ff-find-other-file)

(global-set-key (kbd "C-x W") 'fix-horizontal-size)

(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))

(global-set-key "\C-x\C-c" 'paranoid-exit-from-emacs)



;;;------------------------------------------------------
;;; load/autoloading .el files
;;;------------------------------------------------------
(setq load-path (append (mapcar
						 (lambda (p) (concat emacs-home p))
						 '("lisp/"
						   "site-lisp"
						   "site-lisp/color-theme-6.6.0"
						   "site-lisp/elib-1.0"
						   "site-lisp/jdee-2.4.0.1/lisp"
						   "site-lisp/slime"
						   "site-lisp/slime/contrib"
						   "site-lisp/tnt-2.6"
						   "site-lisp/w3m"
						   "site-lisp/darkroom"))
						load-path))
(add-to-list 'load-path "/usr/share/emacs/site-lisp/w3m")
(if mswindows-p
	(add-to-list 'load-path "c:/Util/IdeBridge/lisp"))

(load-library "util")
(load-library "misc")
(load-library "font-select")

(require 'cl)
(require 'color-theme)
(require 'cltl2)
(require 'g-utils)
(require 'fic-mode)
(require 'rainbow-parens)
(require 'scratch)
(require 'slime)
(require 'slime-autoloads)
(require 'tnt)
(require 'w32-fullscreen)
(require 'w3m-load)

(autoload 'csharp-mode "csharp-mode.el" "Major mode for editing C# code." t)
(autoload 'erlang-mode "erlang.el")
(autoload 'flymake-php "flymake-php.el" "minor mode for catching php errors" t)
(autoload 'forth-mode "gforth.el")
(autoload 'forth-block-mode "gforth.el")
(autoload 'html-php-mode "html-php.el" "multi-mode for php and html" t)
(autoload 'intercal-mode "intercal.el" "Load intercal-mode" t)
(autoload 'jde-mode "jde.el" "Majore mode for editing java" t)
(autoload 'js2-mode "js2.elc" "Major mode for editing javascrip" t) 
(autoload 'multi-mode "multi-mode.el" "mulit-mode support" t)
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
				("\\.java$" . jde-mode))
			  auto-mode-alist))
;; ("\\.py$" . python-mode)


(set-font "consolas 10"
		  "inconsolata 11"
		  "Mono 10")
(color-theme-initialize)
(color-theme-tangotango)
;; arjen clarity sitaramv-solaris tangotango

(if mswindows-p
	(setq inferior-lisp-program "c:/Program Files (x86)/Steel Bank Common Lisp/1.0.37/sbcl.exe --noinform")
  (setq inferior-lisp-program "sbcl --noinform"))
;; or just slime-fancy I think
(slime-setup '(slime-repl slime-editing-commands))

;; cywgin stuff if windows
(when mswindows-p
  (progn
	(setenv "PATH" (concat "c:/cygwin/bin;" (getenv "PATH")))
	(setq exec-path (cons "c:/cygwin/bin/" exec-path))
	(require 'cygwin-mount)
	(cygwin-mount-activate)))

(setq org-agenda-files (list "~/org" "~/My Dropbox/org"))


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

(add-hook 'csharp-mode-hook
          (lambda ()
            (c-set-offset 'statement-block-intro 4)))

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
            (c-set-offset 'statement-cont 4)
            (c-set-offset 'arglist-cont-nonempty 4)))

(add-hook 'lisp-mode-hook
		  (lambda ()
			(define-key lisp-mode-map "\C-c\C-v" 'cltl2-lookup)))

(add-hook 'find-file-hook 'bh-choose-header-mode)



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
 ;;'(jde-ant-read-buildfile t)
 '(jde-compile-option-command-line-args (quote ("-Xlint")))
 '(jde-jdk (quote ("1.6.0")))
 ; '(jde-jdk-doc-url "http://java.sun.com/j2se/1.5.0/docs/api/")
 '(jde-jdk-doc-url "http://download-llnw.oracle.com/javase/6/docs/api")
 '(jde-jdk-registry jdk-locations)
 ;; '(jde-jdk-registry (quote 
 ;; 					 (("1.5.0" . "/usr/lib/jvm/java-1.5.0-sun") 
 ;; 					  ("1.6.0" . "/usr/lib/jvm/java-6-sun") 
 ;; 					  ("gcj" . "/usr/lib/jvm/java-gcj"))))
 '(browse-url-browser-function #'w3m-goto-url)
 ;; '(tnt-username-alist (("aaronsiegler") ("heyitsjesusc")))
 '(tnt-auto-reconnect nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(twit-title-face ((t (:background "black" :underline "DeepSkyBlue"))))
 '(twit-zebra-1-face ((t (:background "gray30"))))
 '(twit-zebra-2-face ((t (:background "black")))))


