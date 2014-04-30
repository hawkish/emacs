;; Package archives
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))
  
(add-hook 'after-init-hook 'my-after-init-hook)
(defun my-after-init-hook ()
  ;; do things after Emacs package initialization
  (if (eq system-type 'darwin)
      (progn
	;; For the exec-path-from-shell needed for Mac OS to read .profile
	;; https://github.com/purcell/exec-path-from-shell
	(require 'exec-path-from-shell)
        (exec-path-from-shell-copy-env "GOROOT")
	(exec-path-from-shell-copy-env "GOPATH")
	(exec-path-from-shell-initialize))
    )
  (if (eq system-type 'windows-nt)
      (progn
	;; Cygwin
	(setenv "PATH" (concat "c:/cygwin/bin;" (getenv "PATH")))
	(setq exec-path (cons "c:/cygwin/bin/" exec-path))
	(require 'cygwin-mount)
	(cygwin-mount-activate))
    )
     
  ;; Speedbar settings
  (require 'sr-speedbar)
  (global-set-key (kbd "<f8>") 'sr-speedbar-toggle)
  (setq sr-speedbar-right-side nil)
  (setq sr-speedbar-auto-refresh nil)
  (setq speedbar-smart-directory-expand-flag t)
  (setq speedbar-use-images nil)
  (setq speedbar-show-unknown-files t) 
  (setq speedbar-hide-button-brackets-flag t)
  (setq speedbar-directory-unshown-regexp "^$")
  (setq sr-speedbar-skip-other-window-p t)
  (sr-speedbar-open)
  ;; From graphene-speedbar.el
  ;; Always use the last selected window for loading files from speedbar.
  (defvar last-selected-window
    (if (not (eq (selected-window) sr-speedbar-window))
	(selected-window)
      (other-window 1)))
  (defadvice select-window (after remember-selected-window activate)
    "Remember the last selected window."
    (unless (or (eq (selected-window) sr-speedbar-window)
		(not (window-live-p (selected-window))))
      (setq last-selected-window (selected-window))))
  (defun sr-speedbar-before-visiting-file-hook ()
    "Function that hooks `speedbar-before-visiting-file-hook'."
    (select-window last-selected-window))
  (defun sr-speedbar-before-visiting-tag-hook ()
    "Function that hooks `speedbar-before-visiting-tag-hook'."
    (select-window last-selected-window))
  (defun sr-speedbar-visiting-file-hook ()
    "Function that hooks `speedbar-visiting-file-hook'."
    (select-window last-selected-window))
  (defun sr-speedbar-visiting-tag-hook ()
    "Function that hooks `speedbar-visiting-tag-hook'."
    (select-window last-selected-window))

  )

;;  Avoid the annoying startup message.
(setq inhibit-startup-message t)

;; Basic window handling
(scroll-bar-mode -1)
(set-fringe-mode '(0 . 0))

;; Create extra shells 
;; - because I can't remember C-u M-x eshell
(defun create-shell ()
    "creates a shell with a given name"
    (interactive);; "Prompt\n shell name:")
    (let ((shell-name (read-string "shell name: " nil)))
    (eshell (concat "*" shell-name "*"))))

;; ERC setup
(require 'erc)
(defcustom erc-fill-column 78
  "The column at which a filled paragraph is broken."
  :group 'erc-fill
  :type 'integer)
;; Check channels
(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
				"324" "329" "332" "333" "353" "477"))
;; Don't show any of this
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))
;; Nick added. 
(setq erc-prompt-for-nickserv-password t)
(setq erc-nick "grayling_")
;; Fix size of window.
(add-hook 'window-configuration-change-hook 
	   '(lambda ()
	      (setq erc-fill-column (- (window-width) 2))))

;; Color-theme
(if (eq system-type 'windows-nt)
    (progn
    ;; This will not work with the startup screen turned on.
    (setq default-directory "c:\\Local\\3420\\")
    (add-to-list 'load-path "~/emacs-lib/color-theme-6.6.0"))
  )
(if (eq system-type 'gnu/linux)
    (progn
    ;; This will not work with the startup screen turned on.
    (setq default-directory "/home/morten/")
    (add-to-list 'load-path "~/emacs-lib/color-theme-6.6.0"))
  )
(if (eq system-type 'darwin)
    (progn
    ;; This will not work with the startup screen turned on.
    (setq default-directory "/Users/morten/")
    (add-to-list 'load-path "~/emacs-lib/color-theme-6.6.0"))
  )

;; Standard lib
(add-to-list 'load-path "~/emacs-lib")

;; directory to put various el files into
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Initialize color-theme
(require 'color-theme)
(color-theme-initialize)
(color-theme-blue-mood)

;; Load the clock and column number
(unless (featurep 'xemacs)
  (display-time) ; put the current time in the modeline
  (column-number-mode t)
  )

;; Show the time on the status bar.
(setq display-time-24hr-format t)
(display-time)

(if (eq system-type 'gnu/linux)
    (custom-set-faces
     ;; custom-set-faces was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     '(default ((t (:height 100 :width normal :family "Monaco")))))
  )

(if (eq system-type 'darwin)
    (progn
    (custom-set-faces
     ;; custom-set-faces was added by Custom.
     ;; If you edit it by hand, you could mess it up, so be careful.
     ;; Your init file should contain only one such instance.
     ;; If there is more than one, they won't work right.
     '(default ((t (:height 120 :width normal :family "Monaco")))))
    (setq default-input-method "MacOSX")
    (setq mac-command-modifier 'meta
	  mac-option-modifier nil
	  mac-command-key-is-meta t)
    

    )
  )

(setq make-backup-files nil)

;; Go section
(require 'go-mode-load)
(unless (eq system-type 'windows-nt)
  ;; This doesn't work in Windows. Frak!
  (add-hook 'before-save-hook 'gofmt-before-save))

;; Groovy and Gradle section
;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(add-to-list 'load-path "~/.emacs.d/groovy")
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
;; For some reason, these recommendations don't seem to work with Aquamacs
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))

;; Haskell section
;; C-c C-l inferior-haskell-load-file
;; C-c C-b switch-to-haskell
;; On Linux
(if (eq system-type 'gnu/linux)
    (progn
      (setq haskell-program-name "/usr/bin/ghci")
      (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent))
  )
;; On Mac
(if (eq system-type 'darwin)
    (progn
      (setq haskell-program-name "/usr/local/bin/ghci")
      (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent))
  )
;; On Windows
(if (eq system-type 'windows-nt)
    (progn
      (setq haskell-program-name "c:\\Local\\3420\\haskell\\2013.2.0.0\\bin\\ghci.exe")
      (add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent))
  )

;; Clojure section
;; (require 'paredit) if you didn't install it via package.el
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; Tramp
(require 'tramp)
(setq tramp-default-method "scp")
(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))

;;Javascript section
;; The elisp below Requires an installation of node.js
;; eshell needs this in Mac OS. 
(setenv "NODE_NO_READLINE" "1")
;;
;; Requires https://code.google.com/p/js2-mode/
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; Requires http://js-comint-el.sourceforge.net/
;; Run with M-x run-js
(require 'js-comint) 
(setq inferior-js-program-command "node --interactive")
;; Use your favorited js mode here:
(add-hook 'js2-mode-hook '(lambda () 
			    (local-set-key "\C-x\C-e" 
					   'js-send-last-sexp)
			    (local-set-key "\C-\M-x" 
					   'js-send-last-sexp-and-go)
			    (local-set-key "\C-cb" 
					   'js-send-buffer)
			    (local-set-key "\C-c\C-b" 
					   'js-send-buffer-and-go)
			    (local-set-key "\C-cl" 
					   'js-load-file-and-go)
			    ))


;;Actionscript section
;;(autoload 'actionscript-mode "actionscript-mode" "Major mode for actionscript." t)
(load-file "~/emacs-lib/actionscript-mode.el")
(autoload 'actionscript-mode "actionscript-mode" "Major mode for actionscript." t)

;; Activate actionscript-mode for any files ending in .as
(setq auto-mode-alist  (cons '(".as$" . actionscript-mode) auto-mode-alist))

;; Install mode-compile to give friendlier compiling support!
(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(global-set-key "\C-cc" 'mode-compile)
(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key "\C-ck" 'mode-compile-kill)

