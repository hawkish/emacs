;; Package archives
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
  )
(add-hook 'after-init-hook 'my-after-init-hook)
(defun my-after-init-hook ()
  ;; Do things after Emacs package initialization
  (if (eq system-type 'gnu/linux)
      (progn
	;; For the exec-path-from-shell needed for Mac OS to read .profile
	;; https://github.com/purcell/exec-path-from-shell
	(require 'exec-path-from-shell)
        (exec-path-from-shell-copy-env "SBCL_HOME")
	(exec-path-from-shell-initialize))
      )
  (if (eq system-type 'darwin)
      (progn
	;; For the exec-path-from-shell needed for Mac OS to read .profile
	;; https://github.com/purcell/exec-path-from-shell
	(require 'exec-path-from-shell)
        (exec-path-from-shell-copy-env "LANG")
        (exec-path-from-shell-copy-env "GEM_HOME")
        (exec-path-from-shell-copy-env "SBCL_HOME")
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
  ;; Colors in shell
  (autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
    
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

;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;;  Avoid the annoying startup message.
(setq inhibit-startup-message t)

;; Standard lib
(add-to-list 'load-path "~/emacs-lib")

;; Basic window handling
(scroll-bar-mode -1)
(set-fringe-mode '(0 . 0))

;; Create extra shells 
(defun create-shell ()
    "creates a shell with a given name"
    (interactive);; "Prompt\n shell name:")
    (let ((shell-name (read-string "shell name: " nil)))
    (shell (concat "*" shell-name "*"))))

(defun create-eshell ()
    "creates a shell with a given name"
    (interactive);; "Prompt\n shell name:")
    (let ((shell-name (read-string "shell name: " nil)))
    (eshell (concat "*" shell-name "*"))))

(defun create-ansi-term ()
    "creates a term with a given name"
    (interactive);; "Prompt\n term name:")
    (let ((shell-name (read-string "term name: " nil)))
    (ansi-term "bash" shell-name)))

(defun create-cygwin ()
  "Run cygwin bash in shell mode."
  (interactive)
  (let ((explicit-shell-file-name "C:/cygwin/bin/bash"))
    (call-interactively 'shell)))

;; Autocomplete settings
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "~/emacs-lib/ac-dict")
(require 'auto-complete-config)
(ac-config-default)

;; ERC setup
(require 'erc)
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

;; UTF-8 magic in OSX, Windows and Linux.
(setq utf-translate-cjk-mode nil) ; disable CJK coding/encoding (Chinese/Japanese/Korean characters)
(set-language-environment 'utf-8)
(set-keyboard-coding-system 'utf-8-mac) ; For old Carbon emacs on OS X only
(setq locale-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(unless (eq system-type 'windows-nt)
  (set-selection-coding-system 'utf-8))
(prefer-coding-system 'utf-8)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

;; Initialize color-theme
;;(load-theme 'tango-dark)
(load-theme 'gotham t)

;; Disable tool-bar
(if window-system
    (tool-bar-mode -1)
  )

;; Flycheck
(global-flycheck-mode)

;; Load the clock and column number
(unless (featurep 'xemacs)
  (display-time) ; put the current time in the modeline
  (column-number-mode t)
  )

;; Show the time on the status bar.
(setq display-time-24hr-format t)
(display-time)

;; Monaco needs to be installed first.
;; https://gist.github.com/rogerleite/99819
;; wget http://www.gringod.com/wp-upload/software/Fonts/Monaco_Linux.ttf
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

;; C, C++ section
(setq-default c-basic-offset 4)
(setq-default c-basic-indent 4)
(setq-default indent-tabs-mode nil)

;; Swift section. Requires swift-mode and fly-mode.
(if (eq system-type 'darwin)
    (require 'swift-mode)
  (setq flycheck-swift-sdk-path "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.11.sdk")
  (add-to-list 'flycheck-checkers 'swift)
  )

;; Groovy and Gradle section
;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(add-to-list 'load-path "~/.emacs.d/groovy")
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
;; For some reason, these recommendations don't seem to work with Aquamacs
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))

;; Haskell section
;; Now in haskell-mode in Melpa.
;; C-c C-l haskell-load-file
;; C-c C-b switch-to-haskell
;; On Linux
(require 'haskell-interactive-mode)
(require 'haskell-process)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(custom-set-variables
  '(haskell-process-suggest-remove-import-lines t)
  '(haskell-process-auto-import-loaded-modules t)
  '(haskell-process-log t)
  '(haskell-process-type 'cabal-repl))
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;; Lisp section. Install Slime.
;; M-x slime to connect
;; C-c C-c to compile defun
;; C-c C-k to compile and load file
;; C-c C-z to switch to output buffer
(setq inferior-lisp-program "~/sbcl/bin/sbcl")
;;(setq inferior-lisp-program "/usr/local/bin/clisp")
;;(setq inferior-lisp-program "~/ccl/scripts/ccl -K utf-8")
(setq slime-contribs '(slime-fancy))

;; Racket Scheme section. Install Geiser. C-c C-a to enter a module to REPL.
;;(setq geiser-active-implementations '(racket))

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


;; Install mode-compile to give friendlier compiling support!
(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)
(global-set-key "\C-cc" 'mode-compile)
(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)
(global-set-key "\C-ck" 'mode-compile-kill)



