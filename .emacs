;; Package archives
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
         '("marmalade" .
           "https://marmalade-repo.org/packages/"))

(package-initialize)

(setq package-selected-packages
      '(
        company
        exec-path-from-shell
        slime
        slime-company
        auto-complete
        erc
        spaceline
	spacemacs-theme
        kotlin-mode
        groovy-mode
        ))

(defun install-packages ()
  "Install all required packages."
  (interactive)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package package-selected-packages)
    (unless (package-installed-p package)
      (package-install package))))

(install-packages)

;; load spacemacs theme
(load-theme 'spacemacs-dark t)

;; load the spaceline modeline theme
(require 'spaceline-config)
(spaceline-emacs-theme)

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
  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output))
    
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;;  Avoid the annoying startup message.
(setq inhibit-startup-message t)

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
;;(require 'moe-theme)
;;(moe-dark)

;; Disable tool-bar
(if window-system
    (tool-bar-mode -1)
  )

;; Flycheck
;;(global-flycheck-mode)

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

;; Groovy and Gradle section
;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(add-to-list 'load-path "~/.emacs.d/groovy")
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
;; For some reason, these recommendations don't seem to work with Aquamacs
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'auto-mode-alist '("\.gradle$" . groovy-mode))

;; Add prettier lambdas...
(defun add-pretty-lambda ()
  "make some word or string show as pretty Unicode symbols"
  (setq prettify-symbols-alist
        '(
          ("lambda" . 955) ; λ
          )))

(add-hook 'lisp-mode-hook 'add-pretty-lambda)
(add-hook 'haskell-mode-hook 'add-pretty-lambda)
(global-prettify-symbols-mode 1)

;; Haskell section
;; Now in haskell-mode in Melpa.
;; C-c C-l haskell-load-file
;; C-c C-b switch-to-haskell
;; On Linux
;; Install Intero
(package-install 'intero)
(add-hook 'haskell-mode-hook 'intero-mode)

;; Lisp section. 
;; Install Quicklisp:
;; sbcl --load quicklisp.lisp
;; (quicklisp-quickstart:install)
;; (ql:add-to-init-file)
;; Install Slime using Quicklisp:
;; (ql:quickload "quicklisp-slime-helper")
;; M-x slime to connect
;; C-c C-c to compile defun
;; C-c C-k to compile and load file
;; C-c C-z to switch to output buffer
(setq slime-lisp-implementations
  `((sbcl ("~/sbcl/bin/sbcl"))
   (clozure ("~/ccl/scripts/ccl -K utf-8"))
   (clisp ("/usr/bin/clisp" "-q -I"))))
(load (expand-file-name "~/quicklisp/slime-helper.el"))
(setq inferior-lisp-program "sbcl")
(setq slime-contribs '(slime-fancy slime-asdf hippie-expand-slime))
(setq slime-default-lisp 'sbcl)
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Tramp
(require 'tramp)
(setq tramp-default-method "scp")
(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (intero company exec-path-from-shell slime slime-company auto-complete erc spaceline spacemacs-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 120 :width normal :family "Monaco")))))
