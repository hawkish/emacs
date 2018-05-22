;; Package archives
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

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
        zenburn-theme
        kotlin-mode
        groovy-mode
        ivy
        counsel
        auctex
        yaml-mode
        swift-mode
        geiser
        julia-mode
        julia-repl
        alchemist
        elixir-mode
	js2-mode
	js2-refactor
	xref-js2
        ))

(package-install-selected-packages)

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
;;(load-theme 'spacemacs-dark t)
(load-theme 'zenburn t)

;; Ivy mode
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

;; load the spaceline modeline theme
(require 'spaceline-config)
(spaceline-emacs-theme)

;; Stop the infernal bell
(setq ring-bell-function #'ignore)

(add-hook 'after-init-hook 'my-after-init-hook)
(defun my-after-init-hook ()
  ;; Do things after Emacs package initialization
  (when (eq system-type 'gnu/linux)
      (progn
	;; For the exec-path-from-shell needed for Mac OS to read .profile
	;; https://github.com/purcell/exec-path-from-shell
	(require 'exec-path-from-shell)
        (exec-path-from-shell-copy-env "SBCL_HOME")
	(exec-path-from-shell-initialize)
        ;; Monaco needs to be installed first.
        ;; https://gist.github.com/rogerleite/99819
        ;; wget http://www.gringod.com/wp-upload/software/Fonts/Monaco_Linux.ttf
        (set-frame-font "Monaco-12" t t)
        (setq default-directory "~/Documents/git/")) 

      )
  (when (eq system-type 'darwin)
      (progn
	;; For the exec-path-from-shell needed for Mac OS to read .profile
	;; https://github.com/purcell/exec-path-from-shell
	(require 'exec-path-from-shell)
        (exec-path-from-shell-copy-env "LANG")
        (exec-path-from-shell-copy-env "GEM_HOME")
        (exec-path-from-shell-copy-env "SBCL_HOME")
	(exec-path-from-shell-initialize)
        ;; Monaco needs to be installed first.
        ;; https://gist.github.com/rogerleite/99819
        ;; wget http://www.gringod.com/wp-upload/software/Fonts/Monaco_Linux.ttf
        (set-frame-font "Monaco-12" t t)
        (setq default-directory "~/Documents/git/")
        (setq default-input-method "MacOSX")
        (setq mac-command-modifier 'meta
              mac-option-modifier nil
              mac-command-key-is-meta t))

    )
  (if (eq system-type 'windows-nt)
      (progn
	;; Cygwin
	(setenv "PATH" (concat "c:/cygwin/bin;" (getenv "PATH")))
	(setq exec-path (cons "c:/cygwin/bin/" exec-path))
	(require 'cygwin-mount)
	(cygwin-mount-activate))
    ))

;; Colors in shell
(ansi-color-for-comint-mode-on)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
    
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;;  Avoid the annoying startup message.
(setq inhibit-startup-message t)

;; Enable auto revert mode. So I don't have to.
(global-auto-revert-mode t)

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

;; Adding to ruby-mode
(add-to-list 'auto-mode-alist '("Fastfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Podfile$" . ruby-mode))

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

(setq make-backup-files nil)

;; Add prettier lambdas...
(defun add-pretty-lambda ()
  "make some word or string show as pretty Unicode symbols"
  (setq prettify-symbols-alist
        '(
          ("lambda" . 955) ; λ
          )))

(add-hook 'lisp-mode-hook 'add-pretty-lambda)
(add-hook 'haskell-mode-hook 'add-pretty-lambda)
(add-hook 'scheme-mode-hook 'add-pretty-lambda)
(add-hook 'racket-mode-hook 'add-pretty-lambda)
(add-hook 'geiser-mode-hook 'add-pretty-lambda)
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

;; Racket section.
;; C-c C-k to compile and load file
;; C-c C-a to jump to REPL and switch module
;; Selecting racket as default implementation of Scheme.
(add-hook 'scheme-mode-hook 'geiser-mode)
(setq geiser-active-implementations '(racket))

;; Elixir section.
(require 'elixir-mode)
(add-to-list 'elixir-mode-hook 'alchemist-mode)
(add-to-list 'elixir-mode-hook 'company-mode)
;;(add-to-list 'elixir-mode-hook 'ruby-end-mode)
(setq alchemist-mix-command "/usr/local/Cellar/elixir/1.6.4/bin/mix")
(setq alchemist-iex-program-name "/usr/local/Cellar/elixir/1.6.4/bin/iex")
(setq alchemist-compile-command "/usr/local/Cellar/elixir/1.6.4/bin/elixirc")
(setq alchemist-key-command-prefix (kbd "C-c a"))


;; Julia
(add-to-list 'load-path "~/.emacs.d/elpa/julia-mode-2017916.628")
(require 'julia-mode)
(add-to-list 'load-path "~/.emacs.d/elpa/julia-repl-20171005.123")
(require 'julia-repl)
(add-hook 'julia-mode-hook 'julia-repl-mode)

;; Javascript
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
(require 'js2-refactor)
(require 'xref-js2)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)
;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)
(add-hook 'js2-mode-hook (lambda ()
  (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

;; Tramp
(require 'tramp)
(setq tramp-default-method "scp")
(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(display-time-mode t)
 '(package-selected-packages
   (quote
    (ac-alchemist geiser ruby-mode intero company exec-path-from-shell slime slime-company auto-complete erc spaceline spacemacs-theme kotlin-mode groovy-mode ivy counsel)))
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
