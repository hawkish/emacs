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
        alchemist
	js2-mode
	js2-refactor
	xref-js2
	rjsx-mode
	org
	flycheck
	flyspell-correct-popup
	magit
	multi-term
	typescript-mode
	ts-comint
	tide
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

;; Setup multi-term
(require 'multi-term)

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
        (set-frame-font "Menlo-12" t t)
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
        (set-frame-font "Menlo-12" t t)
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

(defun create-zsh ()
  "Make a multi-term buffer."
  (interactive)
  (let ((multi-term-program "zsh"))
    (multi-term)))

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

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

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

;; Prolog
(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi)
(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)
                                ("\\.m$" . mercury-mode))
			      auto-mode-alist))

;; Javascript
(require 'js2-mode)
;;(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
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
(require 'rjsx-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))

;; Typescript mode
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; ts-comint
(require 'ts-comint)
(add-hook 'typescript-mode-hook
          (lambda ()
            (local-set-key (kbd "C-x C-e") 'ts-send-last-sexp)
            (local-set-key (kbd "C-M-x") 'ts-send-last-sexp-and-go)
            (local-set-key (kbd "C-c b") 'ts-send-buffer)
            (local-set-key (kbd "C-c C-b") 'ts-send-buffer-and-go)
            (local-set-key (kbd "C-c l") 'ts-load-file-and-go)))

;; Tide mode
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; Org mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(add-hook 'org-mode-hook (lambda ()
			   (visual-line-mode)
			   (org-indent-mode)))

;; Spellcheck
;; brew install hunspell
;; Place a .aff and a .dic in "$HOME/Library/Spelling" for each language.
;; https://github.com/wooorm/dictionaries/tree/master/dictionaries
(setq ispell-program-name "hunspell")
(setq ispell-local-dictionary "da_DK")
(setq ispell-local-dictionary-alist
      '(("da_DK" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8)))
;; Tell relevant modes to use flyspell.
(require 'flyspell)
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-mode)
(define-key flyspell-mode-map (kbd "\C-cc") 'flyspell-correct-wrapper)

;; Tramp
(require 'tramp)
(setq tramp-default-method "scp")
(set-default 'tramp-default-proxies-alist (quote ((".*" "\\`root\\'" "/ssh:%h:"))))
