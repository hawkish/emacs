(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Bootstrap 'use-package'
(eval-after-load 'gnutls
  '(add-to-list 'gnutls-trustfiles "/etc/ssl/cert.pem"))
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

(use-package auto-package-update
   :ensure t
   :config
   (setq auto-package-update-delete-old-versions t
         auto-package-update-interval 4)
   (auto-package-update-maybe))

(eval-when-compile
  (require 'use-package))
(require 'package)

(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize))

(use-package sly
  :ensure t
  :bind (("C-<up>" . comint-previous-input)
	 ("C-<down>" . comint-next-input)))

(use-package geiser-chez
  :ensure t
  :after (paredit)
  :bind
  (("C-c g r" . run-chez)
   ("C-c g c" . geiser-connect))
  :hook
  (geiser-repl-mode . paredit-mode))

;; C-M-f and C-M-b for navigating
;; C-M-left Slurp Backward
;; C-M-Right Barf Backward
;; https://calva.io/paredit/#what-is-paredit 
(use-package paredit
  :ensure t
  :config
  :hook
  (emacs-lisp-mode . paredit-mode)
  ;; enable in the *scratch* buffer
  (lisp-interaction-mode . paredit-mode)
  (ielm-mode . paredit-mode)
  (lisp-mode . paredit-mode)
  (scheme-mode . paredit-mode)
  (racket-mode . paredit-mode)
  (eval-expression-minibuffer-setup . paredit-mode))

(use-package auto-complete
  :ensure t
  :init
  (progn
    (ac-config-default)
    (global-auto-complete-mode t)))

(use-package highlight-parentheses
  :ensure t
  :diminish highlight-parentheses-mode
  :commands highlight-parentheses-mode)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(setq show-paren-delay 0)
(show-paren-mode 1)

(use-package highlight-symbol
  :ensure t
  :config
  (setq highlight-symbol-idle-delay 0.5)
  :hook (prog-mode . highlight-symbol-mode))

(use-package auto-complete
  :ensure t
  :diminish auto-complete-mode
  :config
  (require 'auto-complete-config)
  (ac-config-default))

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package spaceline
  :ensure t
  :config
  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main)))))

(use-package spaceline-config
  :ensure spaceline
  :config
  (spaceline-helm-mode 1)
  (spaceline-emacs-theme))

(use-package vscode-dark-plus-theme
  :ensure t
  :config
  (load-theme 'vscode-dark-plus t))

(use-package ivy
  :ensure t)

(use-package counsel
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package org
  :ensure t)

(use-package flycheck
  :ensure t)

(use-package flyspell-correct-popup
  :ensure t)

(use-package multi-term
  :ensure t)

(use-package js2-mode
  :ensure t
  :defer t
  :config
  (use-package ac-js2
    :ensure t
    :defer t
    :config
    (add-hook 'js2-mode-hook 'ac-js2-mode)))

(use-package magit
  :defer t
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package dumb-jump
  :ensure t
  :bind (("C-x w" . dumb-jump-go-other-window)
         ("C-x j" . dumb-jump-go)
         ("C-x b" . dumb-jump-back)
         ("C-x q" . dumb-jump-quick-look)
         ("C-x x" . dumb-jump-go-prefer-external)
         ("C-x z" . dumb-jump-go-prefer-external-other-window)) 
  :init (dumb-jump-mode)
  :config (setq dumb-jump-selector 'ivy))

(use-package minimap
  :ensure t
  :bind (("\C-n" . minimap-mode)))

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; Display line numbers with a small margin.
(global-display-line-numbers-mode)
(setq-default left-margin-width 2 right-margin-width 2)
(set-window-buffer nil (current-buffer))

;; Line spacing
(setq-default line-spacing 0.3)

;; Ivy mode
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)

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
        (exec-path-from-shell-initialize)
        ;; Monaco needs to be installed first.
        ;; https://gist.github.com/rogerleite/99819
        ;; wget http://www.gringod.com/wp-upload/software/Fonts/Monaco_Linux.ttf
        (set-frame-font "Menlo-12" t t)
        (setq default-directory "~/Documents/git/")
        (setq default-input-method "MacOSX")
        (setq mac-command-modifier 'meta
              mac-option-modifier nil
              mac-command-key-is-meta t))))

;; Colors in shell
(ansi-color-for-comint-mode-on)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
    
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; Avoid the annoying startup message.
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

;; Autocomplete settings
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories "~/emacs-lib/ac-dict")
(require 'auto-complete-config)
(ac-config-default)

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'clojure-mode-hook       #'enable-paredit-mode)

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
;; Remember to:
;; ros install slime
;; and evaluate:
;; (ql:quickload "quicklisp-slime-helper") 
;; M-x slime to connect
;; C-c C-c to compile defun
;; C-c C-k to compile and load file
;; C-c C-z to switch to output buffer
;; Do some standard SLIME configuration.
;;(slime-setup '(slime-fancy slime-tramp))
;; Set the default lisp you want to use (here it's SBCL).
;;(load (expand-file-name "~/.roswell/lisp/quicklisp/slime-helper.el"))
;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "ros -Q run")
;;(setq show-paren-delay 0)
;;(show-paren-mode 1)
;;(add-hook 'lisp-mode-hook
;;          (lambda ()
;;            (local-set-key "\C-cp" 'slime-close-all-parens-in-sexp)))

;; Scheme section.
;; brew install --cask racket
;; Racket shortcuts
;; C-c C-k to compile and load file
;; C-c C-a to jump to REPL and switch module
;; ;; brew install chezscheme
;; Chez Scheme shortcuts
;; C-x C-e will eval the s-expression just before point.
;; C-c C-z shortcut will bring you from REPL to the buffer. 
;; C-c C-k to compile the file
;; C-j for new line in REPL
;; Selecting scheme or racket as default implementation of Scheme.
(add-hook 'scheme-mode-hook 'geiser-mode)
;;(setq geiser-active-implementations '(racket))
(setq geiser-active-implementations '(chez))
;; Extra setting for scheme.
(setq geiser-chez-binary "chez")

;; Prolog
(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi)
(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)
                                ("\\.m$" . mercury-mode))
			      auto-mode-alist))

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(minimap-window-location 'right)
 '(package-selected-packages
   '(highlight-symbol rainbow-delimiters multi-term yaml-mode vscode-dark-plus-theme use-package spaceline sly minimap markdown-mode magit highlight-parentheses flyspell-correct-popup flycheck exec-path-from-shell dumb-jump counsel company auto-complete ac-js2))
 '(safe-local-variable-values '((Syntax . Common-Lisp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 89)) (:background "#1e1e1e" :foreground "#d4d4d4" :family "Menlo" :foundry "nil" :slant normal :weight normal :height 120 :width normal)))))
