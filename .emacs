;; Raise gc threshold for init
(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook
	  (lambda() (setq gc-cons-threshold 67108864))) ; 64 mb

;; For performance reading lsp responses
(setq read-process-output-max (* 1024 1024)) ;; 1mb


(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
;; (setq use-package-compute-statistics t)

(set-face-attribute 'default nil
                    :font "JetBrains Mono"
                    :height 120)

;; Misc
(setq require-final-newline 't)
(setq-default create-lockfiles nil)
(setq select-enable-clipboard t)
;; Locate backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
;; customize file
(setq custom-file "~/.config/emacs-custom.el")
(load custom-file 'noerror)

;; Easier shortcut
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C-b") 'switch-to-buffer)
(global-set-key (kbd "C-x DEL") 'whitespace-cleanup)
(global-unset-key (kbd "C-c C-z"))
(global-unset-key (kbd "C-x C-z"))
(windmove-default-keybindings 'meta)
(global-set-key [f6] 'recompile)
(setq-default fill-column 100)

;; Compilation
(setq compilation-scroll-output "scroll")

(global-set-key
 (kbd "<f5>")
 (lambda (&optional force-reverting)
   "Interactive call to revert-buffer. Ignoring the auto-save
 file and not requesting for confirmation. When the current buffer
 is modified, the command refuses to revert it, unless you specify
 the optional argument: force-reverting to true."
   (interactive "P")
   ;;(message "force-reverting value is %s" force-reverting)
   (if (or force-reverting (not (buffer-modified-p)))
       (revert-buffer :ignore-auto :noconfirm)
     (error "The buffer has been modified"))))

;; Minimalist interface
(setq inhibit-startup-screen t)
(setq visible-bell  t)
(setq inhibit-splash-screen t)
;; (setq tab-always-indent 'complete)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq frame-title-format "%b - Emacs")
(setq icon-title-format "%b - Emacs")
(fset 'yes-or-no-p 'y-or-n-p)

(line-number-mode t)
(column-number-mode t)

;; Easy parenthesis and delimiters
(electric-pair-mode)
(show-paren-mode 1)

;; Do not clutter the modeline
(use-package diminish
  :ensure t)

;; Help when forgetting keys
(use-package which-key
  :ensure t
  :diminish
  :config
  (which-key-mode))

;; CRUX
(use-package crux
  :ensure t
  :bind (
	 ("C-k" . crux-smart-kill-line)
	 ("C-c I" . crux-find-user-init-file)
	 ("C-c S" . crux-find-shell-init-file)
	 ("C-a" . crux-move-beginning-of-line)))

(defun copy-buffer()
  "Copy the whole buffer to clipboard."
  (interactive)
  (kill-new (buffer-string))
  (message "Buffer copied to clipboard"))

(global-set-key (kbd "C-x H") 'copy-buffer)

;; Theme. Maybe check gruvbox
(use-package darktooth-theme
  :ensure t
  :config
  (load-theme 'darktooth t))

;; (use-package nord-theme
;;   :ensure t
;;   :config
;;   (load-theme 'nord t))

(use-package org
  :ensure t
  :config
  (setq org-src-tab-acts-natively t
	org-hide-emphasis-markers t
	org-hide-leading-stars t
	org-todo-keywords '((sequence "TODO" "WAITING" "|" "DONE")))
  (add-hook 'org-mode-hook (lambda () (setq fill-column 100
					    olivetti-body-width 100))))

;; Pretty bullets in org mode
(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode))

;; Nice centered view
(use-package olivetti
  :ensure t
  :hook (org-mode . olivetti-mode)
  :config (define-key olivetti-mode-map (kbd "C-c |") nil))

;; Ido for easy completion
(use-package ido
  :ensure t
  :config
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  ;; Do not try to match when creating file
  (setq ido-auto-merge-work-directories-length -1)
  (ido-mode 1)
  (global-set-key
   "\M-x"
   (lambda ()
     (interactive)
     (call-interactively
      (intern
       (ido-completing-read
        "M-x "
        (all-completions "" obarray 'commandp)))))))

(use-package ido-vertical-mode
  :ensure t
  :after ido
  :config
  (ido-vertical-mode 1))

;; Quick access to a shell at any time
(use-package shell-pop
  :ensure t
  :bind (("C-z" . shell-pop)
	 ("C-x z" . shell-pop)
	 ("C-c C-z" . shell-pop))
  :config
  (setq shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  (shell-pop--set-shell-type 'shell-pop-shell-type shell-pop-shell-type))

;; Git interface
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
	 ("C-x b" . magit-blame)))

;; (use-package forge
;;   :ensure t
;;   :after magit
;;   :config (setq auth-sources '("~/.authinfo")))

;; (use-package code-review
;;   :ensure t)

;; Quick github links to code
(use-package git-link
  :ensure t
  :bind ("C-c g l" . git-link)
  ;; Otherwise preview links doesn't work in github conversations
  :config (setq git-link-use-commit t))

;; Syntax checker for natural language
(use-package flyspell
  :ensure t
  :diminish ;;(flyspell-mode . " φ")
  :hook
  ((text-mode latex-mode LaTeX-mode) . flyspell-mode)
  (prog-mode . flyspell-prog-mode)
  (git-commit-setup  git-commit-turn-on-flyspell)
  (yaml-mode lambda() (flyspell-mode 0))
  :bind
  ("C-c f" . flyspell-check-previous-highlighted-word)
  ("C-c l" . my-cycle-ispell-languages)
  :config
  (ispell-change-dictionary "english" t)
  (add-to-list 'ispell-skip-region-alist '("^#+BEGIN_SRC" . "^#+END_SRC"))
  (let ((langs '("francais" "english")))
    (setq lang-ring (make-ring (length langs)))
    (dolist (elem langs) (ring-insert lang-ring elem)))
  (defun my-cycle-ispell-languages ()
    (interactive)
    (let ((lang (ring-ref lang-ring -1)))
      (ring-insert lang-ring lang)
      (ispell-change-dictionary lang)))
  ;; Do not correct string in prog mode
  (setq flyspell-prog-text-faces '(font-lock-comment-face font-lock-doc-face)))

;; Syntax checking for programming languages
(use-package flycheck
  :ensure t
  :diminish
  :bind ("C-n" . flycheck-next-error)
  :defer t
  :hook
  (prog-mode . flycheck-mode)
  (hledger-mode . flycheck-mode))

;; Easier region selection
(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region))

;; Multiple cursors
(use-package multiple-cursors
  :ensure t
  :bind
  ("C->" . mc/mark-next-like-this)
  ("C-s->" . mc/edit-lines))

;; Project wide interactions
(use-package projectile
  :ensure t
  :diminish
  :bind
  (("C-c p" . projectile-command-map)
   ("C-p" . projectile-command-map))
  :config
  (projectile-mode 1))

(use-package ripgrep
  :ensure t )

;; Snippets !
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :defer 2
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
;; Some of my go snippets like rne and iel aren't loaded
  :defer
  :after yasnippet)

;; Programming auto-completion
(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :defer 2
  :config
  (setq
   company-minimum-prefix-length 2
   company-tooltip-align-annotations t)
  (global-set-key (kbd "TAB") 'company-indent-or-complete-common))


  ;; 	company-require-match 'never
  ;; 	company-idle-delay 0.1
  ;; 	company-tooltip-limit 10
  ;; 	company-selection-wrap-around t
  ;; 	company-transformers '(company-sort-by-occurrence)
  ;; 	company-dabbrev-downcase nil)
  ;; (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  ;; (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  ;; (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
  ;; (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  ;; (global-set-key "\M-/" 'company-complete))

;; Pop up documentation for suggestions
(use-package company-quickhelp
  :ensure t
  :after company
  :defer t
  :init (company-quickhelp-mode 1))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook ((go-mode . lsp-deferred)
	 (lsp-mode . (lambda ()
		       (let ((lsp-keymap-prefix "C-l"))
			 (lsp-enable-which-key-integration)))))
  :config
  ;;(setq lsp-auto-guess-root t)
  (setq lsp-file-watch-threshold 2000)
  (define-key lsp-mode-map (kbd "C-l") lsp-command-map))


(defun lsp-go-install-save-hooks ()
  "Setup before save hooks for go mode."
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :bind (("<f7>" . lsp-ui-flycheck-list)
	 ("<f8>" . lsp-ui-imenu	)))

(use-package lsp-treemacs
  :ensure t)

;; Language specific packages
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

(use-package dockerfile-mode
  :ensure t)

(use-package go-impl
  ;; go-impl need a patch to work https://github.com/emacsorphanage/go-impl/pull/9/files
  :ensure t)

(use-package go-mode
  :ensure t
  :after go-impl
  :bind ("C-c i" . go-impl)
  :hook (go-mode . (lambda() (setq tab-width 4))))

(use-package clojure-mode
  :ensure t
  :hook ((clojure-mode . lsp)
	 ;;(before-save . cider-format-buffer)
	 ))

(use-package cider
  :ensure t
  :pin melpa-stable)

(use-package yaml-mode
  :ensure t
  :config (setq yaml-indent-offset 2))

(use-package protobuf-mode
  :ensure t
  :config (setq c-basic-offset 4))

(use-package hcl-mode
  :ensure t)

(use-package puppet-mode
  :ensure t)

(use-package jenkinsfile-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

(use-package earthfile-mode
  :ensure t)

(use-package terraform-mode
  :ensure t)

(add-hook 'html-mode-hook (lambda() (setq tab-width 2)))

;; Compta
(use-package hledger-mode
  :ensure t
  :mode ("\\.journal\\'" . hledger-mode)
  :init
  (add-hook 'hledger-mode-hook
            (lambda ()
              (make-local-variable 'company-backends)
              (add-to-list 'company-backends 'hledger-company))))

(use-package flycheck-hledger
  :ensure t
  :after (flycheck hledger-mode)
  :demand t)



