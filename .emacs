;; Raise gc threshold for init time
(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook
	  (lambda() (setq gc-cons-threshold 67108864))) ; 64 mb

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
; (setq use-package-compute-statistics t)

;; Misc
(setq require-final-newline 't)
(setq-default create-lockfiles nil)
(setq select-enable-clipboard t)
;; Locate backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; Easier shortcut
(global-set-key (kbd "C-x k") 'kill-this-buffer)
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "C-b") 'switch-to-buffer)
(global-unset-key (kbd "C-x DEL"))
(global-unset-key (kbd "C-c C-z"))
(global-unset-key (kbd "C-x C-z"))
; (global-set-key (kbd "C-z") 'shell)

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

;; Make sure diminish is present
(use-package diminish
  :ensure t)

;; Help when forgetting keys
(use-package which-key
  :ensure t
  :diminish
  :config
  (which-key-mode))

;; Theme. Maybe check gruvbox
(use-package darktooth-theme
  :ensure t
  :config
  (load-theme 'darktooth t))

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
  :bind ("C-x g" . magit-status))

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
  :defer t
  :hook
  (prog-mode . flycheck-mode))

;; Easier region selection
(use-package expand-region
  :ensure t
  :bind
  ("C-=" . er/expand-region))

;; Project wide interactions
(use-package projectile
  :ensure t
  :diminish
  :bind
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode 1))
;; Snippets !
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :defer 2
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
  :defer
  :after yasnippet)

;; Programming auto-completion
(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :defer 2
  :config
  (setq company-minimum-prefix-length 1
	company-tooltip-align-annotations t
	company-require-match 'never
	company-idle-delay 0.1
	company-tooltip-limit 10
	company-selection-wrap-around t
	company-transformers '(company-sort-by-occurrence))
  (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "<tab>") 'company-complete-common-or-cycle)
  (define-key company-active-map (kbd "S-TAB") 'company-select-previous)
  (define-key company-active-map (kbd "<backtab>") 'company-select-previous)
  (global-set-key "\M-/" 'company-complete))

;; Pop up documentation for suggestions
(use-package company-quickhelp
  :ensure t
  :after company
  :defer t
  :init (company-quickhelp-mode 1))

(use-package company-lsp
  :ensure t
  :commands company-lsp)

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred)
  :config
  (setq lsp-auto-guess-root t))

(defun lsp-go-install-save-hooks ()
  "Setup before save hooks for go mode."
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; Language specific packages
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

(use-package go-mode
  :ensure t)
;; )))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (impatient-mode emmet-mode web-mode git-commit-insert-issue yaml-mode yasnippet-snippets yasnippet darktooth-theme darktooh-theme company-quickhelp company which-key flycheck diminish projectile expand-region magit go-mode markdown-mode ido-vertical-mode use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
