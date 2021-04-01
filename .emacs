;; Raise gc threshold for init
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

(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
  :config
  (setq exec-path-from-shell-variables '("PATH" "GOPATH"))
  (exec-path-from-shell-initialize))
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
(windmove-default-keybindings 'super)
(global-set-key [f6] 'recompile)
; (global-set-key (kbd "C-z") 'shell)

;; Compilation
(setq compilation-scroll-output 'first-error)

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
		org-hide-leading-stars t)
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
  :defer t
  :hook
  (prog-mode . flycheck-mode))

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
  ("C-c p" . projectile-command-map)
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
	company-transformers '(company-sort-by-occurrence)
	company-dabbrev-downcase nil)
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
  :hook ((go-mode . lsp-deferred)
		 (lsp-mode . (lambda ()
					   (let ((lsp-keymap-prefix "C-l"))
						 (lsp-enable-which-key-integration)))))
  :config
  ;;(setq lsp-auto-guess-root t)
  (define-key lsp-mode-map (kbd "C-l") lsp-command-map))


(defun lsp-go-install-save-hooks ()
  "Setup before save hooks for go mode."
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :bind ("<f7>" . lsp-ui-flycheck-list))

;; Language specific packages
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "pandoc"))

(use-package dockerfile-mode
  :ensure t
  )
(use-package go-mode
  :ensure t
  :hook (go-mode . (lambda() (setq tab-width 4))))

(use-package yaml-mode
  :ensure t)

;;; Web related stuff
(use-package web-mode
  :ensure t
  :mode ("\\.html$" . web-mode)
  :config
  (setq web-mode-enable-auto-closing t
	web-mode-enable-auto-quoting t))

(use-package emmet-mode
  :ensure t
  :hook (web-mode . emmet-mode))

(use-package simple-httpd
  :ensure t
  :config
  (setq httpd-port 5656))


(use-package impatient-mode
  :ensure t
  :config
  (defun start-impatient ()
  "Starts the `simple-httpd' server if it is not already running. Turns
on `impatient-mode' for the current buffer. Opens firefox to see it"
  (interactive)
  (unless (get-process "httpd")
    (message "starting httpd server...")
    (httpd-start))
  (impatient-mode)
  (start-process-shell-command
   ""
   nil
   (concat
    "firefox localhost:"
    (number-to-string httpd-port)
    "/imp/live/"
    (buffer-name)))))

(use-package js2-mode
  :ensure t
  :mode (("\\.js\\'" . js2-mode))
  :commands (j2-mode)
  :config
  (setq js-indent-level 4)
  (setq js2-indent-level 4))


;; Latex
;; auctex has weird interaction with use package
(use-package auctex
  :ensure t
  :defer t)
(add-hook 'latex-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'text-mode-hook 'turn-on-auto-fill)


(setq TeX-save-query nil)
(setq TeX-auto-save t)
(setq TeX-parse-self t)       ;enable document parsing
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)
(setq TeX-view-program-list (quote (("Okular" "okular --unique %o#src:%n%b"))))
;; Voir https://tex.stackexchange.com/questions/161797/how-to-configure-emacs-and-auctex-to-perform-forward-and-inverse-search

(org-babel-do-load-languages 'org-babel-load-languages
    '(
        (shell . t)
    )
)
;; Mac os stuff
;;On some external keyboard right and left keys are swapped
(setq mac-right-option-modifier 'super)
(setq ns-alternate-modifier 'super)
(setq ns-right-alternate-modifier 'none)
(setq mac-command-modifier 'meta)
(setq-default default-directory "~/")
(setq ns-pop-up-frames nil) 
(cd (getenv "HOME"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(protobuf-mode exec-path-from-shell yasnippet-snippets yaml-mode which-key web-mode use-package shell-pop ripgrep projectile org-superstar olivetti multiple-cursors magit lsp-ui js2-mode impatient-mode ido-vertical-mode go-mode flycheck expand-region emmet-mode dockerfile-mode diminish darktooth-theme crux company-quickhelp company-lsp auctex)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
