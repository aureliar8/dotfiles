;; emacs.el --- fichier de configuration d'Emacs
;;; Commentary:
;; Fichier de configuration d'Emacs (Editeur de texte)
;; Le fichier doit s'appeler ~/.emacs ou ~/.emacs.el
;; Ensimag 2018.

;; Auteurs :
;;  - Matthieu Moy <Matthieu.Moy@imag.fr>
;;  - Mounié Grégory <Gregory.Mounie@imag.fr>
;; et l'équipe du Stage Unix de rentrée

;; Ceci est le fichier de configuration d'Emacs. Il est écrit dans un
;; langage appelé Emacs-lisp, mais rassurez-vous, vous n'avez pas
;; besoin de le connaitre pour changer votre configuration.
;;
;; Pour contribuer:
;; https://gitlab.ensimag.fr/emacs/conf
;;
;; Tout ce qui est précédé par un point-virgule est un commentaire.

;; Aurelien: Ce setup requiert quelques package sur le systeme. Notament
;; Linters:
;; shellcheck, aspell-fr , flake8, pylint
;; et
;; texlive-full, python setuptools, clang
;;; Code:

;; Nécessaire pour pouvoir configurer les packages additionnels
;; (setq package-check-signature nil)
(require 'cl)
(require 'package)
;; Commenter pour enlever l'archive melpa à la liste des packages
;; disponibles :
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)
;; auto-installation au premier lancement
;; utiliser l'interface "M-X list-packages" sinon
(defvar required-packages '(magit flycheck rust-mode company company-quickhelp yasnippet
								  yasnippet-snippets smex ido-vertical-mode yaml-mode
								  company-anaconda multiple-cursors projectile
								  which-key expand-region material-theme auctex)
  "A list of packages to ensure are installed at launch.")

(defun required-packages-installed-p ()
   "Check if all packages in `required-packages' are installed."
   (every #'package-installed-p required-packages))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(unless (required-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs (Ensimag/Personnal auto-install) is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p required-packages)
    (when (not (package-installed-p p))
      (package-install p))))

;; Locate backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; Vérifications en cours de frappe.
;; Si besoin, installer flycheck : http://www.flycheck.org/
(add-hook 'after-init-hook #'global-flycheck-mode)
;; Linter use python3
(add-hook 'python-mode-hook
		  (lambda ()
			(setq-local flycheck-python-pylint-executable "pylint3")
			(setq-local flycheck-python-pycompile-executable "python3")
			(flycheck-add-next-checker 'python-flake8 'python-pylint)))


(setq python-shell-interpreter "python3")



;; dark theme
(load-theme 'tango-dark)
;; (load-theme 'material)
;; Other : zerodark

;; support python
(setq python-indent-def-block-scale 1)
(add-hook 'python-mode-hook 'anaconda-mode)
(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
(require 'anaconda-mode)

;; support de C
;; ede et ide
; (global-ede-mode)
(setq stack-trace-on-error t)
(electric-pair-mode)
;; (semantic-mode )
(setq c-default-style "linux")
(setq c-basic-offset 4)
(setq-default tab-width 4)
;(require 'semantic)
;(setq global-semantic-decoration-mode t)
;(setq global-semantic-highlight-func-mode t)
;(setq global-semantic-idle-completions-mode t nil (semantic/idle))
;(setq global-semantic-idle-summary-mode t)
;(setq global-semantic-stickyfunc-mode t)
(setq inhibit-startup-screen t)


;; Correspondance des parenthèses :
;; Avec ceci, positionnez le curseur sur une parenthèse ouvrante ou
;; une parenthèse fermante, Emacs met en couleur la paire de
;; parenthèses.
(show-paren-mode 1)

;; Afficher les numéros de lignes dans la mode-line (barre du bas de
;; fenêtre) :
(line-number-mode t)
(column-number-mode t)

;; Faire clignoter l'écran au lieu de faire « beep ». Sympa en salle
;; machine !
(setq visible-bell t)

;; Pour les curieux ...

;; La suite de ce fichier ne contient que des commentaires ! Ce sont
;; des suggestions pour vous constituer votre .emacs.el. Décommentez
;; les lignes de configuration (i.e. supprimer les ";") puis relancez
;; Emacs pour les activer.

;; Ne pas afficher le message d'accueil
(setq inhibit-splash-screen t)

;; Visionner la région (aka sélection) courante :
;; (transient-mark-mode t)

;; Des raccourcis claviers et une selection comme sous Windows
;; (C-c/C-x/C-v pour copier coller, ...)
;(cua-mode 1)
;; Sauver avec Control-s :
;(global-set-key [(C s)] 'save-buffer)

;; Correction orthographique :
;; Be sure to have or install it with :
;; $ apt intall aspell-fr
(ispell-change-dictionary "francais" t)
;; (ispell-change-dictionary "en_GB" t)

;; Souligner les mots incorrects en mode LaTeX
(add-hook 'latex-mode-hook 'flyspell-mode)
;; Idem mais compatibilité Auctex
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
;; Souligner les mots incorrects en mode org
;; (add-hook 'org-mode-hook 'flyspell-mode)
(add-to-list 'ispell-skip-region-alist '("^#+BEGIN_SRC" . "^#+END_SRC"))
;; fontify code in code blocks and dont ask confimation
;; for execution
(setq org-src-fontify-natively t
    org-src-tab-acts-natively t
    org-confirm-babel-evaluate nil
    org-edit-src-content-indentation 0)

;; Allow execution of python and shell code (and elisp by default)
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (python . t)
   (sh . t)
   ;; Include other languages here...
   ))

;; Se limiter à des lignes de 80 caractères dans les modes textes (y
;; compris le mode LaTeX) :
;; cf. http://www-verimag.imag.fr/~moy/emacs/#autofill
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Activation de Ido
;; Change le comportement de la selection de fichiers (C-x C-f)
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
;; Ne pas tenter de trouver des fichier ailleurs quand on tente
;; d'en creer un nouveau
(setq ido-auto-merge-work-directories-length -1)
(ido-mode 1)
;;Diplay ido verticaly
(require 'ido-vertical-mode)
(ido-vertical-mode 1)


;; Dans la même série : changer le comportement de la complétion.
(icomplete-mode)

;; Pour une interface graphique un peu dépouillée
;; (menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
;(blink-cursor-mode -1)

;; Définir des touches pour se déplacer rapidement :
;; Aller à la parenthèse ouvrante correspondante :
;(global-set-key [M-right] 'forward-sexp)
;; Aller à la parenthèse Fermante correspondante :
;(global-set-key [M-left] 'backward-sexp)

;; pour démarrer un serveur
;(server-start)

;; Compiler avec M-f9, recompiler (avec la même commande de
;; compilation) avec f9.
(global-set-key [M-f9]   'compile)
(global-set-key [f9]     'recompile)

;; Se souvenir des derniers fichiers ouverts
;; (setq recentf-menu-path nil)N
;; (setq recentf-menu-title "Recentf")
;; (recentf-mode 1)

;; Un menu pour naviguer entre les fonctions dans un fichier (Python,
;; Ada, C, ...). On l'ajoute pour tous les modes ayant de la
;; coloration syntaxique :
;(defun try-to-add-imenu ()
;  (condition-case nil (imenu-add-to-menubar "Navigation") (error nil)))
;(add-hook 'font-lock-mode-hook 'try-to-add-imenu)


;; et maintenant ?
;; Si vous avez lu jusqu'ici, vous aurez probablement envie d'aller
;; plus loin. Commencez-donc par la page EnsiWiki sur le .emacs :
;;   http://ensiwiki.ensimag.fr/index.php/Dot_Emacs

;; Ajout Perso
;; Liste des buffers ouverts plus agréable
(global-set-key (kbd "C-x C-b") 'electric-buffer-list)

;;nom du buffer en cours dans la barre de titre
(setq frame-title-format "%b - Emacs")
(setq icon-title-format "%b - Emacs")

;;toujours s'assurer que le fichier finit par une newline
(setq require-final-newline 't)
; ido mode in command invokation (Or cf smex)
(global-set-key
     "\M-x"
     (lambda ()
       (interactive)
       (call-interactively
        (intern
         (ido-completing-read
          "M-x "
          (all-completions "" obarray 'commandp))))))

;;(require 'smex) ; Not needed if you use package.el
;; (smex-initialize) ; Can be omitted. This might cause a (minimal) delay
                    ; when Smex is auto-initialized on its first run.

;;(global-set-key (kbd "M-X") 'smex-major-mode-commands)
  ;; This is your old M-x.
;; (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


;; Add company-mode
(add-hook 'prog-mode-hook 'global-company-mode)
;;Tweak in company
(setq company-minimum-prefix-length 1 )
(setq company-idle-delay 0.2 )
(setq company-tooltip-align-annotations t)
;; company quickhelp
(company-quickhelp-mode)

(setq company-quickhelp-delay 0)
;; Allow dabbrev to match camelCase words properly
(setq company-dabbrev-downcase nil)
;; Don't auto complete numbers
(setq company-dabbrev-char-regexp "[A-z:-]")
;; Change la couleur du prompt companyn, sinon changer de theme
;; (require 'color)
;; (let ((bg (face-attribute 'default :background)))
;;   (custom-set-faces
;;    `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
;;    `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
;;    `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
;;    `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
;;    `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))

(add-to-list 'company-backends 'company-anaconda)


;; Easy shorcut
(require 'which-key)
(which-key-mode)
;;Add yasnippet
(require 'yasnippet)
(yas-global-mode 1)

;; Add markdown mode
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))

;; Binding for quick acces to magit status
(global-set-key (kbd "C-x g") 'magit-status)

;;Add yaml mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; Keep emacs and host clipboard synchronized
(setq select-enable-clipboard t)
;; Ansii color in compilation buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (read-only-mode)
  (ansi-color-apply-on-region compilation-filter-start  (point))
  (read-only-mode))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;;Multiple cursor
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Projectile : project wide compilation / commands
(require 'projectile)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)

;; Auctex
(setq TeX-save-query nil)
(setq TeX-auto-save t)
(setq TeX-parse-self t)       ;enable document parsing
(setq reftex-plug-into-AUCTeX t)
(setq TeX-PDF-mode t)

; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(add-to-list 'tramp-default-proxies-alist
                 '("192.168.1.1" nil "/ssh:root@192.168.25.68:"))

;; Disable font size augment in title and section
;; see : font-latex-slide-title-face and font-latex-fontify-....


;; Shortcut to revert buffer
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
;; Never have trailing whitespace.
;; May be annoying on legacy code. See ws-butler ??
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; footer

(provide '.emacs)
;; emacs.el ends here.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
	("a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "1068ae7acf99967cc322831589497fee6fb430490147ca12ca7dd3e38d9b552a" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "5f27195e3f4b85ac50c1e2fac080f0dd6535440891c54fcfa62cdcefedf56b1b" default)))
 '(font-latex-fontify-sectioning 1)
 '(package-selected-packages
   (quote
	(go-mode material-theme company-anaconda anaconda-mode dockerfile-mode json-mode multiple-cursors zerodark-theme company-quickhelp which-key ido-vertical-mode yaml-mode markdown-mode yasnippet-snippets spacemacs-theme monokai-theme company rust-mode flycheck magit)))
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-latex-slide-title-face ((t (:inherit (variable-pitch font-lock-type-face) :weight bold :height 1.0)))))
