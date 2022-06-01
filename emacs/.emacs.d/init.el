;; -*- lexical-binding: t; -*-
;; File generato automaticamente, cambiare il emacs.org

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "!!! Emacs loader in %s with %d garbage collections."
		     (format "%.3f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)))

;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
;; Initialize use-package
(package-initialize)

(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))
;; Configurazione use-package
(eval-and-compile
   (setq use-package-always-ensure nil)
   (setq use-package-always-defer nil)
   (setq use-package-always-demand nil)
   (setq use-package-expand-minimally nil)
   (setq use-package-enable-imenu-support t)
   (setq use-package-compute-statistics nil)
   (setq use-package-hook-name-suffix nil))
(eval-when-compile
        (require 'use-package))

;;(use-package exec-path-from-shell
;;   :ensure t)
;;(when (daemonp)
;;  (exec-path-from-shell-initialize))
;;(when (memq window-system '(mac ns x))
;;  (exec-path-from-shell-initialize))
;;(dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE" "NIX_SSL_CERT_FILE" "NIX_PATH"))
;;  (add-to-list 'exec-path-from-shell-variables var))

(use-package no-littering
  :ensure
  :config
  (setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  )

(use-package gcmh
  :ensure t
  :custom
  (gcmh-mode 1)
  (gcmh-idle-delay 5)
  (gcmh-high-cons-threshold (* 16 1024 1024))
  (gc-cons-percentage 0.1))

(use-package auto-package-update
  :ensure t
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

(use-package emacs
  :init
  (set-face-attribute 'default nil :height 180) ;; Font
  (set-face-attribute 'fixed-pitch nil :height 180) ;; Font
  (set-face-attribute 'variable-pitch nil :height 180) ;; Font
  ;; Vertico setup
  (setq enable-recursive-minibuffers t)
  :config
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8-unix)
  (setq-default frame-title-format '("%b"))
  (global-set-key (kbd "<escape>") 'keyboard-escape-quit) ;; TODO Separare il keybind
  (global-display-line-numbers-mode)
  ;;(server-start)
  :custom
  (inhibit-startup-screen t)
  (initial-scratch-message "")
  (use-short-answer t)
  (read-process-output-max (* 1024 1024))
  ;;(user-emacs-directory "~/.cache/emacs")
  (tramp-default-method "ssh")
  (vc-follow-symlinks t)
  (delete-old-versions -1) ;; Avoid excessive backups
  (version-control t)
  (vc-make-backup-files t)
  )

;;;; Tema
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

;; Modeline
(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  )

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode-hook . rainbow-delimiters-mode))

(use-package undo-fu
  :after emacs
  :ensure
  :init
  )
(use-package undo-fu-session
  :after undo-fu
  :defer 1
  :ensure
  :config
  (setq undo-fu-session-file-limit 1024)
  (global-undo-fu-session-mode 1)
  )

;; Evil config
(use-package evil
  :ensure
  :after undo-fu
  :init	
  ;; Vim-like
  (general-evil-setup)
  ;; Spostamenti
  (setq evil-want-integration t) ;; TODO: Capire cosa fa
  (setq evil-want-keybinding nil) ;; TODO: Capire cosa fa
  (setq evil-want-C-i-jump t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-u-delete t)
  ;; Search
  (setq evil-search-module 'evil-search)
  ;; Indentazione
  (setq evil-shift-width 4) ;; Questo e' il default
  ;; Undo-Redo
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-mode 1)
  :custom
  (evil-vsplit-window-right t)
  )

(use-package evil-collection
  :ensure
  :after evil
  :init
  :custom
  (collection-setup-minibuffer t)
  (evil-collection-calendar-want-org-bindings t)
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (evil-collection-init)
  )

;; Folding
(use-package vimish-fold
  :ensure t
  :defer 2
  :after evil)

(use-package evil-vimish-fold
  :ensure t
  :after vimish-fold
  :init
  (setq evil-vimish-fold-mode-lighter " ")
  (setq evil-vimish-fold-target-modes '(prog-mode conf-mode text-mode))
  :config
  (global-evil-vimish-fold-mode)
 )

(use-package general 
  :ensure t
  :after evil
  :config
  (general-define-key
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"
    "c" '((lambda () (interactive) (find-file "~/.emacs.d/Emacs.org")) :which-key "Open Configuration")
    "a" '(org-agenda :which-key "Agenda")
    "SPC" '(find-file :which-key "Find file")
    ;; Buffers
    "b" '(:ignore t :which-key "Buffers")
    "be" '(eval-buffer :which-key "Eval")
    ;;"o" '(:ignore t :which-key "Org")
    ;;"oa" '(org-agenda :which-key "Agenda")
    ))

(use-package which-key
  :ensure t
  :after general
  :config
  (setq which-key-sort-order 'which-key-prefix-then-key-order)
  (which-key-mode)
)

(use-package ws-butler
  :ensure t
  :defer 1
  :custom
  (ws-butler-mode 1)
)

(use-package format-all
  :disabled
  :ensure
  :init
  ;; TODO Aggiungere un keybind per Black
  )

(use-package vertico
  :ensure t
  :custom
  (vertico-cycle t)
  ;;:custom-face
  ;;(vertico-current ((t (:background "#3a3f5a"))))
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Da attivare piano piano
;;(use-package orderless  :ensure t)
;;(use-package marginalia  :ensure t)
;;(use-package embark  :ensure t)
;;(use-package consult  :ensure t)
;;(use-package embark-consult  :ensure t)

(defun my/agenda-fetch ()
  ;; 1. Se voglio aggiungere una task ad un file nuovo?!
  ;; 2. Se non ne trova non parte l'agenda
  ;;(interactive)
  (split-string 
   (shell-command-to-string (concat "rg --type org '" locregex "' " org-agenda-base " -l "))
   "\n")
   )
(defun my/update-agenda (&rest _)
  ;;(interactive)
  (setq org-agenda-files (my/agenda-fetch)))

(use-package org
  :ensure t
  ;;:defer t
  :commands (org-capture org-agenda)
  :hook (org-mode . (lambda()
			(org-indent-mode)
			(fill-column 80)
			(auto-fill-mode 1)
			(org-src-tab-acts-natively t)
			(evil-auto-indent nil)))
  
  :config

  (setq string-todos '("TODO" "ACTIVE" "DONE" "HOLD" "CANCELED"))
  (setq locregex (string-join string-todos "|"))
  (setq org-agenda-base (getenv "ORG_PATH"))
  (setq org-agenda-files '(my/update-agenda))
  (setq org-todo-keywords
      '((sequence "TODO(t@)" "ACTIVE(a@)" "|" "DONE(d@)") ;; Generali
	(sequence  "|" "HOLD(h@)" "CANCELED(c@)")
	))
  (setq org-tag-alist
    '((:startgroup)
       ; Put mutually exclusive tags here
       (:endgroup)
       ("note" . ?n)
       ("idea" . ?i)))
  (advice-add 'org-agenda :before #'my/update-agenda)
  (advice-add 'org-todo-list :before #'my/update-agenda)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)))
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (setq org-src-tab-acts-natively t)
  (setq org-src-preserve-indentation t)

)

(use-package org-super-agenda
  :ensure t
  :after org
  :init
  (setq org-super-agenda-header-map (make-sparse-keymap))
  :hook (org-agenda-mode . org-super-agenda-mode)
  ;;:config
  ;;(org-super-agenda-mode)
  )

(use-package toc-org
  :ensure t
)

(use-package org-roam
  :ensure
  :demand t
  :defer 1
  :custom
  (org-roam-directory org-agenda-base)
  (org-roam-completion-everywhere t)
  (org-roam-completion-system 'default)
  (org-roam-dailies-directory "journals")
  :config

  (require 'org-roam-dailies)
  (org-roam-db-autosync-mode))

(use-package ledger-mode
  :ensure t
  :mode ("\\.dat\\'"
         "\\.ledger\\'")
  :config
  (add-hook 'ledger-mode-hook #'ledger-flymake-enable)
)

(use-package evil-ledger
  :ensure t
  :after ledger-mode
  :config
  (setq evil-ledger-sort-key "S")
  (add-hook 'ledger-mode-hook #'evil-ledger-mode))
