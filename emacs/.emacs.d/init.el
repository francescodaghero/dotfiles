;; -*- lexical-binding: t; -*-
;; File generato automaticamente, cambiare il emacs.org

(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "!!! Emacs loader in %s with %d garbage collections."
		     (format "%.3f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)))
(setq use-package-verbose t)

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
      (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Always use straight to install on systems other than Linux
(setq straight-use-package-by-default (not (eq system-type 'gnu/linux)))

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)

;; Load the helper package for commands like `straight-x-clean-unused-repos'
(require 'straight-x)

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
  :disabled
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
  (setq-default indent-tabs-mode nil)

  )
(when (display-graphic-p)
  (set-frame-parameter (selected-frame) 'alpha '(90 . 90))
  (add-to-list 'default-frame-alist '(alpha . (90 . 90)))
  (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))

(eval-after-load 'tramp '(setenv "SHELL" "/bin/bash"))
(eval-after-load 'tramp '(setq doom-modeline-buffer-file-name-style 'file-name))

;; https://emacs.stackexchange.com/questions/37172/how-to-insert-special-characters-not-on-a-us-uk-keyboard
(when (memq window-system '(mac ns))
  (setq ns-command-modifier 'meta)
  (setq ns-alternate-modifier nil))

;;;; Tema
(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

;; Best addition ever
(use-package poke-line
  :ensure t
  :init
  (poke-line-global-mode 1)
  (setq-default poke-line-pokemon "gengar")
  )
;; Modeline
(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-buffer-file-name-style 'relative-to-project)
  (doom-modeline-lsp t)
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
  ;; Spostamenti
  (setq evil-want-integration t) ;; TODO: Capire cosa fa
  (setq evil-want-keybinding nil) ;; TODO: Capire cosa fa
  (setq evil-want-C-i-jump nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-u-delete t)
  (setq evil-respect-visual-line-mode t)
  ;; Search
  (setq evil-search-module 'evil-search)
  ;; Indentazione
  (setq evil-shift-width 4) ;; Questo e' il default
  ;; Undo-Redo
  (setq evil-undo-system 'undo-fu)
  ;; Vim-like
  :config
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-mode 1)
  :custom
  (evil-vsplit-window-right t)
  )

(use-package evil-collection
  :ensure
  :after evil
  :init
  :custom
  (evil-collection-outline-bind-tab-p nil)
  (collection-setup-minibuffer t)
  (evil-collection-calendar-want-org-bindings t)
  :config
  (evil-collection-init)
  )

(use-package evil-org
  :ensure
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
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
  (general-evil-setup t)
  (general-create-definer fd/supreme-leader
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  ;(general-define-key
  ; :states '(normal visual insert emacs)
  ; :prefix "SPC"
  ; :non-normal-prefix "C-SPC"
  ;  "c" '((lambda () (interactive) (find-file "~/.emacs.d/Emacs.org")) :which-key "Open Configuration")
  ;  "a" '(org-agenda :which-key "Agenda")
  ;  "SPC" '(find-file :which-key "Find file")
  ;  ;; Buffers
  ;  "b" '(:ignore t :which-key "Buffers")
  ;  "be" '(eval-buffer :which-key "Eval")
  ;  ;; Windows
  ;  "w" '(:ignore t :which-key "Windows")
  ;  "wv" '(split-window-vertically :which-key "Vertical Split")
  ;  "wh" '(split-window-horizontally :which-key "Horizontal Split")
  ;  ;; Org
  ;  "o" '(:ignore t :which-key "Org")
  ;  ;; Elfeed
  ;  "e" '(:ignore t :which-key "Elfeed")
  ;  "eo" '(elfeed :which-key "Open feed")
  ;  "eu" '(elfeed-update :which-key "Update feed"))
  )

(use-package which-key
  :ensure t
  :after general
  :diminish which-key-mode
  :config
  (setq which-key-sort-order 'which-key-prefix-then-key-order)
  (setq which-key-idle-delay 0.3)
  (which-key-mode)
)

(fd/supreme-leader
  "c" '((lambda () (interactive) (find-file "~/.emacs.d/Emacs.org")) :which-key "Open Configuration")
  "a" '(org-agenda :which-key "Agenda")
  "SPC" '(find-file :which-key "Find file")
  ;; Windows
  "w" '(:ignore t :which-key "Windows")
  "wv" '(split-window-vertically :which-key "Vertical Split")
  "wh" '(split-window-horizontally :which-key "Horizontal Split")
  "wk" '(split-window-horizontally :which-key "Kill windows")
  ;; Buffers
  "b" '(:ignore t :which-key "Buffers")
  "be" '(eval-buffer :which-key "Eval")
  ;; Roam!
  "o" '(:ignore t :which-key "Org-Roam")
  "oi" '(org-roam-node-insert :which-key "Insert node")
  "of" '(org-roam-node-find :which-key "Find node")
 )

(use-package ws-butler
  :hook ((text-mode . ws-butler-mode)
         (prog-mode . ws-butler-mode)))

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

(defun cst-org ()
      (org-indent-mode)
      (visual-line-mode 1)
)
(use-package org
    :defer t
    :init
    (add-hook 'org-mode-hook 'cst-org)
    :config

  (setq string-todos '("TODO" "ACTIVE" "DONE" "HOLD" "CANCELED"))
  (setq locregex (string-join string-todos "|"))
  (setq org-agenda-base (getenv "ORG_PATH"))
  (setq org-agenda-files '(my/update-agenda))
  (setq org-todo-keywords
      '((sequence "TODO(t@)" "ACTIVE(a@)" "|" "DONE(d@)") ;;   Generali
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

 (setq org-agenda-custom-commands nil)
 (setq org-agenda-custom-commands
      '(("ces" "Custom: Agenda and Emacs SOMEDAY [#A] items"
         ((org-ql-block '(todo "TODO")
                        ((org-ql-block-header "SOMEDAY :Emacs: High-priority")))
          (agenda)))))

  (org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
      ))
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("sh" . "src sh"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  ;;(setq org-src-tab-acts-natively t)
  (setq org-src-preserve-indentation t)

)

(use-package org-ql
  :straight (:files (:defaults (:exclude"helm-org-ql.el")))
  :defer t
  )

(use-package org-super-agenda
  :ensure t
  :after org
  :init
  (setq org-super-agenda-header-map (make-sparse-keymap))
  ;;:hook (org-agenda-mode . org-super-agenda-mode)
  :config
  (org-super-agenda-mode 1)
  (setq org-super-agenda-groups
        '(;; Each group has an implicit boolean OR operator between its selectors.
          (:name "Today"  ; Optionally specify section name
                 ;;:time-grid t  ; Items that appear on the time grid
                 :todo t)  ; Items that have this TODO keyword
          ))
  )

(use-package toc-org
  :ensure t
  :after org
)

(use-package default-text-scale
  :defer 1
  :config
  (default-text-scale-mode))

(use-package org-appear
  :ensure
  :hook (org-mode . org-appear-mode))

(use-package all-the-icons-dired
  :ensure) ;; Forse da limitare su terminale?

(use-package dired
  :ensure nil
  :straight nil
  :defer 1
  :commands (dired dired-jump)
  :config
  (when (eq system-type 'darwin)
    (setq insert-directory-program "/usr/local/bin/gls"))
  (setq dired-listing-switches "-agho --group-directories-first"
        dired-omit-files "^\\.[^.].*"
        dired-omit-verbose nil
        dired-hide-details-hide-symlink-targets nil
        delete-by-moving-to-trash t)

  (autoload 'dired-omit-mode "dired-x")

  (add-hook 'dired-load-hook
            (lambda ()
              (interactive)
              (dired-collapse)))

  (add-hook 'dired-mode-hook
            (lambda ()
              (interactive)
              (dired-omit-mode 1)
              (dired-hide-details-mode 1)
              (when (display-graphic-p)
                (all-the-icons-dired-mode 1))
              (hl-line-mode 1))))

(use-package dired-rainbow
    :ensure
    :defer 2
    :config
    (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
    (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
    (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
    (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
    (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
    (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
    (dired-rainbow-define media "#de751f" ("mp3" "mp4" "mkv" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
    (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
    (dired-rainbow-define log "#c17d11" ("log"))
    (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
    (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
    (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
    (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
    (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
    (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
    (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
    (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
    (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
    (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
    (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*"))

(use-package dired-single
  :ensure
  :defer t)
(use-package dired-ranger
  :ensure
  :defer t)
(use-package dired-collapse
  :ensure
  :defer t)

(evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "H" 'dired-omit-mode
    "l" 'dired-single-buffer
    "y" 'dired-ranger-copy
    "X" 'dired-ranger-move
    "p" 'dired-ranger-paste)

(use-package openwith
  :ensure
  :defer 1
  :if (display-graphic-p)
  :config
  (setq openwith-associations
        (list
          (list (openwith-make-extension-regexp
                '("mpg" "mpeg" "mp3" "mp4"
                  "avi" "wmv" "wav" "mov" "flv"
                  "ogm" "ogg" "mkv"))
                "open"
                '(file))
          (list (openwith-make-extension-regexp
                '("xbm" "pbm" "pgm" "ppm" "pnm"
                  "png" "gif" "bmp" "tif" "jpeg")) ;; Removed jpg because Telega was
                  ;; causing feh to be opened...
                  "open"
                  '(file))
          (list (openwith-make-extension-regexp
                '("pdf"))
                "open"
                '(file)))))

(use-package org-roam
  :straight t
  :after org
  :defer t
  :custom
  (org-roam-directory org-agenda-base)
  (org-roam-completion-everywhere t)
  (org-roam-completion-system 'default)
  (org-roam-dailies-directory "journals")
  (org-roam-capture-templates
   '(
     ("d" "default" plain "%?"
    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
    :unnarrowed t)
     )
   )

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



(use-package magit
  :ensure
  :commands (magit-status magit-get-current-branch)
  :init
  (fd/supreme-leader
    "g" '(:ignore t :which-key "Git")
    "gs" '(magit-status :which-key "Status")
    )
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package git-link
  :ensure
  :commands git-link
  :config
  (setq git-link-open-in-browser t))

(use-package git-gutter
  :straight git-gutter-fringe
  :diminish
  :hook ((text-mode . git-gutter-mode)
         (prog-mode . git-gutter-mode))
  :config
  (setq git-gutter:update-interval 2)
  (setq git-gutter:modified-sign "!")
  (setq git-gutter:added-sign "+")
  (setq git-gutter:deleted-sign "-")
  (set-face-foreground 'git-gutter:added "LightGreen")
  (set-face-foreground 'git-gutter:modified "LightGoldenrod")
  (set-face-foreground 'git-gutter:deleted "LightCoral"))

(use-package projectile
  :ensure t
  :defer t
  :diminish projectile-mode
  :config
  (projectile-mode)
)

(use-package lsp-mode
  :ensure t
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (python-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
    :commands lsp)
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package dap-mode
  :ensure t
  :after org
  )

(use-package anaconda-mode
  :ensure t
  :hook (python-mode-hook . anaconda-mode)
  :hook (python-mode-hook . anaconda-eldoc-mode)
)

(use-package poetry
 :ensure t
 :hook (python-mode-hook .poetry-tracking-mode))

(use-package elfeed
  :ensure t
  :commands elfeed
  :init
  (fd/supreme-leader
    "e" '(:ignore t :which-key "Elfeed")
    "eo" '(elfeed :which-key "Feeds")
    "eu" '(elfeed-update :which-key "Update")
    )
  :config
  (setq elfeed-feeds
	'(("http://export.arxiv.org/api/query?search_query=cat:cs.LG&start=0&max_results=300&sortBy=submittedDate&sortOrder=descending")
      ("[[http://export.arxiv.org/api/query?search_query=cat:stat.TH&start=0&max_results=300&sortBy=submittedDate&sortOrder=descending]]")))
;;  (setq elfeed-feeds
	;;'("http://export.arxiv.org/api/query?search_query=cat:cs.LG"
      ;;"[[http://export.arxiv.org/api/query?search_query=cat:stat.TH]]")
	;;)
  ;;(setq-default elfeed-search-filter "=start=0 max_results=300 =sortBy=submittedDate =sortOrder=descending #30 ")
  (require 'elfeed-arxiv-aspect)
  (require 'elfeed-arxiv-excerpts)

)

(use-package elfeed-score
  :ensure t
  :after elfeed
  :config
  (elfeed-score-load-score-file "~/.emacs.d/lisp/elfeed/elfeed.score") ; See the elfeed-score documentation for the score file syntax
  (setq elfeed-score-serde-score-file "elfeed.score")
  (elfeed-score-enable)
  ;;(define-key elfeed-search-mode-map "=" elfeed-score-map)
  )
