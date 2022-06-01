;;; -*- lexical-binding: t; -*-
; File generato automaticamente da emacs.org

(setq user-emacs-directory "~/.cache/emacs")
(setq package-user-dir "~/.cache/emacs/packages")

(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
	  (expand-file-name  "var/eln-cache/" user-emacs-directory))))

;; Meno warnings
(setq comp-async-report-warnings-errors nil)

;; Garbage collector
(setq gc-cons-threshold most-positive-fixnum
    gc-cons-percentage 0.6)

;; UI
(push '(menu-bar-lines .0) default-frame-alist)
(push '(tool-bar-lines .0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(mode-line-format . 0) default-frame-alist)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

;; Fast-startup
(setq package-enable-at-startup nil)
(setq package-quickstart t)
;;(setq frame-inhibit-implied-resize t)
(advice-add #'x-apply-session-resources :override #'ignore)
