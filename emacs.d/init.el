(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("marmalade" . "https://marmalade-repo.org/packages/")
	("melpa" . "http://melpa.milkbox.net/packages/")
	("org" . "http://orgmode.org/elpa/")
	("melpa-stable" . "http://stable.melpa.org/packages/")))

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; Can't properly use emacs without evil.
(use-package evil
  :ensure t)
(use-package olivetti
  :ensure t)
(use-package feature-mode
  :ensure t)
(use-package reverse-im
  :ensure t)

(evil-mode)
(tool-bar-mode -1)
(menu-bar-mode -1)

(defun set-indentation ()
  "4 spaces per tab"
  (setq indent-tabs-mode nil)
  (setq tab-width 4))

(add-hook 'java-mode-hook 'auto-revert-mode)
(add-hook 'java-mode-hook 'set-indentation)

(add-hook 'python-mode-hook 'set-indentation)

(add-hook 'go-mode-hook '(lambda () (setq tab-width 4)))

(reverse-im-activate "russian-computer")

;;;;
;;; Org Mode
;;;
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
(use-package org
  :ensure org-plus-contrib)
;;
;; Standard key bindings
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-font-lock-mode 1)

(setq show-trailing-whitespace t)
(setq ac-auto-show-menu 0.3)
(setq browse-url-browser-function 'browse-url-generic browse-url-generic-program "luakit")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-search-module (quote evil-search))
 '(inhibit-startup-screen t)
 '(wc-modeline-format "%tw")
 '(org-agenda-files nil)
 '(org-trello-current-prefix-keybinding "C-c o" nil (org-trello)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-mode-line-clock ((t (:background "grey75" :foreground "red" :box (:line-width -1 :style released-button))))))

(modify-syntax-entry ?_ "w")

(use-package files)

(setq backup-directory-alist
      `(("." . ,(locate-user-emacs-file "backups"))))
(setq auto-save-file-name-transforms
      `((".*" ,(locate-user-emacs-file "backups") t)))
(setq auto-save-list-file-prefix
      (locate-user-emacs-file "backups/"))

(use-package wc-mode
  :ensure t)
(define-derived-mode my-writing-mode org-mode "my-writing"
  (olivetti-mode t)
  (wc-mode t))

(add-to-list 'default-frame-alist '(font . "freemono-12"))
(add-text-properties (point-min) (point-max)
		     '(line-spacing 0.25 line-height 1.25))

(load "~/.emacs.d/norang-org")
