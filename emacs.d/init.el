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

(defvar *packages-need-refresh* t)

(defun install-if-not-yet (package)
  (when (not (package-installed-p package))
    (when *packages-need-refresh*
      (package-refresh-contents)
      (setf *packages-need-refresh* nil))
    (package-install package)))

;; Can't properly use emacs without evil.
(install-if-not-yet 'evil)
(install-if-not-yet 'olivetti)
(install-if-not-yet 'feature-mode)
(install-if-not-yet 'org-plus-contrib)
(install-if-not-yet 'reverse-im)

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

(require 'reverse-im)
(reverse-im-activate "russian-computer")

;;;;
;;; Org Mode
;;;
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
(require 'org)
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
 '(org-trello-current-prefix-keybinding "C-c o" nil (org-trello)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-mode-line-clock ((t (:background "grey75" :foreground "red" :box (:line-width -1 :style released-button)))) t)
 )

(modify-syntax-entry ?_ "w")

(require 'files)

(setq backup-directory-alist
      `(("." . ,(locate-user-emacs-file "backups"))))
(setq auto-save-file-name-transforms
      `((".*" ,(locate-user-emacs-file "backups") t)))
(setq auto-save-list-file-prefix
      (locate-user-emacs-file "backups/"))

(require 'wc-mode)
(define-derived-mode my-writing-mode org-mode "my-writing"
  (olivetti-mode t)
  (wc-mode t))

(load "~/.emacs.d/norang-org")
