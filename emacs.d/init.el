(require 'package)

(setq package-archives
  '(("gnu"         . "http://elpa.gnu.org/packages/")
    ("marmalade"   . "https://marmalade-repo.org/packages/")
    ("melpa"       . "http://melpa.milkbox.net/packages/")))

(package-initialize)

(defun install-if-not-yet (package)
  (when (not (package-installed-p package))
    (package-refresh-contents)
    (package-install package)))

(install-if-not-yet 'evil)
(install-if-not-yet 'olivetti)
(install-if-not-yet 'feature-mode)

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

(require 'org-trello)

(setq show-trailing-whitespace t)
(setq browse-url-browser-function 'browse-url-generic browse-url-generic-program "luakit")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-search-module (quote evil-search))
 '(inhibit-startup-screen t)

(modify-syntax-entry ?_ "w")
