(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "https://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")))

(package-initialize)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package cus-edit
  :after files
  :config
  (setf custom-file (locate-user-emacs-file "custom.el"))
  (add-hook 'after-init-hook
            (lambda () (load custom-file t))))

;; I can't properly use emacs without evil.
(use-package evil
  :ensure t
  :config
  (evil-mode)
  (fset 'evil-visual-update-x-selection 'ignore)
  :custom
  (evil-search-module (quote evil-search)))

(use-package vdiff
  :ensure t
  :custom
  (vdiff-truncate-lines t)
  :config
  (define-key vdiff-mode-map (kbd "C-c") vdiff-mode-prefix-map)
  (evil-define-key 'normal vdiff-mode-map "," vdiff-mode-prefix-map))

(use-package olivetti
  :ensure t)

(use-package feature-mode
  :ensure t)

(use-package ivy
  :ensure t
  :config
  (ivy-mode t))

(use-package reverse-im
  :ensure t
  :config
  (reverse-im-activate "russian-computer"))

(use-package org
  :ensure org-plus-contrib
  :bind (("\C-cl" . org-store-link)
         ("\C-ca" . org-agenda))
  :mode ("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode)
  :config
  (global-set-key "\C-cc" 'org-capture)
  :custom
  (org-agenda-span 1)
  (org-agenda-todo-list-sublevels nil)
  (org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
  (org-confirm-babel-evaluate nil)
  (org-enforce-todo-dependencies t)
  (org-extend-today-until 3)
  (org-hide-leading-stars nil)
  (org-log-into-drawer "LOGBOOK")
  (org-log-repeat nil)
  (org-outline-path-complete-in-steps nil)
  (org-refile-use-outline-path 'file)
  (org-sort-agenda-notime-is-late nil)
  (org-startup-indented t)
  (org-agenda-custom-commands
   (quote
    (("d" "Undated tasks" alltodo ""
      ((org-agenda-todo-ignore-with-date t))))))
  (org-capture-templates
   '(("h" "Add Hledger entry"
      plain (file (lambda ()
                    (concat *hledger-dir*
                            (format-time-string "%Y/%m.journal" (current-time)))))
      "%<%Y-%m-%d> %^{entry title}\n %?"
      :empty-lines 1
      :unnarrowed t)
     ("r" "Read later")
     ("rw" "Web page"
      entry (file"~/Dropbox/org/review.org")
      "* READ [[%x][%?]]\nSCHEDULED:%^t")))
  (org-refile-targets
   '((nil :maxlevel . 3)
     (org-agenda-files :maxlevel . 3)))
  :custom-face
  (org-mode-line-clock ((t (:background "grey75" :foreground "red" :box (:line-width -1 :style released-button))))))

(add-hook 'org-capture-mode-hook 'evil-insert-state)

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(setq org-todo-keyword-faces
      '(("FAILED" . org-warning)
        ("PLANNED" . "#806000")
	("MEETING" . "#806000")
	("PROJ" . "#101080")
	("CANCELLED" . "#101080")))

(use-package projectile
  :ensure t
  :custom
  (projectile-completion-system 'ivy)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (add-to-list 'projectile-globally-ignored-directories ".venv")
  (projectile-mode +1))

(use-package wc-mode
  :ensure t
  :custom
  (wc-modeline-format "%tw "))

(use-package files
  :custom
  (backup-directory-alist
   `(("." . ,(locate-user-emacs-file "backups"))))
  (auto-save-file-name-transforms
   `((".*" ,(locate-user-emacs-file "backups") t)))
  (auto-save-list-file-prefix
   (locate-user-emacs-file "backups/")))

(tool-bar-mode -1)
(menu-bar-mode -1)
(setq-default indent-tabs-mode nil)
(setq whitespace-style '(face tabs tab-mark trailing))
(global-whitespace-mode)

(add-hook 'go-mode-hook '(lambda ()
                           (setq indent-tabs-mode t)
                           (setq tab-width 4)))

(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))

(global-font-lock-mode 1)

(setq show-trailing-whitespace t)
(setq ac-auto-show-menu 0.3)
(setq browse-url-browser-function 'browse-url-generic browse-url-generic-program "google-chrome-stable")
(modify-syntax-entry ?_ "w")

(define-derived-mode my-writing-mode org-mode "my-writing"
  (setq olivetti-body-width 80)
  (olivetti-mode t)
  (wc-mode t))

(add-to-list 'default-frame-alist '(font . "monospace-12"))
(add-text-properties (point-min) (point-max)
                     '(line-spacing 0.25 line-height 1.25))

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(setq inhibit-startup-screen t)
(setq initial-scratch-message nil)

(let ((local-settings-file (locate-user-emacs-file "local_settings.el")))
  (if (file-exists-p local-settings-file)
      (load local-settings-file)))

(defun daily-page ()
  (interactive)
  (let ((header "#-*- Mode: my-writing -*-\n"))
    (with-current-buffer
        (find-file
         (concat *daily-page-dir*
                 (format-time-string "%Y-%m-%d.txt" (current-time))))
      (my-writing-mode)
      (if (> 2 (point-max-marker))
          (insert header))
      (narrow-to-region (1+ (length header)) (point-max-marker)))))

(let ((slime-helper-file (expand-file-name "~/quicklisp/slime-helper.el")))
  (if (file-exists-p slime-helper-file)
      (use-package slime
        :ensure t
        :init
        (load slime-helper-file)
        :config
        (setq inferior-lisp-program "/usr/bin/sbcl"))))

(rassq-delete-all 'change-log-mode auto-mode-alist)

(load (locate-user-emacs-file "org-subtask-reset.el"))
(require 'org-subtask-reset)

(global-auto-revert-mode t)

(load (locate-user-emacs-file "agile-results.el"))
