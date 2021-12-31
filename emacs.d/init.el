(defconst old-gc-cons-threshold gc-cons-threshold)
(setq gc-cons-threshold most-positive-fixnum)
(add-hook
 'emacs-startup-hook
 (lambda ()
   (let ((startup-time (float-time (time-subtract after-init-time before-init-time))))
     (message "Emacs ready in %.2f seconds with %d garbage collections." startup-time gcs-done))
   (setq gc-cons-threshold old-gc-cons-threshold)))

(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("nongnu" . "http://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

(package-initialize)

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(org-babel-load-file (expand-file-name (locate-user-emacs-file "myinit.org")))
