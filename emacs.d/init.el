(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("nongnu" . "http://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")))

(package-initialize)

(defun aragaer--load-config (name)
  "Load the Emacs Lisp file tangled from an Org file called NAME and
placed in the user emacs directory, if that tangled code is still
actual. Otherwise, resort to `org-babel-load-file'."
  (let* ((org-file (expand-file-name name user-emacs-directory))
         (elisp-file (concat (file-name-sans-extension org-file) ".el")))
    (when (file-exists-p org-file)
      (if (file-newer-than-file-p elisp-file org-file)
          (load-file elisp-file)
        (org-babel-load-file org-file)))))

(aragaer--load-config "myinit.org")

