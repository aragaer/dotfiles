;;; org-subtask-reset.el --- functions to reset subtasks when repeating

;; Copyright (C) 2014 Jez Cope

;; Author: Jez Cope (http://github.com/jezcope)
;; Version: 1.0
;; Keywords: org, subtasks
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides functions to reset subtasks (i.e. those defined
;; with a TODO keyword rather than '[ ]'-style checkboxes) when the
;; parent task is completed - particularly useful for projects (tasks
;; with subtasks) that repeat.
;;
;; This code is heavily based on org-checklist.el, which can be found
;; in the contrib section of the org-mode distribution.
;;
;;; Usage:
;;
;; (require 'org-subtask-reset)
;;
;; Set the RESET_SUBTASKS property to t in any projects that should
;; have their subtasks reset upon project completion.
;;
;;; Code:
(setq org-default-properties (cons "RESET_SUBTASKS" org-default-properties))

(defun org-reset-subtask-state-subtree ()
  "Reset all subtasks in an entry subtree."
  (interactive "*")
  (if (org-before-first-heading-p)
      (error "Not inside a tree")
    (save-excursion
      (save-restriction
	(org-narrow-to-subtree)
	(org-show-subtree)
	(goto-char (point-min))
        (beginning-of-line 2)
        (narrow-to-region (point) (point-max))
        (org-map-entries
         '(when (member (org-get-todo-state) org-done-keywords)
            (org-todo (car org-todo-keywords))))
        ))))

(defun org-reset-subtask-state-maybe ()
  "Reset all subtasks in an entry if the `RESET_SUBTASKS' property is set"
  (interactive "*")
  (if (org-entry-get (point) "RESET_SUBTASKS")
      (org-reset-subtask-state-subtree)))

(defun org-subtask-reset ()
  (when (member org-state org-done-keywords) ;; org-state dynamically bound in org.el/org-todo
    (org-reset-subtask-state-maybe)
    (org-update-statistics-cookies t)))

(add-hook 'org-after-todo-state-change-hook 'org-subtask-reset)

(provide 'org-subtask-reset)
