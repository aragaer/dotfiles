(defun insert-3-planned-items ()
  (org-insert-heading) (insert "PLANNED") (org-do-demote)
  (org-insert-heading) (insert "PLANNED")
  (org-insert-heading) (insert "PLANNED")
  (forward-line -2) (end-of-line))

(defun monday-vision ()
  (interactive)
  (let ((file (cl-first (remove-if-not
			 (lambda (entry)
			   (string-match "agile-results" entry))
			 org-agenda-files)))
	(entry (format-time-string "Week %W (%Y)" (current-time)))
	(org-link-search-must-match-exact-headline t))
    (condition-case nil
	(org-open-link-from-string (format "[[file:%s::*%s]]" file entry))
      (error
       (with-current-buffer
	   (find-file file)
	 ; before first heading
	 (goto-char 0) (search-forward "*") (backward-char)
	 (org-insert-heading) (insert entry)
	 (org-insert-heading) (insert "Monday vision") (org-do-demote)
	 (insert-3-planned-items))))))

(defun daily-outcomes ()
  (interactive)
  (let ((file (cl-first (remove-if-not
			 (lambda (entry)
			   (string-match "agile-results" entry))
			 org-agenda-files)))
	(entry (format-time-string "Week %W (%Y)" (current-time)))
	(org-link-search-must-match-exact-headline t))
    (with-current-buffer
	(find-file file)
      (org-open-link-from-string (format "[[*%s]]" entry))
      (org-insert-heading-respect-content)
      (insert (format-time-string "Daily outcomes for %A" (current-time)))
      (org-do-demote)
      (insert-3-planned-items))))
