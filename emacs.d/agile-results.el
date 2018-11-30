(defconst *my-agile-results-file* "~/Dropbox/org/agile-results.org")

(defun agile-result-goto-week ()
  (setq hd (format-time-string "Week %W (%Y)" (current-time)))
  (goto-char (point-min))
  (if (re-search-forward
       (format org-complex-heading-regexp-format (regexp-quote hd))
       nil t)
      (goto-char (point-at-bol))
    (goto-char (point-min))
    (search-forward "*") (backward-char)
    (org-insert-heading) (insert hd)))

(add-to-list 'org-capture-templates '("a" "Agile results templates"))
(add-to-list 'org-capture-templates '("am" "Monday Vision" entry (file+function
                                                                  *my-agile-results-file*
                                                                  agile-result-goto-week)
                                      (file "~/Dropbox/org/templates/monday-vision.org")))
(add-to-list 'org-capture-templates '("ad" "Daily Outcomes" entry (file+function
                                                                   *my-agile-results-file*
                                                                   agile-result-goto-week)
                                      (file "~/Dropbox/org/templates/daily-outcomes.org")))
(add-to-list 'org-capture-templates '("af" "Friday Reflection" entry (file+function
                                                                      *my-agile-results-file*
                                                                      agile-result-goto-week)
                                      (file "~/Dropbox/org/templates/friday-reflection.org")))
