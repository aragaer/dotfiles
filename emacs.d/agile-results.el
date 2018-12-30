(defconst *my-agile-results-file* "~/Dropbox/org/agile-results.org")

(defun agile-append ()
  (setq hd (format-time-string "Week %W (%Y)" (current-time)))
  (goto-char (point-min))
  (if (re-search-forward
       (format org-complex-heading-regexp-format (regexp-quote hd))
       nil t)
      (goto-char (point-at-bol))
    (search-forward "*")
    (while (org-up-heading-safe))
    (org-insert-heading) (insert hd)
    (backward-char)))

(defun add-agile-result-templates()
  (add-to-list 'org-capture-templates '("a" "Agile results templates"))
  (add-to-list 'org-capture-templates '("am" "Monday Vision" entry (file+function
                                                                    *my-agile-results-file*
                                                                    agile-append)
                                        (file "~/Dropbox/org/templates/monday-vision.org")
                                        :unnarrowed t))
  (add-to-list 'org-capture-templates '("ad" "Daily Outcomes" entry (file+function
                                                                     *my-agile-results-file*
                                                                     agile-append)
                                        (file "~/Dropbox/org/templates/daily-outcomes.org")
                                        :unnarrowed t))
  (add-to-list 'org-capture-templates '("af" "Friday Reflection" entry (file+function
                                                                        *my-agile-results-file*
                                                                        agile-append)
                                        (file "~/Dropbox/org/templates/friday-reflection.org")
                                        :unnarrowed t)))

(add-agile-result-templates)
