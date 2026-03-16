;; ============================================================
;; AUTO-GENERATED — DO NOT EDIT DIRECTLY
;; Edits will be overwritten on next org-babel tangle.
;; 
;; Source:  /home/jeszyman/repos/basecamp/basecamp.org
;; Author:  Jeffrey Szymanski
;; Tangled: 2026-03-09 10:37:13
;; ============================================================

(require 'org)

(defun my-tbl-export (name)
"Search for table named `NAME` and export."
(interactive "s")
(show-all)
(let ((case-fold-search t))
  (if (search-forward-regexp (concat "#\\+NAME: +" name) nil t)
  (progn
    (next-line)
    (org-table-export (format "data/%s_table.csv" name) "orgtbl-to-csv")))))
