(require 'org)

(defun my-tbl-export (name)
"Search for table named `NAME` and export."
(interactive "s")
(show-all)
(let ((case-fold-search t))
  (if (search-forward-regexp (concat "#\\+NAME: +" name) nil t)
  (progn
    (next-line)
    (org-table-export (format "data/inputs/%s.csv" name) "orgtbl-to-csv")))))
(require 'org)

(defun my-tbl-export ()
"Search for table named `NAME` and export."
(interactive "s")
(show-all)
  (if (search-forward-regexp (concat("#\\+name: .*_input$") nil t)
  (progn
    (next-line)
    (org-table-export (format "/tmp/%s.csv" name) "orgtbl-to-csv"))))
(require 'org)

(defun my-tbl-export (name)
"Search for table named `NAME` and export."
(interactive "s")
(show-all)
(let ((case-fold-search t))
  (if (search-forward-regexp ("#\\+NAME: .*_name$") nil t)
  (progn
    (let (name (replace-match
    (next-line)
    (org-table-export (format "/tmp/%s.csv" name) "orgtbl-to-csv")))))
