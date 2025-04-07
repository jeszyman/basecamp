;; defun org-execute-named-block-anywhere
;; :PROPERTIES:
;; :ID:       5ace2dd4-9dfb-41b9-a7ec-5ca32aaf94ae
;; :END:
;; https://claude.ai/chat/c12f61f1-0d93-4658-8d52-be64f45a12e0

(defun org-execute-named-block-anywhere (file block-name)
  "Execute a named source block from specified org file.
FILE is the path to the org file containing the block.
BLOCK-NAME is the name of the block to execute."
  (interactive "fFile: \nsBlock name: ")
  (let* ((original-buffer (current-buffer))
         (target-buffer (or (get-file-buffer file)
                           (find-file-noselect file)))
         info)
    (with-current-buffer target-buffer
      (save-excursion
        (goto-char (point-min))
        (if (re-search-forward (concat "^[ \t]*#\\+name:[ \t]*" 
                                      (regexp-quote block-name) "[ \t]*$") nil t)
            (progn
              (setq info (org-babel-get-src-block-info t))
              (org-babel-execute-src-block nil info))
          (error "Named block %s not found" block-name))))
    (when (and (not (eq target-buffer original-buffer))
               (not (buffer-modified-p target-buffer))
               (not (get-file-buffer file)))
      (kill-buffer target-buffer))))
;; find-duplicate-lines


(defun find-duplicate-lines ()
  "Find and list all duplicate lines in the current buffer."
  (interactive)
  (let ((lines (make-hash-table :test 'equal))
        duplicates)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (if (gethash line lines)
              (push line duplicates)
            (puthash line t lines)))
        (forward-line 1)))
    (if duplicates
        (progn
          (with-output-to-temp-buffer "*Duplicate Lines*"
            (dolist (line (delete-dups duplicates))
              (princ line)
              (terpri)))
          (message "Duplicate lines found and listed in *Duplicate Lines* buffer."))
      (message "No duplicate lines found."))))
;; run-latex-at-point
;; :PROPERTIES:
;; :ID:       3975a173-ac71-4c02-b032-ef215329ca59
;; :END:
;; - https://chatgpt.com/c/66f5c68c-8c4c-8005-b7c6-14665eadb848
;; - https://chatgpt.com/c/66f95df4-0dfc-8005-9e7b-c3f15c1ca924
;; - [[id:9fa465f4-68f7-45c5-966e-80084e2c8a05][emacs_export_header_to_latex.py]]

(defun run-latex-at-point ()
  "Extract the ID from the Org properties and run emacs-save and emacs-latex for the current file."
  (interactive)
  (let* ((id (org-entry-get (point) "ID"))
         (current-file (buffer-file-name))
         (output-buffer "*Latex Output*"))
    (if (not id)
        (message "No ID property found!")
      (with-current-buffer (get-buffer-create output-buffer)
        (erase-buffer))
      (let ((command (concat "source ~/repos/basecamp/lib/basecamp_function.sh && emacs-save && /usr/bin/python3 ~/repos/latex/scripts/emacs_export_header_to_latex.py --org_file \""
                             current-file "\" --node_id \"" id "\"")))
        ;; Start the process without a process name
        (start-process-shell-command "latex-process" output-buffer command)
        ;; Display the output buffer
        (pop-to-buffer output-buffer)))))
;; export-org-table-by-name-to-csv

(defun export-org-table-by-name-to-csv (org-file table-name csv-file)
  "Export an org table with TABLE-NAME in ORG-FILE to CSV-FILE."
  (with-current-buffer (find-file-noselect org-file)
    (goto-char (point-min))
    (when (re-search-forward (concat "^#\\+NAME: " (regexp-quote table-name)) nil t)
      (forward-line)
      (org-table-export csv-file "orgtbl-to-csv"))))
(defun open-vterm-in-new-frame ()
  "Open a new vterm buffer in a new frame."
  (interactive)
  (let* ((current-prefix-arg '(4))  ; simulate pressing C-u
         (timestamp (format-time-string "%Y%m%d%H%M%S")) ; timestamp generated
         (buffer-name (concat "TEST-" timestamp)) ; buffer-name created
         (new-frame (make-frame))) ; create new frame
    (select-frame-set-input-focus new-frame) ; focus the new frame
    (call-interactively 'vterm)    ; execute M-x vterm
    ;; Rename the buffer.
    (rename-buffer buffer-name t)))  ; The t parameter forces the renaming even if a buffer named "TEST-<timestamp>" already exists.
