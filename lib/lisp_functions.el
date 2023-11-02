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
