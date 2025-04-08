(load-directory "~/.emacs.d/lisp/")

(run-with-idle-timer
 1 nil
 (lambda ()
   (when (member "Hack" (font-family-list))
     (set-face-attribute 'default nil
                         :family "Hack"
                         :height 114
                         :weight 'light)
     (message "Font set to Hack"))))


(custom-set-faces
 '(default ((t (:family "Hack" :height 114 :weight light)))))

(add-hook 'emacs-startup-hook
          (lambda ()
            (load-theme 'manoj-dark t)))

(load-theme 'manoj-dark t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (ditaa . t)
   (dot .t)
   (emacs-lisp . t)
   (latex . t)
   (mermaid .t)
   (org . t)
   (python . t)
   (R . t)
   (shell . t)
   (sql .t)
   (sqlite . t)
   ))

(require 'ob-shell)
(require 'yaml-mode)

(defun org-babel-execute:yaml (body params)
  "Execute a block of YAML code with org-babel."
  (let ((temp-file (org-babel-temp-file "yaml-")))
    (with-temp-file temp-file
      (insert body))
    (org-babel-eval (format "cat %s" temp-file) "")))

(add-to-list 'org-src-lang-modes '("yaml" . yaml))

(defun endless/follow-tag-link (tag)
  "Display a list of TODO headlines with tag TAG.
With prefix argument, also display headlines without a TODO keyword."
  (org-tags-view current-prefix-arg tag))

(org-add-link-type
 "tag" 'endless/follow-tag-link)

(require 'essh)
(defun essh-sh-hook ()
  (define-key sh-mode-map "\C-c\C-r" 'pipe-region-to-shell)
  (define-key sh-mode-map "\C-c\C-b" 'pipe-buffer-to-shell)
  (define-key sh-mode-map "\C-c\C-j" 'pipe-line-to-shell)
  (define-key sh-mode-map "\C-c\C-n" 'pipe-line-to-shell-and-step)
  (define-key sh-mode-map "\C-c\C-f" 'pipe-function-to-shell)
  (define-key sh-mode-map "\C-c\C-d" 'shell-cd-current-directory))
(add-hook 'sh-mode-hook 'essh-sh-hook)

(add-hook 'sh-mode-hook 'flycheck-mode)
