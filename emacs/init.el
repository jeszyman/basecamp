;;-*- mode: elisp -*-

;; Package Management Setup

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Ensure 'use-package' is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(defun add-to-load-path-if-exists (dir)
  "Add DIR and its subdirectories to the Emacs load-path if DIR exists."
  (if (file-directory-p dir)
      (let ((default-directory dir))
        (message "Adding %s and its subdirectories to load-path" dir)
        (normal-top-level-add-to-load-path '("."))
        (normal-top-level-add-subdirs-to-load-path))
    (message "Directory %s does not exist, skipping." dir)))

(add-to-load-path-if-exists "~/.emacs.d/lisp/")

;; Function to safely load a file if it exists
(defun safe-load-file-if-exists (filepath)
  "Safely load the Emacs Lisp file at FILEPATH if it exists."
  (when (file-exists-p filepath)
    (condition-case err
        (load (file-name-sans-extension filepath))
      (error (message "Error loading %s: %s" filepath err)))))

;; Load early configuration
(safe-load-file-if-exists "~/.emacs.d/load-first.el")

;; Define the path to your configuration directory
(defvar my-config-dir "~/.emacs.d/config/"
  "Directory containing personal Emacs configuration files.")

;; Load all .el files in the config directory
(when (file-directory-p my-config-dir)
  (dolist (file (directory-files my-config-dir t "\\.el$"))
    (condition-case err
        (load (file-name-sans-extension file))
      (error (message "Error loading %s: %s" file err)))))

;; Load late configuration
(safe-load-file-if-exists "~/.emacs.d/load-last.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ess-R-font-lock-keywords
   '((ess-R-fl-keyword:modifiers . t)
     (ess-R-fl-keyword:fun-defs . t)
     (ess-R-fl-keyword:keywords . t)
     (ess-R-fl-keyword:assign-ops . t)
     (ess-R-fl-keyword:constants . t)
     (ess-fl-keyword:fun-calls . t)
     (ess-fl-keyword:numbers . t)
     (ess-fl-keyword:operators . t)
     (ess-fl-keyword:delimiters . t)
     (ess-fl-keyword:= . t)
     (ess-R-fl-keyword:F&T . t)
     (ess-R-fl-keyword:%op% . t)))
 '(package-selected-packages
   '(key-chord yaml-mode yaml ws-butler web-mode vertico use-package undo-tree tree-sitter-langs sqlite3 snakemake-mode smex smart-mode-line rainbow-mode rainbow-delimiters pydoc pos-tip pdf-tools org-sql org-ros org-ref org-notify org-gcal org-contrib org-anki org-alert orderless openwith ob-mermaid ob-async oauth2 native-complete mustache multiple-cursors multi-vterm move-text marginalia magit lsp-ui lispy ivy-yasnippet ivy-xref ivy-hydra ivy-bibtex ivy-avy ibuffer-projectile helm-org-rifle helm-org-ql helm-lsp helm-bibtex gptel google-this git-messenger ggtags flyspell-correct-ivy flycheck-eglot flx expand-region exec-path-from-shell esup ess emacsql-sqlite emacsql elpy ellama elfeed drag-stuff diminish dashboard counsel-projectile consult conda button-lock blacken auctex anki-editor anki-connect aggressive-indent)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Hack" :height 114 :weight light))))
 '(org-scheduled-today ((t (:foreground "cyan"))))
 '(vterm-color-blue ((t (:foreground "#477EFC" :background "#477EFC")))))
