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

(defun load-directory (dir)
  "Add DIR and its subdirectories to the load-path and load all .el files."
  (when (file-directory-p dir)
    (let ((default-directory dir))
      (message "Adding %s and its subdirectories to load-path" dir)
      (normal-top-level-add-to-load-path '("."))
      (normal-top-level-add-subdirs-to-load-path))
    (dolist (file (directory-files dir t "\\.el\\'"))
      (load file nil 'nomessage))))


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

;;TEST
(setq server-name "server") ;; optional, default is "server"
(setq server-auth-dir (expand-file-name "server" user-emacs-directory)) ;; ~/.emacs.d/server
(require 'server)
(unless (server-running-p)
  (server-start))

;; Load all .el files in the config directory
(when (file-directory-p my-config-dir)
  (dolist (file (directory-files my-config-dir t "\\.el$"))
    (condition-case err
        (load (file-name-sans-extension file))
      (error (message "Error loading %s: %s" file err)))))

;; Load late configuration
(safe-load-file-if-exists "~/.emacs.d/load-last.el")

(require 'server)
(setq server-socket-dir (expand-file-name "~/.emacs.d/server"))
(unless (server-running-p) (server-start))

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
   '(conda magit yaml-mode web-mode vertico vc-use-package tree-sitter-langs snakemake-mode rainbow-delimiters ox-pandoc org-ql org-edna org-contrib orderless native-complete multi-vterm marginalia ivy htmlize helm-org-rifle helm-org flycheck expand-region exec-path-from-shell ess elpy corfu consult citar-embark blacken auctex)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Hack" :height 114 :weight light))))
 '(vterm-color-blue ((t (:foreground "#477EFC" :background "#477EFC")))))
