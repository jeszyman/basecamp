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