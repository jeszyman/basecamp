;;-*- mode: elisp -*-

;; Package Management Setup

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(defun add-to-load-path-if-exists (dir)
  "Add DIR and its subdirectories to the Emacs load-path if DIR exists."
  (if (file-directory-p dir)
      (let ((default-directory dir))
	(message "Adding %s and its subdirectories to load-path" dir)
	(normal-top-level-add-to-load-path '("."))
	(normal-top-level-add-subdirs-to-load-path))
    (message "Directory %s does not exist, skipping." dir)))

(add-to-load-path-if-exists "~/.emacs.d/lisp/")
(add-to-load-path-if-exists "~/.emacs.d/elpa/")

(require 'use-package)
(setq use-package-always-ensure t)

(defun load-file-if-exists (file)
  (if (file-exists-p file)
      (progn
	(load-file file)
	(message "Loaded %s" file))
    (message "File %s was not found" file)))

(load-file-if-exists "~/.emacs.d/public_config.el")
(load-file-if-exists "~/.emacs.d/private_config.el")
