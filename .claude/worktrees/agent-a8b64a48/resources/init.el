;;-*- mode: elisp -*-

head1
;;
;; declare package archives
;; https://www.reddit.com/r/emacs/comments/6s9ez3/package_management_under_emacs_25/
(require 'package)
;(setq package-user-dir "~/.emacs-packages/")
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;;(when (not package-archive-contents)
;;  (package-refresh-contents))
;;
;; declare load-path
;; https://www.emacswiki.org/emacs/LoadPath
(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))
(let ((default-directory  "~/.emacs.d/elpa/"))
  (normal-top-level-add-to-load-path '("."))
  (normal-top-level-add-subdirs-to-load-path))

(setq custom-file "~/.emacs.d/not_custom.el")

(load "~/.emacs.d/common_config.el")

(let ((lib-file (expand-file-name "~/repos/org/lib/lisp_functions.el")))
  (if (file-exists-p lib-file)
      (progn
        (load-file lib-file)
        (message "Loaded %s" lib-file))
    (message "File %s not found" lib-file)))
