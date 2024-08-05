;; -*- mode: elisp -*-

;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   General Setup   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

; ---   Setup Custom Configuration   --- ;
; -------------------------------------- ;

; Load private config or throw an error
(let ((private-config-file "~/.emacs.d/private_config.el"))
  (unless (file-exists-p private-config-file)
    (error "This initiazation expects a private configuration file at %s which is missing or unreadable" private-config-file))
  (load private-config-file))

(load "~/.emacs.d/private_config.el")

; Ensure downstream variables from the private config are present
(defun ensure-variable (var)
  (unless (boundp var)
    (error "Variable %s is not set in the private configuration file" var))
  (symbol-value var))


; ---   Setup Package Handling   --- ;
; ---------------------------------- ;

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/"))
(package-initialize)

(setq package-user-dir (ensure-variable 'my-elpa-dir))
(add-to-list 'load-path package-user-dir)

(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Per-Package Configuration   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ---   Base Emacs   --- ;
; ---------------------- ;

(setq large-file-warning-threshold most-positive-fixnum) ; disable large file warning
(setq-default cache-long-scans nil)
(setq bibtex-completion-bibliography (ensure-variable 'my-bibtex-bibliography))

; ---   Org-mode   --- ;
; -------------------- ;

(require 'org)
(require 'ox-latex)


;; Babel
(setq org-babel-default-header-args '((:results . "silent")
                                      (:eval . "no-export")
                                      (:exports . "none")
                                      (:tangle . "no")
                                      (:cache . "yes")
                                      (:noweb . "yes")))
(setq org-export-babel-evaluate t)

;; Export
(setq org-latex-src-block-backend 'minted)
(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	"bibtex %b"
	"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
(setq org-export-with-broken-links t)
(setq org-export-allow-bind-keywords t)
(setq org-latex-log-file "/tmp/org-latex-output.log")

;; Miscellaneous

;; Ox-latex
(customize-set-value 'org-latex-with-hyperref nil) ; https://emacs.stackexchange.com/questions/12878/how-to-change-style-of-hyperlinks-within-pdf-published-from-org-mode-document
(add-to-list 'org-latex-packages-alist '("" "listingsutf8"))

(setq org-latex-caption-above nil)

(add-to-list 'org-latex-classes '("empty"
                                  "\\documentclass{article}
\\newcommand\\foo{bar}
[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]"
                                  ("\\section{%s}" . "\\section*{%s}")
                                  ("\\subsection{%s}" . "\\subsection*{%s}")
                                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

; ---   Org-ref   --- ;
; ------------------- ;

(use-package org-ref
  :ensure t)

(setq org-ref-default-bibliography (ensure-variable 'my-bibtex-bibliography))

; ---   Ox-extra   --- ;
; -------------------- ;

(require 'ox-extra)
(ox-extras-activate '(ignore-headlines))
(ox-extras-activate '(latex-header-blocks ignore-headlines))


;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   Miscellaneous   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package helm-bibtex
  :ensure t)

(use-package ivy-bibtex
  :ensure t)

(use-package pdf-tools
  :ensure t)
