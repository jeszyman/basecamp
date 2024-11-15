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

(require 'org)
(require 'ox-latex)



;; the below as nil fucks of export of inline code
(setq org-export-babel-evaluate t)
;; https://emacs.stackexchange.com/questions/23982/cleanup-org-mode-export-intermediary-file/24000#24000


(setq-default cache-long-scans nil)
(setq org-export-with-broken-links t)
(setq org-export-allow-bind-keywords t)

(setq org-export-with-sub-superscripts nil
      org-export-headline-levels 2
      org-export-with-toc nil
      org-export-with-section-numbers nil
      org-export-with-tags nil
      org-export-with-todo-keywords nil)
(require 'ox-latex)

(customize-set-value 'org-latex-with-hyperref nil) 

(setq org-latex-logfiles-extensions (quote ("auto" "lof" "lot" "tex~" "aux" "idx" "log" "out" "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk" "blg" "brf" "fls" "entoc" "ps" "spl" "bbl")))

(add-to-list 'org-latex-packages-alist '("" "listings"))
(add-to-list 'org-latex-packages-alist '("" "color"))
(setq org-latex-caption-above nil)

(setq org-latex-remove-logfiles t)

(add-to-list 'org-latex-packages-alist '("" "listingsutf8"))
(setq org-latex-src-block-backend 'minted)

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
    "bibtex %b"
    "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
    "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))


(defun org-plain-follow (id _)
  "Follow a plain link as if it were an ID link."
  (org-id-open id nil))

(org-link-set-parameters "plain"
                         :follow #'org-plain-follow
                         :export #'org-plain-export)

(defun org-plain-export (link description format _)
  "Export a plain link. Always export as plain text."
  (cond
   ((eq format 'html) (or description link))
   ((eq format 'latex) (or description link))
   ((eq format 'ascii) (or description link))
   (t link)))

(provide 'ol-plain)

(with-eval-after-load 'org
  (require 'ol-plain))
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes '("empty"
                                    "\\documentclass{article}
\\newcommand\\foo{bar}
[NO-DEFAULT-PACKAGES]
[NO-PACKAGES]"
                                    ("\\section{%s}" . "\\section*{%s}")
                                    ("\\subsection{%s}" . "\\subsection*{%s}")
                                    ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                                    ("\\paragraph{%s}" . "\\paragraph*{%s}")
                                    ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

; ---   Org-ref   --- ;
; ------------------- ;

(use-package org-ref
  :ensure t)

;(setq org-ref-default-bibliography (variable 'my-bibtex-bibliography))

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
