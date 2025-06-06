;; Public configuration
;; :PROPERTIES:
;; :ID:       28ed3017-eedb-4a6c-94e5-f477353170c8
;; :header-args: :tangle ./emacs/public_config.el :comments org
;; :END:
;; [[file:emacs/public_config.el]]

;; Public Config
;; AUCTeX

(use-package tex
  :ensure auctex
  :config
  (setenv "PATH" (concat "/usr/local/texlive/2021/bin/x86_64-linux:"
			 (getenv "PATH")))
  (add-to-list 'exec-path "/usr/local/texlive/2021/bin/x86_64-linux")

  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-save-query nil
        TeX-view-program-selection '((output-pdf "Okular"))
        TeX-view-program-list '(("Okular" "okular %o"))))
;; Dired
;; :PROPERTIES:
;; :ID:       b3bd2685-ea78-4097-ae4c-e4a5e7a422ea
;; :END:

(setq dired-kill-when-opening-new-dired-buffer t)
(setq dired-listing-switches "-alh")
;; Needed in --batch
;; Code that needs to be tangled both here and into custom inits for batch export
;; #+name need_in_batch

(setq large-file-warning-threshold most-positive-fixnum) ; disable large file warning
(setq-default cache-long-scans nil)
;; Add to load paths

(setq exec-path (append exec-path '("/opt/miniconda3/bin")))
(setenv "PATH" (concat (getenv "PATH") ":/opt/miniconda3/bin"))
;; Appearance

; ---   General   --- ;
; ------------------- ;

(setq frame-background-mode 'dark)
(setq inhibit-splash-screen t)

; ---   Windows   --- ;
; ------------------- ;

;; Remove bars:
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
;
; Fringe- Set finge color to background
;https://emacs.stackexchange.com/a/31944/11502
(set-face-attribute 'fringe nil :background nil)

; ---   Lines   --- ;
; ----------------- ;

;;
;; Enable visual line mode
(global-visual-line-mode 1)
;;
;; Line highlighting in all buffers
(global-hl-line-mode t)
;;
;; Line numbers
(global-display-line-numbers-mode 0)
;;;
;;; Disable line numbers by buffer
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))
;;
(setq-default indicate-empty-lines t)

; Do not wrap lines, but extend them off screen
(setq default-truncate-lines nil)

;; no line numbers
(setq global-linum-mode nil)

; ---   Syntax Highlighting   --- ;
; ------------------------------- ;

;; When enabled, any matching parenthesis is highlighted
(show-paren-mode)
;;
;; Enables highlighting of the region whenever the mark is active
(transient-mark-mode 1)

; ---   Code   --- ;
; ---------------- ;

;; Delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

; ---   Faces   --- ;
; ----------------- ;

;; ?Fix broken face inheritance
(let ((faces (face-list)))
  (dolist (face faces)
    (let ((inh (face-attribute face :inherit)))
      (when (not (memq inh faces))
        (set-face-attribute face nil :inherit nil)))))

; ---   Text   --- ;
; ---------------- ;

;https://emacs.stackexchange.com/questions/72483/how-to-define-consult-faces-generically-for-minibuffer-highlighting-that-fits-wi
(global-hl-line-mode 1)
(set-face-attribute 'highlight nil :background "#294F6E")
;; Tramp

(setq tramp-default-method "ssh")


(defadvice tramp-completion-handle-file-name-all-completions
  (around dotemacs-completion-docker activate)
  "(tramp-completion-handle-file-name-all-completions \"\" \"/docker:\" returns
    a list of active Docker container names, followed by colons."
  (if (equal (ad-get-arg 1) "/docker:")
      (let* ((dockernames-raw (shell-command-to-string "docker ps | awk '$NF != \"NAMES\" { print $NF \":\" }'"))
             (dockernames (cl-remove-if-not
                           #'(lambda (dockerline) (string-match ":$" dockerline))
                           (split-string dockernames-raw "\n"))))
        (setq ad-return-value dockernames))
    ad-do-it))

; https://emacs.stackexchange.com/questions/29286/tramp-unable-to-open-some-files
(setq tramp-copy-size-limit 10000000)
;; Key bindings

; ASCII Arrows

; ---   ASCII Arrows   --- ;
; ------------------------ ;

(global-set-key (kbd "C-<right>") (lambda () (interactive) (insert "\u2192")))
(global-set-key (kbd "C-<up>") (lambda () (interactive) (insert "\u2191")))

; ---   Disable Keys   --- ;
; ------------------------ ;

;; Minimize
(global-unset-key (kbd "C-z"))
;; Print
(global-unset-key (kbd "s-p"))

(global-set-key (kbd "C-S-n")
		(lambda () (interactive) (next-line 10)))
(global-set-key (kbd "C-S-p")
		(lambda () (interactive) (next-line -10)))
;; On-save hooks and backup
;; - Visited files are backed up to =~/.emacs.d/backup-save-list=.


;; Shorthand for save all buffers
;;  https://stackoverflow.com/questions/15254414/how-to-silently-save-all-buffers-in-emacs
(defun save-all ()
  (interactive)
  (save-some-buffers t))


; ---   Saving And Backup   --- ;
; ----------------------------- ;

; Delete trailing whitespace on save
(add-hook 'before-save-hook
          'delete-trailing-whitespace)

;; Backup process upon save

(setq vc-make-backup-files t) ; Allow old versions to be saved
(setq delete-old-versions 20) ; Save 20
(setq backup-directory-alist '(("." . "~/.emacs.d/backup-save-list"))) ; Save them here

(setq auto-save-visited-mode t) ; Visited files will be auto-saved


(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "auto-save-list/") t)))
;; Miscellaneous

; ---   Miscellaneous   --- ;
; ------------------------- ;

;https://emacs.stackexchange.com/questions/62419/what-is-causing-emacs-remote-shell-to-be-slow-on-completion
(defun my-shell-mode-setup-function ()
  (when (and (fboundp 'company-mode)
             (file-remote-p default-directory))
    (company-mode -1)))

(add-hook 'shell-mode-hook 'my-shell-mode-setup-function)

;; delete the region when typing, just like as we expect nowadays.
(delete-selection-mode t)

(setq explicit-shell-file-name "/bin/bash")

;; Don't count two spaces after a period as the end of a sentence.
(setq sentence-end-double-space nil)

;; don't check package signatures
;;  https://emacs.stackexchange.com/questions/233/how-to-proceed-on-package-el-signature-check-failure
(setq package-check-signature nil)

;; Avoid nesting exceeds max-lisp-eval-depth error
;;  https://stackoverflow.com/questions/11807128/emacs-nesting-exceeds-max-lisp-eval-depth
(setq max-lisp-eval-depth 1200)

;; allow remembering risky variables
;;  https://emacs.stackexchange.com/questions/10983/remember-permission-to-execute-risky-local-variables
(defun risky-local-variable-p (sym &optional _ignored) nil)

; Disable "buffer is read only" warning
;;https://emacs.stackexchange.com/questions/19742/is-there-a-way-to-disable-the-buffer-is-read-only-warning
(defun my-command-error-function (data context caller)
  "Ignore the buffer-read-only signal; pass the rest to the default handler."
  (when (not (eq (car data) 'buffer-read-only))
    (command-error-default-function data context caller)))

(setq command-error-function #'my-command-error-function)

; Follow symlinks in dired
;;https://emacs.stackexchange.com/questions/41286/follow-symlinked-directories-in-dired
(setq find-file-visit-truename t)

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "/usr/bin/brave-browser")

; y or n instead of yes or no
(setopt use-short-answers t)

;; don't check package signatures
;;  https://emacs.stackexchange.com/questions/233/how-to-proceed-on-package-el-signature-check-failure
(setq package-check-signature nil)

;; Avoid nesting exceeds max-lisp-eval-depth error
;;  https://stackoverflow.com/questions/11807128/emacs-nesting-exceeds-max-lisp-eval-depth
(setq max-lisp-eval-depth 1200)
;;

;; normal c-c in ansi-term
;; https://emacs.stackexchange.com/questions/32491/normal-c-c-in-ansi-term
(eval-after-load "term"
  '(progn (term-set-escape-char ?\C-c)
          (define-key term-raw-map (kbd "C-c") nil)))

(setq comint-scroll-to-bottom-on-output t)

;; allow kill hidden part of line
;;  https://stackoverflow.com/questions/3281581/how-to-word-wrap-in-emacs
(setq-default word-wrap t)

;; auto-refresh if source changes
;;  https://stackoverflow.com/questions/1480572/how-to-have-emacs-auto-refresh-all-buffers-when-files-have-changed-on-disk
(global-auto-revert-mode 1)

; ---   Frames And Windows   --- ;
; ------------------------------ ;

(setq truncate-partial-width-windows nil)
(setq split-window-preferred-function (quote split-window-sensibly))

; ---   Other   --- ;
; ----------------- ;

(setq require-final-newline nil)
(defun toggle-theme ()
  "Toggle between dark and light themes."
  (interactive)
  (if (custom-theme-enabled-p 'manoj-dark)
      (progn
        (disable-theme 'manoj-dark)
        (load-theme 'leuven t))
    (progn
      (disable-theme 'leuven)
      (load-theme 'manoj-dark t))))
(setq create-lockfiles nil)
;; (open-texdoc-in-background)
;; :PROPERTIES:
;; :ID:       60651916-4ffa-458f-9084-1ade3d163ea0
;; :END:

(defun open-texdoc-in-background (docname)
  "Open a TEXDOC for DOCNAME in the background and close the terminal."
  (interactive "sEnter the name of the document: ")
  (let ((term-buffer (ansi-term "/bin/bash")))
    (with-current-buffer term-buffer
      (term-send-raw-string (concat "texdoc " docname "\n"))
      (term-send-raw-string "sleep 2; exit\n")
      (set-process-sentinel
       (get-buffer-process term-buffer)
       (lambda (process signal)
         (when (or (string= signal "finished\n")
                   (string= signal "exited\n"))
           (kill-buffer (process-buffer process)))))
      (bury-buffer))))
;; Make header regions read-only via tag

(defun org-mark-readonly ()
  (interactive)
  (let ((buf-mod (buffer-modified-p)))
    (org-map-entries
     (lambda ()
       (org-mark-subtree)
       (add-text-properties (region-beginning) (region-end) '(read-only t)))
     "read_only")
    (unless buf-mod
      (set-buffer-modified-p nil))))


(defun org-remove-readonly ()
  (interactive)
  (let ((buf-mod (buffer-modified-p)))
    (org-map-entries
     (lambda ()
       (let* ((inhibit-read-only t))
     (org-mark-subtree)
     (remove-text-properties (region-beginning) (region-end) '(read-only t))))
     "read_only")
    (unless buf-mod
      (set-buffer-modified-p nil))))

(add-hook 'org-mode-hook 'org-mark-readonly)
;; Protect text regions as read-only
;; https://chatgpt.com/c/fe962d8c-eb34-42fe-b362-032a61d8b728

(defun make-region-read-only (start end)
  (interactive "*r")
  (let ((inhibit-read-only t))
    (put-text-property start end 'read-only t)
    (put-text-property start end 'font-lock-face '(:background "#8B0000"))))

(defun make-region-read-write (start end)
  (interactive "*r")
  (let ((inhibit-read-only t))
    (put-text-property start end 'read-only nil)
    (remove-text-properties start end '(font-lock-face nil))))
;; Shell

(defun dont-ask-to-kill-shell-buffer ()
  "Don't ask for confirmation when killing *shell* buffer."
  (let ((buffer-name (buffer-name)))
    (when (string-equal buffer-name "*shell*")
      (setq kill-buffer-query-functions
            (delq 'process-kill-buffer-query-function
                  kill-buffer-query-functions)))))

(add-hook 'shell-mode-hook 'dont-ask-to-kill-shell-buffer)
;; cua-mode

(cua-mode t)
;; remove-blank-lines

(defun remove-blank-lines ()
  "Remove all blank lines (including lines with only whitespace) in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (flush-lines "^[[:space:]]*$")))
;; Startup

(require 'org)
(setq org-startup-folded t)
(setq org-startup-with-inline-images t)
;; Add persistant highlights

(defun org-add-persistent-highlights ()
  "Add persistent highlighting for custom markers in Org mode."
  (font-lock-add-keywords
   nil
   '(("<{\\(.*?\\)}>" ; Match the pattern <{...}>
      (1 '(:background "firebrick") t)))))

(add-hook 'org-mode-hook #'org-add-persistent-highlights)

(defun org-hide-markers-without-space ()
  "Hide markers like << and >> in Org-mode without leaving empty space."
  (font-lock-add-keywords
   nil
   '(("<{\\|}>"
      (0 (progn (put-text-property (match-beginning 0) (match-end 0)
                                   'display "")
                'org-hide))))))

(add-hook 'org-mode-hook #'org-hide-markers-without-space)
;; Tags

        (setq
         org-tags-exclude-from-inheritance
         (list
	  "alert"
          "biotool"
          "biopipe"
          "bimonthly"
          "block"
          "blk"
          "flat"
          "hierarchy"
          "include"
          "semimonthly"
          "purpose"
          "midGoal"
          "nearGoal"
          "focus"
          "project"
          "daily"
          "dinner"
	  "kit"
          "maint"
          "manuscript"
          "mod"
          "monthly"
	  "poster"
          "present"
          "prog"
          "report"
          "routine"
          "soln"
          "weekly"
          "write"
          "sci_rep"
          "stretch"
          "study"))
;; .TODO

(setq org-todo-keyword-faces
      (quote (("TODO" :background "red")
              ("NEXT" :foreground "black" :background "yellow"))))

;; keep TODO state timestamps in drawer
(setq org-log-into-drawer t)

;; add done timestamp
(setq org-log-done 'time)

;; enforce dependencies
(setq org-enforce-todo-dependencies t)

;; priority levels
(setq org-highest-priority 65)
(setq org-lowest-priority 89)
(setq org-default-priority 89)
;; open in same frame

(setq org-link-frame-setup
      '((vm . vm-visit-folder)
        (vm-imap . vm-visit-imap-folder)
        (gnus . org-gnus-no-new-news)
        (file . find-file)  ;; Open files in the same frame
        (wl . wl)))
;; Set org-file-apps to use xdg-open for all file extensions


(setq org-file-apps
      `((directory . "/usr/bin/gnome-terminal --working-directory=\"%s\"")
        ("\\.pdf\\'" . "setsid -w xdg-open \"%s\"")
        ("\\.svg\\'" . "setsid -w xdg-open \"%s\"")
        ("\\.\\(yaml\\|yml\\|txt\\|md\\|conf\\|list\\)\\'" .
         (lambda (file path)
           (start-process "emacsclient" nil
                          "setsid"
                          "/usr/local/bin/emacsclient"
                          "--socket-name" "/home/jeszyman/.emacs.d/server/server"
                          "-c" (expand-file-name file))))
        (auto-mode . emacsclient)
        (t . "setsid -w xdg-open \"%s\"")))
;; Lists

(setq org-cycle-include-plain-lists 'integrate)
(setq org-list-indent-offset 0)
;; Tables

;; https://emacs.stackexchange.com/questions/22210/auto-update-org-tables-before-each-export
(add-hook 'before-save-hook 'org-table-recalculate-buffer-tables)
(setq org-startup-align-all-tables t)
(setq org-startup-shrink-all-tables t)
;; org-image-actual-width
;; :PROPERTIES:
;; :ID:       a760d06b-c2b2-4b54-a0b6-375a13a20538
;; :END:
;; When set as a list as below, 300 pixels will be the default, but another width can be specified through ATTR, e.g. #+ATTR_ORG: :width 800px

(setq org-image-actual-width '(300))
;; shk-fix-inline-images, reload inline images after code eval
;; If you have code blocks in Org mode that produce images (such as R or Python plots), this setup refreshes the images right after code execution without requiring manual intervention.


(defun shk-fix-inline-images ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

(with-eval-after-load 'org
  (add-hook 'org-babel-after-execute-hook 'shk-fix-inline-images))
;; Default header arguments
;; #+name: babel_default_header_args

(setq org-babel-default-header-args '(
				      (:comments . "no")
				      (:mkdirp . "yes")
				      (:padline . "no")
				      (:results . "silent")
                                      (:cache . "no")
                                      (:eval . "no-export")
                                      (:exports . "none")
                                      (:noweb . "yes")
                                      (:tangle . "no")
				      ))
;; General

(setq
 ;; Blocks inserted directly without additional formatting
 org-babel-inline-result-wrap "%s"
 ;;
 ;; Preserve language-specific indentation, aligns left
 org-src-preserve-indentation t
 ;;
 ;; Tab works like in major mode of lanuauge
 org-src-tab-acts-natively t
 ;;
 org-babel-python-command "python3"
 ;;
 org-confirm-babel-evaluate nil
 ;;
 org-src-fontify-natively t
 ;;
 ;; Open src windows in current frames
 org-src-window-setup 'current-window)
;; disable confrmation for elisp execution of org src blocks
(setq safe-local-variable-values '((org-confirm-elisp-link-function . nil)))

(setq org-hide-block-startup t)
;; Toggle collapse blocks

(defvar org-blocks-hidden nil)

(defun org-toggle-blocks ()
  (interactive)
  (if org-blocks-hidden
      (org-show-block-all)
    (org-hide-block-all))
  (setq-local org-blocks-hidden (not org-blocks-hidden)))
;; org-babel-min-lines-for-block-output
;; When executing a source block in org mode with the output set to verbatim, it will sometimes wrap the results in an #begin_example block, and sometimes it uses : symbols at the beginning of the line. Prevented with org-babel-src-preserve-indentation

;; https://emacs.stackexchange.com/questions/39390/force-org-to-use-instead-of-begin-example-for-source-block-output


(setq org-babel-min-lines-for-block-output 1000)
;; Change noweb wrapper symbols

(setq org-babel-noweb-wrap-start "<#"
      org-babel-noweb-wrap-end "#>")
;; stripe properties drawer on tangle

;; (defun my/org-babel-tangle-no-drawers ()
;;   "Tangle with `:comments org`, removing property drawers entirely."
;;   (interactive)
;;   (let ((tangled-files (org-babel-tangle)))  ;; returns a list of file names
;;     (dolist (f tangled-files)
;;       (with-temp-buffer
;;         (insert-file-contents f)
;;         (goto-char (point-min))
;;         ;; Remove everything from #.*:PROPERTIES: up to #.*:END:
;;         (while (re-search-forward "^#.*:PROPERTIES:" nil t)
;;           (let ((start (match-beginning 0)))
;;             (when (re-search-forward "^#.*:END:" nil t)
;;               (delete-region start (match-end 0)))))
;;         ;; Remove extra blank lines
;;         (goto-char (point-min))
;;         (while (re-search-forward "\n\\{2,\\}" nil t)
;;           (replace-match "\n"))
;;         (write-region (point-min) (point-max) f)))))
;; Header views and cycling

(setq org-show-hierarchy-above t)

(setq org-fold-show-context-detail
      '((default . tree)))
(setq
 org-show-context-detail
 '((agenda . ancestors)
   (bookmark-jump . ancestors)
   (isearch . ancestors)
   (default . ancestors))
)
(defun org-hide-all-src-blocks ()
  "Hide all source blocks in the current Org buffer."
  (interactive)
  (org-babel-map-src-blocks nil
    (save-excursion
      (goto-char (org-babel-where-is-src-block-head))
      (org-hide-block-toggle t))))

(defun my-collapse-all-drawers (&optional arg)
  "Hide all drawers and optionally cycle global visibility.
When called with a prefix ARG (C-u), also cycle global visibility, hide all src blocks, and jump to the beginning of the buffer."
  (interactive "P")
  (org-hide-drawer-all)
  (when arg
    (org-cycle-global)
    (org-hide-all-src-blocks)
    (goto-char (point-min))) ;; Use `goto-char` instead of `beginning-of-buffer` for clarity
  ;; Ensure tags are aligned after all visibility changes
  (org-align-tags t)) ;; Pass `t` to align all tags in the buffer


(global-set-key (kbd "C-c d") 'my-collapse-all-drawers)
;; You might want to remove the hook if you don't want this function to run every time you open an org file
(add-hook 'org-mode-hook 'my-collapse-all-drawers)
;; No blank lines!

(setq org-cycle-separator-lines 0)
(setq yas-indent-line 'fixed)


;; https://chatgpt.com/c/670d4cb7-5c08-8005-bec8-a2800e4bd0c4

;; (defun my-remove-trailing-newlines-in-tangled-blocks ()
;;   "Remove trailing newlines from tangled block bodies."
;;   (save-excursion
;;     (goto-char (point-max))
;;     (when (looking-back "\n" nil)
;;       (delete-char -1))))

;; (add-hook 'org-babel-post-tangle-hook #'my-remove-trailing-newlines-in-tangled-blocks)
;; General

(setq org-confirm-shell-link-function nil)
(with-eval-after-load 'org
        (add-to-list 'org-modules 'org-habit))

;; Clock times in hours and minutes
;;  (see https://stackoverflow.com/questions/22720526/set-clock-table-duration-format-for-emacs-org-mode
(setq org-time-clocksum-format
      '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))
(setq org-duration-format (quote h:mm))

(setq org-catch-invisible-edits 'error)
(global-set-key (kbd "C-c l") 'org-store-link)
(setq org-indirect-buffer-display 'current-window)

(setq org-id-link-to-org-use-id 'use-existing)
;;https://stackoverflow.com/questions/28351465/emacs-orgmode-do-not-insert-line-between-headers


(setq org-enforce-todo-checkbox-dependencies t)
;; don't adapt indentation to header level
(setq org-adapt-indentation nil)

(setq org-support-shift-select t)
(setq org-src-window-setup 'current-window)
(setq org-export-async-debug nil)
;; ensures that any file with the .org extension will automatically open in org-mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
;; Make heading regex include tags
(setq org-heading-regexp "^[[:space:]]*\\(\\*+\\)\\(?: +\\(.*?\\)\\)?[ \t]*\\(:[[:alnum:]_@#%:]+:\\)?[ \t]*$")
;; org-blank-before-new-entry
;; https://stackoverflow.com/questions/28351465/emacs-orgmode-do-not-insert-line-between-headers

(setq org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))
;; my-org-tree-to-indirect-buffer

(defun my-org-tree-to-indirect-buffer (&optional arg)
  "Open current org tree in indirect buffer, using one prefix argument.
When called with two prefix arguments, ARG, run the original function without prefix argument."
  (interactive "P")
  (if (equal arg '(16)) ; 'C-u C-u' produces (16)
      (org-tree-to-indirect-buffer nil) ; original behavior
    (org-tree-to-indirect-buffer t)) ; one prefix argument
  (my-collapse-all-drawers))
(define-key org-mode-map (kbd "C-c C-x b") 'my-org-tree-to-indirect-buffer)
;; Export
;; :PROPERTIES:
;; :ID:       8fb9a592-2ad4-44c4-a8d6-ce33f691c010
;; :END:
;; #+name: orgmode_export_general

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
(setq org-odt-preferred-output-format "docx")
;; LaTeX
;; #+name: orgmode_export_latex

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


(setq org-export-preserve-breaks t)
;; Empty latex class
;; #+name: org_latex_empty_class

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
;; Open URLs in new window with C-u C-c C-o

(defun my-org-open-in-brave-new-window (link)
  "Open the LINK in Brave browser in a new window."
  (start-process "brave-new-window" nil "brave-browser" "--new-window" link))

(defun org-link-frame-open-id-or-file-in-new-frame (link)
  "Open the FILE or ID LINK in a new frame."
  (let ((location (cond
                   ((string= (org-element-property :type (org-element-context)) "id")
                    (org-id-find link 'marker))
                   (t
                    link)))) ; Assume it's a file link directly usable by `find-file-noselect`
    (if (markerp location)
        (with-current-buffer (marker-buffer location)
          (select-frame (make-frame))
          (goto-char location))
      (select-frame (make-frame))
      (find-file link))))

(defun my-org-open-at-point (&optional arg)
  "Open the link at point.
Use a new window in Brave if ARG is non-nil and the link is a URL.
Open in a new Emacs frame if ARG is non-nil for ID or file links."
  (interactive "P")
  (let* ((context (org-element-context))
         (link-type (org-element-property :type context))
         (raw-link (org-element-property :raw-link context))
         (link-path (org-element-property :path context)))
    (if (and arg link-type)
        (cond
         ((or (string= link-type "http") (string= link-type "https"))
          (my-org-open-in-brave-new-window raw-link))
         ((or (string= link-type "id") (string= link-type "file"))
          (org-link-frame-open-id-or-file-in-new-frame link-path))
         (t
          (message "No special handling for this link type: %s" link-type)))
      (org-open-at-point))))

;; Rebind C-c C-o in org mode to our custom function
(define-key org-mode-map (kbd "C-c C-o") 'my-org-open-at-point)
;; Checkboxes


;; (defun org-toggle-checkbox-and-children ()
;;   "Toggle checkbox and all children checkboxes."
;;   (interactive)
;;   (save-excursion
;;     (let* ((parent-indent (current-indentation))
;;            (end (save-excursion
;;                   (org-end-of-subtree)
;;                   (point)))
;;            (current-checkbox (save-excursion
;;                              (beginning-of-line)
;;                              (when (re-search-forward "\\[[ X-]\\]" (line-end-position) t)
;;                                (match-string 0))))
;;            (new-state (if (equal current-checkbox "[ ]") "[X]" "[ ]")))
;;       ;; Toggle the parent checkbox
;;       (beginning-of-line)
;;       (when (re-search-forward "\\[[ X-]\\]" (line-end-position) t)
;;         (replace-match new-state))
;;       ;; Toggle all children checkboxes
;;       (forward-line)
;;       (while (< (point) end)
;;         (let ((line-start (point)))
;;           (when (and (> (current-indentation) parent-indent)
;;                     (re-search-forward "\\[[ X-]\\]" (line-end-position) t))
;;             (replace-match new-state)
;;             (goto-char line-start)))
;;         (forward-line 1)))))

;; ;; Bind it to a convenient key
;; (define-key org-mode-map (kbd "C-c x") 'org-toggle-checkbox-and-children)
;; Documentation

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("documentation"
                 "\\documentclass{article}
                 \\usepackage{/home/jeszyman/repos/latex/sty/documentation}
                 [NO-DEFAULT-PACKAGES]
                 [NO-PACKAGES]
                 [EXTRA]
                 \\tableofcontents"
                 ("\\section{%s}" . "\\section{%s}")
                 ("\\subsection{%s}" . "\\subsection{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection{%s}")
                 ("\\paragraph{%s}" . "\\paragraph{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph{%s}"))))
(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("documentation"
                 "\\documentclass{article}
                 \\usepackage{/home/jeszyman/repos/latex/sty/documentation}
                 [NO-DEFAULT-PACKAGES]
                 [NO-PACKAGES]
                 [EXTRA]
                 \\begin{document}
                 \\tableofcontents
                 \\vspace{1cm}"
                 ("\\section{%s}" . "\\section{%s}")
                 ("\\subsection{%s}" . "\\subsection{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection{%s}")
                 ("\\paragraph{%s}" . "\\paragraph{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph{%s}"))))
;; iCalendar

(setq org-icalendar-with-timestamps 'active)
(setq org-icalendar-use-scheduled t)
(setq org-icalendar-use-deadline nil)
(setq org-icalendar-include-todo t)
(setq org-icalendar-exclude-tags (list "noexport"))
(setq org-icalendar-include-body '1)
(setq org-icalendar-alarm-time '5)
(setq org-icalendar-store-UID t) ;;Required for syncs
(setq org-icalendar-timezone "America/Chicago")
(setq org-agenda-default-appointment-duration 30)
(setq org-icalendar-combined-agenda-file "/tmp/org.ics")
;; Properties

(setq org-use-property-inheritance t)
;; (browse-org-table-urls-by-name)


(defun browse-org-table-urls-by-name (table-name)
  "Browse URLs listed in an Org-mode table identified by TABLE-NAME.
TABLE-NAME is the name of the table identified as #+name."

  (interactive "sEnter table name: ")
  (let* ((element (ignore-errors
                    (org-element-map (org-element-parse-buffer) 'table
                      (lambda (el)
                        (when (string= (org-element-property :name el) table-name)
                          el))
                      nil t))))
    (if (not element)
        (message "Table with name %s not found" table-name)
      (let ((table-begin (org-element-property :contents-begin element))
            (table-end (org-element-property :contents-end element)))
        (if (or (null table-begin) (null table-end))
            (message "No contents found for the table with name %s" table-name)
          (let ((table-content (buffer-substring-no-properties table-begin table-end)))
            (with-temp-buffer
              (insert table-content)
              (goto-char (point-min))
              (let ((urls (org-table-to-lisp)))
                (if (not urls)
                    (message "No URLs found in the table with name %s" table-name)
                  (let ((first-url (car (car urls))))
                    (start-process "brave-browser" nil "brave-browser" "--new-window" first-url)
                    (sit-for 2)  ;; Wait for the new window to open
                    (dolist (url-row (cdr urls))
                      (start-process "brave-browser" nil "brave-browser" (car url-row))
                      (sit-for 0.5)))  ;; Delay between each URL
                  (message "Opened URLs from table with name %s" table))))))))))
;; Agenda

(global-set-key "\C-ca" 'org-agenda)

(setq org-agenda-repeating-timestamp-show-all nil)
(setq org-sort-agenda-notime-is-late nil)
(setq org-agenda-start-on-weekday nil)
(setq org-agenda-remove-tags t)
(setq org-agenda-skip-scheduled-if-done t)

(setq org-agenda-files
      (append
       (list "~/repos/org/") ;; keep your org directory
       (cl-remove-if-not
        #'file-regular-p
        (mapcar (lambda (d)
                  (expand-file-name
                   (concat (file-name-nondirectory (directory-file-name d)) ".org")
                   d))
                (directory-files "~/repos/" t "^[^.]+" t)))))

(setq org-agenda-skip-unavailable-files t)


(setq org-agenda-use-tag-inheritance t)
;;  http://stackoverflow.com/questions/36873727/make-org-agenda-full-screen
(setq org-agenda-window-setup (quote only-window))
(setq org-agenda-todo-ignore-time-comparison-use-seconds t)

;;; Based on http://article.gmane.org/gmane.emacs.orgmode/41427
  (defun my-skip-tag(tag)
    "Skip entries that are tagged TAG"
    (let* ((entry-tags (org-get-tags-at (point))))
      (if (member tag entry-tags)
          (progn (outline-next-heading) (point))
        nil)))
;; Needed for no y/n prompt at linked agenda execution
(setq org-confirm-elisp-link-function nil)
;; :plain link type
;; :PROPERTIES:
;; :ID:       0f1b7abb-233b-4e81-abcb-e424e9b860e4
;; :END:
;; https://claude.ai/chat/c775f0eb-fa91-45b4-82d6-e1a0df8b5526
;; #+name: org_plain_links

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
;; org-image-actual-width
;; :PROPERTIES:
;; :ID:       a760d06b-c2b2-4b54-a0b6-375a13a20538
;; :END:
;; When set as a list as below, 300 pixels will be the default, but another width can be specified through ATTR, e.g. #+ATTR_ORG: :width 800px

(setq org-image-actual-width '(300))
;; Editing text

;;https://emacs.stackexchange.com/questions/12701/kill-a-line-deletes-the-line-but-leaves-a-blank-newline-character
(setq kill-whole-line t)
;; bibtex

(setq reftex-default-bibliography '("~/repos/org/bib.bib"))

;; see org-ref for use of these variables

(setq bibtex-completion-bibliography "~/repos/org/bib.bib"
      bibtex-completion-library-path "~/library"
      bibtex-completion-notes-path "~/repo/org/notes")
;; get-bibtex-from-doi
;; :PROPERTIES:
;; :ID:       f22974c3-ff99-4da0-b253-35ced07588bf
;; :END:

;; Amazing bibtex from doi fetcher
;; https://www.anghyflawn.net/blog/2014/emacs-give-a-doi-get-a-bibtex-entry/
(defun get-bibtex-from-doi (doi)
 "Get a BibTeX entry from the DOI"
 (interactive "MDOI: ")
 (let ((url-mime-accept-string "text/bibliography;style=bibtex"))
   (with-current-buffer
     (url-retrieve-synchronously
       (format "http://dx.doi.org/%s"
       	(replace-regexp-in-string "http://dx.doi.org/" "" doi)))
     (switch-to-buffer (current-buffer))
     (goto-char (point-max))
     (setq bibtex-entry
     	  (buffer-substring
          	(string-match "@" (buffer-string))
              (point)))
     (kill-buffer (current-buffer))))
 (insert (decode-coding-string bibtex-entry 'utf-8))
 (bibtex-fill-entry))
;; python.el
;; https://github.com/gregsexton/ob-ipython/issues/28

(setq python-shell-completion-native-enable nil)

(add-hook 'python-mode-hook
  (lambda () (setq indent-tabs-mode nil)))

(setq python-indent-guess-indent-offset-verbose nil)
;; UTF8

(set-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
;; Alpha key

(global-set-key (kbd "C-x a") (lambda () (interactive) (insert "α")))
;; Use-package

(use-package blacken
  :after elpy
  :hook (elpy-mode . blacken-mode))
;; citar

;; Citar configuration for org-cite integration
(use-package citar
  :after org
  :init
  (require 'oc)  ;; Ensure org-cite is loaded before configuring citar

  :custom
  ;; Bibliography paths
  (org-cite-global-bibliography '("~/repos/org/bib.bib"))
  (citar-bibliography org-cite-global-bibliography)
  (citar-library-paths '("~/data/library/"))   ;; PDF storage

  ;; Set citation processors
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)

  ;; Display formatting
  (citar-display-transform-functions '((t . citar-clean-string)))

  (citar-templates
   '((main . "${author editor:30%sn}     ${date year issued:4}     ${title:48}")
     (suffix . "          ${=key= id:15}    ${=type=:12}    ${tags keywords:*}")
     (preview . "${author editor:%etal} (${year issued date}) ${title}, ${journal journaltitle publisher container-title collection-title}.\n")
     (note . "Notes on ${author editor:%etal}, ${title}")))

  ;; Visual configuration
  (citar-symbols `((file . ?)))  ;; Icon for PDFs only

  ;; Open PDFs and BibTeX entries
  (citar-open-functions
   '((file . (lambda (fpath) (call-process (if (eq system-type 'darwin) "open" "xdg-open") nil 0 nil fpath)))
     (bibtex . citar-open-bibtex-entry)))  ;; Add BibTeX option

  :config
  (define-key org-mode-map (kbd "C-c ]") 'org-cite-insert))

;; Configure file opening for citar
(setq citar-file-open-functions
      '(("html" . citar-file-open-external)
        ("pdf" . citar-file-open-external)  ;; Use system default for PDFs
        (t . find-file)))                   ;; Default to Emacs for others
(use-package citar-embark
  :after citar embark
  :no-require
  :config (citar-embark-mode))

(setq citar-at-point-function 'embark-act)
;; Company-mode
;; - [[id:c65678fd-8df9-4f16-929c-c960c7441cd0][Company mode reference header]]

;; (use-package company
;;   :config
;;   (global-company-mode)
;;   (setq
;;    company-dabbrev-downcase nil)) ; Don't downcase by default
;; Conda

(setenv "WORKON_HOME" "~/miniconda3/envs")
;; Corfu

;; Corfu for completion UI
(use-package corfu
  :ensure t
  :init
  (global-corfu-mode) ;; Enable globally
  :custom
  (corfu-auto t)               ;; Enable auto completion
  (corfu-auto-prefix 2)        ;; Start completing after typing 2 characters
  (corfu-auto-delay 2.5)       ;; Delay before suggestions pop up
  :config
  ;; Free up keybindings for `completion-at-point`
  (with-eval-after-load 'flyspell
    (define-key flyspell-mode-map (kbd "C-M-i") nil)
    (define-key flyspell-mode-map (kbd "M-TAB") nil))
  (global-set-key (kbd "M-TAB") #'completion-at-point)) ;; Bind `M-TAB` globally
;; Dabbrev

;; Dynamic abbreviation completion
(use-package dabbrev
  :ensure nil
  :config
  (setq dabbrev-case-fold-search t) ;; Case-insensitive search
  (setq dabbrev-upcase-means-case-search t)) ;; Respect case for uppercase words
;; Use-package

(use-package eglot
  :ensure t
  :init
  (add-hook 'sh-mode-hook 'eglot-ensure)
  (add-hook 'ess-r-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook 'eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs '(sh-mode . ("bash-language-server" "start")))
  (add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))
  (add-to-list 'eglot-server-programs '(ess-r-mode . ("R" "--slave" "-e" "languageserver::run()"))))

(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c <tab>") #'company-complete))
;; Elpy
;; - [[id:5cd6f647-11ba-445a-bcde-07493b0e4fae][Elpy reference header]]
;; - Use package

(use-package elpy
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  (define-key elpy-mode-map (kbd "C-c C-n") 'elpy-shell-send-statement-and-step)
  (setenv "PATH" (concat "~/miniconda3/bin:" (getenv "PATH")))
  (setenv "WORKON_HOME" "~/miniconda3/envs")
  (setq exec-path (append '("~/miniconda3/bin") exec-path))
  (add-to-list 'process-coding-system-alist '("python" . (utf-8 . utf-8)))
  (setq elpy-rpc-python-command "~/miniconda3/bin/python")
)



;; note, need to
;; conda activate base
;; pip install 'python-lsp-server[all]'
;; - Consistent keybindings with ESS, essh

(with-eval-after-load 'elpy
  (define-key elpy-mode-map (kbd "C-c C-b") 'elpy-shell-send-buffer))




;; - Open Python shells in a new frame
;;   - https://chatgpt.com/c/67252540-09c0-8005-8b13-e8cac310cd3a

(defun my-elpy-shell-display-buffer-in-new-frame (buffer alist)
  "Display the Python shell buffer in a new frame."
  (let ((display-buffer-alist
         '(("*Python*" display-buffer-pop-up-frame))))
    (display-buffer buffer alist)))

(advice-add 'elpy-shell-send-statement-and-step :around
            (lambda (orig-fun &rest args)
              "Send statement and open the Python buffer in a new frame."
              (let ((display-buffer-alist
                     '(("*Python*" . (my-elpy-shell-display-buffer-in-new-frame)))))
                (apply orig-fun args))))
;; Use-package

(use-package ess
  :init
  (require 'ess-site)
  :config
  (setq ess-ask-for-ess-directory nil
	ess-help-own-frame 'one
	ess-indent-with-fancy-comments nil
	ess-use-auto-complete t
	ess-use-company t
	inferior-ess-own-frame t
	inferior-ess-same-window nil)
  (define-key ess-mode-map (kbd "C-c C-n") 'ess-eval-line-and-step)
  :mode (
         ("/R/.*\\.q\\'"       . R-mode)
         ("\\.[rR]\\'"         . R-mode)
         ("\\.[rR]profile\\'"  . R-mode)
         ("NAMESPACE\\'"       . R-mode)
         ("CITATION\\'"        . R-mode)
         ("\\.[Rr]out"         . R-transcript-mode)
         ("\\.Rd\\'"           . Rd-mode)
         ))
;; Syntax highlighting

(custom-set-variables
 '(ess-R-font-lock-keywords
   (quote
    ((ess-R-fl-keyword:modifiers . t)
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
     (ess-R-fl-keyword:%op% . t)))))
;; Use-package

(use-package exec-path-from-shell
  :config
  (when (daemonp)
    (exec-path-from-shell-initialize)))
;; expand-region
;; :PROPERTIES:
;; :ID:       aa87da56-3f24-475c-adbe-d4d99a7376cc
;; :END:
;; https://github.com/magnars/expand-region.el

(use-package expand-region)
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
;; Use-package

(use-package flycheck
  :hook
  (org-src-mode . my-org-mode-flycheck-hook)
  :config
  (defun my-org-mode-flycheck-hook ()
    (when (derived-mode-p 'prog-mode) ;; Check if it's a programming mode
      (flycheck-mode 1))))
;; Helm
;; - [[id:96c0f509-c06b-4e48-8f28-019cd2ca1a38][Helm reference header]]

(use-package helm
  :config
  (global-set-key (kbd "C-x b") 'helm-mini)
  (global-set-key (kbd "C-s") 'helm-occur)
  (setq
   helm-completion-style 'emacs
   helm-move-to-line-cycle-in-source nil)) ;; allow C-n through different sections
;; helm-org


(use-package helm-org
  :config
  (global-set-key (kbd "C-c j") 'helm-org-in-buffer-headings)
  (global-set-key (kbd "C-c w") 'helm-org-refile-locations)
  (setq org-outline-path-complete-in-steps nil
	org-refile-allow-creating-parent-nodes 'confirm
	org-refile-targets '((org-agenda-files :maxlevel . 20))
	org-refile-use-outline-path 'file))
  (define-key global-map (kbd "C-c C-j") nil)
  (global-set-key (kbd "C-c C-j") 'helm-org-agenda-files-headings)
  (define-key global-map (kbd "C-$") 'org-mark-ring-goto)
  (global-set-key (kbd "C-c C-j") 'helm-org-agenda-files-headings)
  (setq helm-org-ignore-autosaves t)
(global-set-key (kbd "C-c C-j") 'helm-org-agenda-files-headings)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-j") 'helm-org-agenda-files-headings))
;; helm-org-rifle

(use-package helm-org-rifle
    :config
    (setq helm-org-rifle-show-path nil
	  helm-org-rifle-show-full-contents nil)
    (require 'helm)
    (global-set-key (kbd "C-c C-j") 'helm-org-agenda-files-headings))
;; Use-pacakge

(use-package htmlize)
;; Use-package

(use-package ivy
  :diminish)
;; marginalia

(use-package marginalia
  ;; Either bind `marginalia-cycle' globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  ;; The :init configuration is always executed (Not lazy!)
  :init
  (marginalia-mode)
  :ensure t)
;; mark-whole-word

  ;; https://emacs.stackexchange.com/questions/35069/best-way-to-select-a-word
  (defun mark-whole-word (&optional arg allow-extend)
    "Like `mark-word', but selects whole words and skips over whitespace.
  If you use a negative prefix arg then select words backward.
  Otherwise select them forward.

  If cursor starts in the middle of word then select that whole word.

  If there is whitespace between the initial cursor position and the
  first word (in the selection direction), it is skipped (not selected).

  If the command is repeated or the mark is active, select the next NUM
  words, where NUM is the numeric prefix argument.  (Negative NUM
  selects backward.)"
    (interactive "P\np")
    (let ((num  (prefix-numeric-value arg)))
      (unless (eq last-command this-command)
	(if (natnump num)
	    (skip-syntax-forward "\\s-")
	  (skip-syntax-backward "\\s-")))
      (unless (or (eq last-command this-command)
		  (if (natnump num)
		      (looking-at "\\b")
		    (looking-back "\\b")))
	(if (natnump num)
	    (left-word)
	  (right-word)))
      (mark-word arg allow-extend)))

  (global-set-key (kbd "C-c C-SPC") 'mark-whole-word)
;; Use-package

(use-package native-complete)
;; open-chatgtp-query-in-new-browser-window

;; - Make a ChatGPT query from emacs
;;   - https://chatgpt.com/c/d4f18f6b-2f09-4a69-93f1-8f8ab5b39cb0


(defun open-chatgpt-query-in-new-browser-window (query &optional use-gpt-4)
  "Send a QUERY to ChatGPT and open the result in a new browser window.
With a prefix argument USE-GPT-4, use GPT-4 instead of GPT-4-turbo."
  (interactive "sEnter your ChatGPT query: \nP")
  (let* ((model (if use-gpt-4 "gpt-4" "gpt-4-turbo"))
         (url (concat "https://chat.openai.com/?q=" (url-hexify-string query)
                      "&model=" model)))
    (start-process "brave-browser" nil "brave-browser" "--new-window" url)))

(global-set-key (kbd "C-c C-g") 'open-chatgpt-query-in-new-browser-window)
;; Use-package

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))
;; org-edna

(use-package org-edna
  :ensure t
  :config
  (org-edna-mode 1))
;; Use-package

(use-package org-contrib
  :ensure t)
(require 'org-checklist)
(require 'ox-extra)
(ox-extras-activate '(ignore-headlines))
;; org-ql

(use-package org-ql)
;; org-ros
;; https://github.com/LionyxML/ros
;; https://chatgpt.com/c/6841f0e8-c080-8005-a3ed-fc6d67a76e19

(with-eval-after-load 'org-ros
  (defun org-ros ()
    "Screenshots an image to an org-file, prompting for filename with path completion."
    (interactive)
    (if buffer-file-name
        (progn
          (message "Waiting for region selection with mouse...")
          (let* ((default-filename (concat
                                     (file-name-nondirectory buffer-file-name)
                                     "_"
                                     (format-time-string "%Y%m%d_%H%M%S")
                                     ".png"))
                 (filepath (read-file-name
                            "Save screenshot as: "
                            (file-name-directory buffer-file-name)
                            nil nil
                            default-filename)))
            ;; ensure .png extension
            (unless (string-suffix-p ".png" filepath)
              (setq filepath (concat filepath ".png")))
            ;; ensure parent directory exists
            (make-directory (file-name-directory filepath) t)
            ;; capture screenshot
            (cond ((executable-find org-ros-primary-screencapture)
                   (call-process org-ros-primary-screencapture nil nil nil org-ros-primary-screencapture-switch filepath))
                  ((executable-find org-ros-secondary-screencapture)
                   (call-process org-ros-secondary-screencapture nil nil nil org-ros-secondary-screencapture-switch filepath))
                  ((executable-find org-ros-windows-screencapture)
                   (start-process "powershell" "*PowerShell*" "powershell.exe"
                                  "-File" (expand-file-name "./printsc.ps1" org-ros-dir) filepath)))
            ;; insert Org link
            (insert (format "[[file:%s][%s]]" filepath (file-name-nondirectory filepath)))
            (org-display-inline-images t t))
          (message "File created and linked..."))
      (message "You're in a not saved buffer! Save it first!"))))
;; ox-pandoc

(use-package ox-pandoc
  :after org
  :config
  (setq org-pandoc-options-for-docx '((standalone . nil)))
  )
;; Use-package

(use-package savehist)
;; snakemake-mode

(use-package snakemake-mode)
(defcustom snakemake-indent-field-offset nil
  "Offset for field indentation."
  :type 'integer)

(defcustom snakemake-indent-value-offset nil
  "Offset for field values that the line below the field key."
  :type 'integer)
;; Tree-sitter

;; Install and configure tree-sitter
(use-package tree-sitter
  :ensure t
	 )

;; Install and configure tree-sitter-langs
(use-package tree-sitter-langs
  :ensure t
  :after tree-sitter
  :config
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))


(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(defun disable-tree-sitter-for-org-mode ()
  (when (eq major-mode 'org-mode)
    (tree-sitter-mode -1)))

(add-hook 'tree-sitter-mode-hook #'disable-tree-sitter-for-org-mode)
;; vc-use-package

(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)
;; Use-package

(use-package vertico)
;; Use `consult-completion-in-region' if Vertico is enabled.

;; Otherwise use the default `completion--in-region' function.
(setq completion-in-region-function
      (lambda (&rest args)
        (apply (if vertico-mode
                   #'consult-completion-in-region
                 #'completion--in-region)
               args)))
;; other

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(define-key vertico-map (kbd "TAB") #'minibuffer-complete)
(define-key vertico-map (kbd "C-n") #'vertico-next)
(define-key vertico-map (kbd "C-p") #'vertico-previous)
;; Ensure you have these packages installed
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

(use-package marginalia
  :ensure t
  :after vertico
  :init
  (marginalia-mode))

(use-package orderless
  :ensure t
  :init
  ;; Customize completion styles to include orderless
  (setq completion-styles '(orderless basic))
  ;; Optionally configure completion categories
  (setq completion-category-defaults nil)
  (setq completion-category-overrides '((file (styles basic partial-completion)))))

(use-package savehist
  :ensure t
  :init
  (savehist-mode))

(use-package consult
  :ensure t
  :bind (("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)
         ("C-s" . consult-line)
         ("M-g g" . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("C-M-l" . consult-imenu)
         :map minibuffer-local-map
         ("M-r" . consult-history))
  :init
  (setq register-preview-delay 0
        register-preview-function #'consult-register-preview)
  ;; Optionally configure preview
  (autoload 'consult-register-window "consult")
  (setq consult-register-window-function #'consult-register-window)
  ;; Optionally configure narrowing key
  (setq consult-narrow-key "<"))

;; Enable vertico-directory for better directory navigation
(use-package vertico-directory
  :ensure nil
  :load-path "path/to/vertico-directory"
  :after vertico
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)))

;; Example configuration for more intuitive completion cycling
(define-key vertico-map (kbd "TAB") #'minibuffer-complete)
(define-key vertico-map (kbd "C-n") #'vertico-next)
(define-key vertico-map (kbd "C-p") #'vertico-previous)
;; Use-package

(use-package vterm
  :bind* (:map vterm-mode-map
               ("C-z" . vterm-undo)
               ("C-v" . vterm-yank))
  :init
  (add-hook 'vterm-mode-hook (lambda () (setq-local cua-mode nil)))
  :config
  (setq vterm-max-scrollback 100000)
  (custom-set-faces
   '(vterm-color-blue ((t (:foreground "#477EFC" :background "#477EFC"))))))
;; Use-package

(use-package web-mode
  :mode ("\\.phtml\\'"
         "\\.tpl\\.php\\'"
         "\\.[agj]sp\\'"
         "\\.as[cp]x\\'"
         "\\.erb\\'"
         "\\.mustache\\'"
         "\\.djhtml\\'"
         "\\.html?\\'"))
;; yaml

(use-package yaml-mode)
;; Use-package

(use-package yasnippet
  :init
  ;; Ensure the snippets directory exists
  (let ((snippets-dir "~/.emacs.d/snippets"))
    (unless (file-directory-p snippets-dir)
      (make-directory snippets-dir t)))

  ;; Dynamically add subdirectories in ~/.emacs.d/snippets to yas-snippet-dirs
  (setq yas-snippet-dirs
        (directory-files "~/.emacs.d/snippets" t "^[^.]+")) ; Directories only
  :config
  (yas-global-mode 1) ; Enable yasnippet globally
  (define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-expand))
;; (use-package yasnippet
;;   :init
;;   ;; Dynamically add subdirectories in ~/.emacs.d/snippets to yas-snippet-dirs
;;   (setq yas-snippet-dirs
;;         (directory-files "~/.emacs.d/snippets" t "^[^.]+")) ; Directories only, no append needed
;;    :config
;;   (yas-global-mode 1) ; Enable yasnippet globally
;;   (define-key yas-minor-mode-map (kbd "<C-tab>") 'yas-expand))
(defun my-org-mode-hook ()
  (setq-local yas-buffer-local-condition
              '(not (org-in-src-block-p t))))
(add-hook 'org-mode-hook #'my-org-mode-hook)
;; Prevent company mode during expansions

(add-hook 'yas-before-expand-snippet-hook (lambda () (setq-local company-backends nil)))
(add-hook 'yas-after-exit-snippet-hook    (lambda () (kill-local-variable 'company-backends)))
;; Provide a setting to auto-expand snippets
;; :PROPERTIES:
;; :ID:       485a4034-bae9-4d46-bed1-e21bb23e258c
;; :END:

(setq require-final-newline nil)
(defun yas-auto-expand ()
  "Function to allow automatic expansion of snippets which contain a condition, auto."

  (when yas-minor-mode
    (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
      (yas-expand))))

(defun my-yas-try-expanding-auto-snippets ()
  (when yas-minor-mode
    (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
      (yas-expand))))

(add-hook 'post-command-hook #'my-yas-try-expanding-auto-snippets)
;; Magit

(use-package magit)
