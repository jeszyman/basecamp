(defmacro with-minibuffer-input (form &rest inputs)
  (declare (indent 1))
  `(minibuffer-with-setup-hook
       (lambda ()
         (minibuffer-input-provider ',inputs))
     ,form))

(defun minibuffer-input-provider (inputs)
  (let ((hook (make-symbol "hook")))
    (fset hook (lambda ()
                 (remove-hook 'post-command-hook hook)
                 (when inputs
                   (when (= 0 (minibuffer-depth))
                     (error "Too many inputs"))
                   (when (cdr inputs)
                     (add-hook 'post-command-hook hook))
                   (insert (pop inputs))
                   (exit-minibuffer))))
    (add-hook 'post-command-hook hook)))

(list "cons" "newelt" "balsh")

        '(this is a list(with a list inside of it "and another by quotes"))

  (message "The name of this buffer is: %s." (buffer-name))

(+ 3 4 5)

(+ (* 3 5)
   (* 47
      (- 20 6.8)) 12 )

(defvar A (* 5 5))
(+ A A)

(defvar B (+ A (* 5 A)))
B

(defvar (square2 (lambda (x) (* x x ))))
(square2 10)

(use-package lsp-mode
  :commands lsp
  :hook
  (sh-mode . lsp))

(setq lsp-bash-highlight-parsing-errors 1)
(setq lsp-bash-explanshell-endpoint 1)
(setq lsp-bash-glob-pattern 1)
