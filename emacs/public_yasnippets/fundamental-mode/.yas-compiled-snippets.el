;;; Compiled snippets and support files for `fundamental-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'fundamental-mode
		     '(("yas.var" "\\${${1:<NUMBER>}: <${2:VALUE$$(upcase yas-text)}>} $0\n" "yas.var" 'auto nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/fundamental-mode/yas.var" nil nil)
		       ("usepackage" "(use-package ${1: <PACKAGE>}\n  :bind-keymap\n  (\"\" . cmd)\n  :config\n  :ensure t\n  (setq )\n  :hook\n)\n" "usepackage" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/fundamental-mode/usepackage" nil nil)
		       ("r.cmd" "args = commandArgs(trailingOnly = TRUE)\n$0=args[1]\n=args[2]\n" "r.cmd" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/fundamental-mode/r.cmd" nil nil)
		       ("nested" "$1\n\n$2\n\n$3\n" "nested" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/fundamental-mode/nested" nil nil)
		       ("function.py" "def ${1: <FUNCTION NAME>}(${2: <ARGUMENT 1>}):\n    $0\n" "function.py" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/fundamental-mode/function.py" nil nil)))


;;; Do not edit! File generated at Tue May  6 10:40:19 2025
