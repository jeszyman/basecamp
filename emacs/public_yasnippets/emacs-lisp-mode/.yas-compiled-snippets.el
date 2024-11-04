;;; Compiled snippets and support files for `emacs-lisp-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'emacs-lisp-mode
		     '(("head4" ";;; ---   ${1:$$(capitalize yas-text)}   --- ;;;\n;;; ${1:$(make-string (+ (string-width yas-text) 12) ?\\-)} ;;;\n$0\n" "head4" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/emacs-lisp-mode/head4" nil nil)
		       ("head3" ";; ---   ${1:$$(capitalize yas-text)}   --- ;;\n;; ${1:$(make-string (+ (string-width yas-text) 12) ?\\-)} ;;\n$0\n" "head3" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/emacs-lisp-mode/head3" nil nil)
		       ("head2" "; ---   ${1:$$(capitalize yas-text)}   --- ;\n; ${1:$(make-string (+ (string-width yas-text) 12) ?\\-)} ;\n$0\n" "head2" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/emacs-lisp-mode/head2" nil nil)
		       ("head1" "${1:$(make-string (+ (string-width yas-text) 12) ?\\;)}\n;;;   ${1:$$(capitalize yas-text)}   ;;;\n${1:$(make-string (+ (string-width yas-text) 12) ?\\;)}\n" "head1" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/emacs-lisp-mode/head1" nil nil)
		       ("function" "(defun ${1: <FUNCTION NAME>} (${2: <VARIABLE 1 NAME>})\n  \"${3: <DOC STRING>}\n\n$2 ${4: <VARIABLE 1 DEFINITION>}\n\nExample usage:\n  ($1 ${5: <USAGE>})\"\n\n$0\n" "function" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/emacs-lisp-mode/function" nil nil)
		       ("80" ";;;;;;;;;1;;;;;;;;;2;;;;;;;;;3;;;;;;;;;4;;;;;;;;;5;;;;;;;;;6;;;;;;;;;7;;;;;;;;;8\n" "80" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/emacs-lisp-mode/80" nil nil)))


;;; Do not edit! File generated at Mon Nov  4 07:58:32 2024
