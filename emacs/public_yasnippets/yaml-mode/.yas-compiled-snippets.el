;;; Compiled snippets and support files for `yaml-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'yaml-mode
		     '(("head2" "# ---   ${1:$$(capitalize yas-text)}   --- #\n# ${1:$(make-string (+ (string-width yas-text) 12) ?\\-)} #\n$0\n" "head2" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/yaml-mode/head2" nil nil)
		       ("comment.block" "nil\n" "comment.block" nil nil nil "/home/jeszyman/.emacs.d/snippets/public_yasnippets/yaml-mode/comment.block" nil nil)
		       ("80" "#########1#########2#########3#########4#########5#########6#########7#########8\n$0\n" "80" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/yaml-mode/80" nil nil)))


;;; Do not edit! File generated at Fri Jul 25 08:39:24 2025
