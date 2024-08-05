;;; Compiled snippets and support files for `python-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'python-mode
		     '(("script" "#!/usr/bin/env python3\n\"\"\"\n$1\n\"\"\"\n\n# ---   Load Packages   --- #\n# ------------------------- #\n\nimport argparse\n\n# ---   Load Inputs   --- #\n# ----------------------- #\n\ndef load_inputs():\n    parser = argparse.ArgumentParser(description=__doc__)\n    parser.add_argument(\"--your_option\", type=str, default=\"\", help=\"Description of option\")\n\n    args = parser.parse_args()\n\n    return args\n\n# ---   Main   --- #\n# ---------------- #\n\ndef main():\n\n# ---   Functions   --- #\n# --------------------- #\n\n# ---   Main Guard   --- #\n# ---------------------- #\n\nif __name__ == \"__main__\":\n    main()\n" "script" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/public_yasnippets/python-mode/script" nil nil)
		       ("head2" "# ---   ${1:$$(capitalize yas-text)}   --- #\n# ${1:$(make-string (+ (string-width yas-text) 12) ?\\-)} #\n$0\n" "head2" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/public_yasnippets/python-mode/head2" nil nil)
		       ("head1" "${1:$(make-string (+ (string-width yas-text) 12) ?\\#)}\n###   ${1:$$(capitalize yas-text)}   ###\n${1:$(make-string (+ (string-width yas-text) 12) ?\\#)}\n" "head1" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/public_yasnippets/python-mode/head1" nil nil)
		       ("function" "def ${1:FUNCTIONNAME$$(replace-regexp-in-string \" \" \"_\" yas-text)}(${2:PARAM$$(replace-regexp-in-string \" \" \"_\" yas-text)}):\n    \"\"\"\n    ${3: <DOC STRING>}\n\n    \"\"\"\n    $0\n" "function" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/public_yasnippets/python-mode/function" nil nil)
		       ("80" "#########1#########2#########3#########4#########5#########6#########7#########8\n$0\n" "80" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/public_yasnippets/python-mode/80" nil nil)))


;;; Do not edit! File generated at Mon Aug  5 10:58:40 2024
