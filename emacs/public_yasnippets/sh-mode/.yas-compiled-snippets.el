;;; Compiled snippets and support files for `sh-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'sh-mode
		     '(("variables" "variables(){\n    ${1:$$(replace-regexp-in-string \" \" \"_\" yas-text)}=\"\\${1${2:$$(when (and yas-moving-away-p (not (string= \"\" yas-text)))(concat \":-\" yas-text))}}\"\n    ${3:$$(replace-regexp-in-string \" \" \"_\" yas-text)}=\"\\${2${4:$$(when (and yas-moving-away-p (not (string= \"\" yas-text)))(concat \":-\" yas-text))}}\"\n}\n$0\n" "variables" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/sh-mode/variables" nil nil)
		       ("var" "$1=\"$\\{$2}\"$0\n" "var" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/sh-mode/var" nil nil)
		       ("usage" "print_usage(){\n    cat <<- EOF\n\n Usage: ${1:${1:SCRIPT NAME}$$(replace-regexp-in-string \" \" \"_\" yas-text)} [OPTIONS] ${2:${2:REQUIRED OPERAND}$$(upcase yas-text)}  ${3:${3:[OPTIONAL OPERAND]}$$(upcase yas-text)}\n\n ${4:<DESCRIPTION>}\n\n Options:\n   -h, --help    Show this help message and exit\n\n Arguements:\n   ${2:$(ignore-errors (format (yas-field-value 2)))} ${5: <OPERAND DESCRIPTION>}\n\n Example:\n   ${1:$(ignore-errors (format (yas-field-value 1)))} ${6: <EXAMPLE>}\n\nEOF\n}\n\n# See print usage caller yas\n# return_usage_any\n" "usage" nil nil
			((yas-indent-line 'fixed))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/sh-mode/usage" nil nil)
		       ("script" "#!/usr/bin/env bash\n\nhead1\n\n# Block comment here\n\nset -o errexit   # abort on nonzero exitstatus\nset -o nounset   # abort on unbound variable\nset -o pipefail  # don't hide errors within pipes\n\nusage\n\nvariables\n\nmain(){\n    variables /$1\n}\n\nmain \"$@\"\n" "script" nil nil nil "/home/jeszyman/.emacs.d/snippets/public_yasnippets/sh-mode/script" nil nil)
		       ("return_usage_any" "# Return usage if any arguments are provided\nif [[ $# -gt 0 ]]; then\n    print_usage\n    return 0\nfi\n$0\n" "return_usage_any" nil nil
			((yas-indent-line 'fixed))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/sh-mode/return_usage_any" nil nil)
		       ("must.src" "if [[ \"\\${BASH_SOURCE[0]}\" == \"\\${0}\" ]]; then\n  echo \"You must source this script\"\n  exit 1\nfi\n" "must.src" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/sh-mode/must.src" nil nil)
		       ("head1" "${1:$(make-string (+ (string-width yas-text) 12) ?\\#)}\n###   ${1:$$(capitalize yas-text)}   ###\n${1:$(make-string (+ (string-width yas-text) 12) ?\\#)}\n$0\n" "head1" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/sh-mode/head1" nil nil)
		       ("function" "${1:$$(replace-regexp-in-string \" \" \"_\" yas-text)}(){\n    local ${2: <VARIABLE NAME> }=\"\\${1}\"\n    $0\n}\n" "function" nil nil
			((yas-indent-line 'fixed))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/sh-mode/function" nil nil)
		       ("doc.fun" "  [[ \"\\$1\" =~ (-h|--help) || -z \"\\$1\" ]] && {\n    cat <<EOF\nUsage: ${1: <FUNCTION NAME>} ${2: <USAGE>}\n${3: <DESCRIPTION>}\nEOF\n    return\n  }\n" "doc.fun" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/sh-mode/doc.fun" nil nil)
		       ("comment.block" "`(let* ((input (read-string \"Comment block text: \"))\n        ;; wrap to 80 chars using fill-region\n        (fill-column 80)\n        (temp-buf (generate-new-buffer \"*yas-fill*\")))\n   ;; Fill text and split into lines\n   (prog1\n       (mapconcat (lambda (line) (concat \"# \" line)) \n                  (with-temp-buffer\n                    (insert input)\n                    (fill-region (point-min) (point-max))\n                    (split-string (buffer-string) \"\\n\"))\n                  \"\\n\")\n     (kill-buffer temp-buf)))`\n" "comment.block" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/sh-mode/comment.block" nil nil)
		       ("array.from.find" "${1:ARRAY NAME}=()\nwhile IFS=  read -r -d $'\\0'; do\n    $1+=(\"$REPLY\")\ndone < <(find ${2:FIND DIR} -name ${3:FIND TERM} -print0)\nprintf '%s\\n' \"\\${$1[@]}\"\n$0\n" "array.from.find" nil nil nil "/home/jeszyman/.emacs.d/snippets/public_yasnippets/sh-mode/array.from.find" nil nil)
		       ("80" "##########1##########2##########3##########4##########5##########6##########7##########8\n$0\n" "80" nil nil
			((yas-indent-line 'fixed))
			"/home/jeszyman/.emacs.d/snippets/public_yasnippets/sh-mode/80" nil nil)))


;;; Do not edit! File generated at Tue Jul  1 20:39:05 2025
