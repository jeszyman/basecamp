;;; Compiled snippets and support files for `sh-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'sh-mode
		     '(("variables" "variables(){\n    ${1:$$(replace-regexp-in-string \" \" \"_\" yas-text)}=\"\\${1${2:$$(when (and yas-moving-away-p (not (string= \"\" yas-text)))(concat \":-\" yas-text))}}\"\n    ${3:$$(replace-regexp-in-string \" \" \"_\" yas-text)}=\"\\${2${4:$$(when (and yas-moving-away-p (not (string= \"\" yas-text)))(concat \":-\" yas-text))}}\"\n}\n$0\n" "variables" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/public_yasnippets/sh-mode/variables" nil nil)
		       ("var" "$1=\"$\\{$2}\"$0\n" "var" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/public_yasnippets/sh-mode/var" nil nil)
		       ("usage" "print_usage(){\n    cat <<- EOF\n\n Usage: ${1:${1:SCRIPT NAME}$$(replace-regexp-in-string \" \" \"_\" yas-text)} [OPTIONS] ${2:${2:REQUIRED OPERAND}$$(upcase yas-text)}  ${3:${3:[OPTIONAL OPERAND]}$$(upcase yas-text)}\n \n ${4:<DESCRIPTION>}\n \n Options:\n   -h, --help    Show this help message and exit\n\n Arguements:\n   ${2:$(ignore-errors (format (yas-field-value 2)))} ${5: <OPERAND DESCRIPTION>} \n\n Example:\n   ${1:$(ignore-errors (format (yas-field-value 1)))} ${6: <EXAMPLE>} \n\nEOF\n}\n\n# Check if any arguments are provided\nif [[ $# -eq 0 ]]; then\n    print_usage\n    exit 1\nfi\n\n# Check for help flag\nif [[ $# -eq 1 && (\"\\$1\" == \"-h\" || \"\\$1\" == \"--help\") ]]; then\n    print_usage\n    exit 0\nfi\n\n# Check if the correct number of arguments are provided\nif [[ $# -lt 2 ]]; then\n    print_usage\n    exit 1\nfi\n" "usage" nil nil
			((yas-indent-line 'fixed))
			"/home/jeszyman/.emacs.d/public_yasnippets/sh-mode/usage" nil nil)
		       ("script" "#!/usr/bin/env bash\n\nhead1\n\n# Block comment here\n\nset -o errexit   # abort on nonzero exitstatus\nset -o nounset   # abort on unbound variable\nset -o pipefail  # don't hide errors within pipes\n\nusage\n\nvariables\n\nmain(){\n    variables /$1\n}\n\nmain \"$@\"\n" "script" nil nil nil "/home/jeszyman/.emacs.d/public_yasnippets/sh-mode/script" nil nil)
		       ("must.src" "if [[ \"\\${BASH_SOURCE[0]}\" == \"\\${0}\" ]]; then\n  echo \"You must source this script\"\n  exit 1\nfi\n" "must.src" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/public_yasnippets/sh-mode/must.src" nil nil)
		       ("head1" "${1:$(make-string (+ (string-width yas-text) 12) ?\\#)}\n###   ${1:$$(capitalize yas-text)}   ###\n${1:$(make-string (+ (string-width yas-text) 12) ?\\#)}\n$0\n" "head1" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/public_yasnippets/sh-mode/head1" nil nil)
		       ("function" "${1:$$(replace-regexp-in-string \" \" \"_\" yas-text)}(){\n    local ${2: <VARIABLE NAME> }=\"\\${1}\"\n    $0\n}\n" "function" nil nil
			((yas-indent-line 'fixed))
			"/home/jeszyman/.emacs.d/public_yasnippets/sh-mode/function" nil nil)
		       ("doc.fun" "  [[ \"\\$1\" =~ (-h|--help) || -z \"\\$1\" ]] && {\n    cat <<EOF\nUsage: ${1: <FUNCTION NAME>} ${2: <USAGE>}\n${3: <DESCRIPTION>}\nEOF\n    return\n  }\n" "doc.fun" nil nil
			((yas-indent-line 'none))
			"/home/jeszyman/.emacs.d/public_yasnippets/sh-mode/doc.fun" nil nil)
		       ("array.from.find" "${1:ARRAY NAME}=()\nwhile IFS=  read -r -d $'\\0'; do\n    $1+=(\"$REPLY\")\ndone < <(find ${2:FIND DIR} -name ${3:FIND TERM} -print0)\nprintf '%s\\n' \"\\${$1[@]}\"\n$0\n" "array.from.find" nil nil nil "/home/jeszyman/.emacs.d/public_yasnippets/sh-mode/array.from.find" nil nil)))


;;; Do not edit! File generated at Mon Aug  5 10:58:40 2024
