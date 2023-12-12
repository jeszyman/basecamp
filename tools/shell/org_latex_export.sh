#!/bin/sh

if [ $# -ne 2 ]; then
    echo "Usage: $0 <file> <org-id>"
    exit 1
fi

FILE="$1"
ORG_ID="$2"

/usr/bin/emacs --batch -l ~/.emacs.d/lisp/latex.el --eval "(progn
                        (require 'org)
                        (require 'org-id)
                        (setq org-confirm-babel-evaluate nil)
                        (find-file \"$FILE\")
                        (org-id-goto \"$ORG_ID\")
                        (org-latex-export-to-pdf nil t)
                        (kill-emacs))"
