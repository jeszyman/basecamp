# org_export_to_beamer.sh

emacsclient -e "(progn (find-file \"$1\") \
                       (org-id-goto \"$2\") \
                       (org-beamer-export-to-pdf nil t))"
