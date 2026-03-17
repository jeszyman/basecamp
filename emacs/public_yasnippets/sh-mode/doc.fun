# ============================================================
# AUTO-GENERATED — DO NOT EDIT DIRECTLY
# Edits will be overwritten on next org-babel tangle.
# 
# Source:  /home/jeszyman/repos/basecamp/basecamp.org
# Author:  Jeffrey Szymanski
# Tangled: 2026-03-16 21:27:15
# ============================================================

# -*- mode: snippet --*
# key: doc.fun
# expand-env: ((yas-indent-line 'none))
# --
  [[ "\$1" =~ (-h|--help) || -z "\$1" ]] && {
    cat <<EOF
Usage: ${1: <FUNCTION NAME>} ${2: <USAGE>}
${3: <DESCRIPTION>}
EOF
    return
  }
