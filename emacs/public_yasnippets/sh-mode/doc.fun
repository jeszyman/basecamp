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
