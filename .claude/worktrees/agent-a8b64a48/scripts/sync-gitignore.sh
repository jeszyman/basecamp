#!/usr/bin/env bash
# ============================================================
# AUTO-GENERATED — DO NOT EDIT DIRECTLY
# Edits will be overwritten on next org-babel tangle.
# 
# Source:  /home/jeszyman/repos/basecamp/basecamp.org
# Author:  Jeffrey Szymanski
# Tangled: 2026-03-09 10:37:13
# ============================================================

set -euo pipefail

print_usage() {
    cat <<EOF
USAGE: sync-gitignore <REPO_DIR> <GITIGNORE_URL>

DESCRIPTION:
  Appends new entries from a remote base .gitignore into the local one,
  skipping duplicates and empty lines.

REQUIRED ARGUMENTS:
  <REPO_DIR>        Path to the local git repo
  <GITIGNORE_URL>   Raw URL of the base .gitignore to sync from

OPTIONS:
  -h, --help        Show this help message

EXAMPLES:
  sync-gitignore ~/repos/emacs https://raw.githubusercontent.com/jeszyman/basecamp/v1.1.0/git/jeszyman_gitignore

EOF
}

parse_args() {
    if [[ "${1:-}" == "-h" || "${1:-}" == "--help" ]]; then
        print_usage
        exit 0
    fi
    if [[ $# -lt 2 ]]; then
        echo "Error: Missing required arguments."
        print_usage
        exit 1
    fi
    declare -g repo_dir="$1"
    declare -g gitignore_url="$2"
}

sync-gitignore() {
    local target="$repo_dir/.gitignore"
    local tmp
    tmp=$(mktemp)

    curl -fsSL "$gitignore_url" | sed 's/\r$//' > "$tmp"

    touch "$target"
    # Ensure file ends with newline
    [[ -s "$target" ]] && tail -c1 "$target" | grep -q $'\n' || echo >> "$target"

    echo "" >> "$target"
    echo "# --- Base ignore sync $(date -u +%Y-%m-%d) ---" >> "$target"

    while IFS= read -r line; do
        [[ -z "$line" ]] && continue
        grep -Fxq "$line" "$target" || echo "$line" >> "$target"
    done < "$tmp"

    rm -f "$tmp"
    echo "Updated $target"
}

main() {
    parse_args "$@"
    sync-gitignore
}

main "$@"
