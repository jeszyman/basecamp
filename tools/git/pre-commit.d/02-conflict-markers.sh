#!/bin/sh
files=$(git diff --cached --name-only --diff-filter=ACM)
[ -z "$files" ] && exit 0

conflict_files=""
for f in $files; do
    if git show ":$f" 2>/dev/null | grep -qE '^(<{7}|={7}|>{7})( |$)'; then
        conflict_files="$conflict_files $f"
    fi
done

if [ -n "$conflict_files" ]; then
    echo "ERROR: conflict markers found in staged files:" >&2
    for f in $conflict_files; do
        echo "  $f" >&2
    done
    exit 1
fi
exit 0
