#!/bin/sh
threshold=200
org_files=$(git diff --cached --name-only --diff-filter=M | grep '\.org$' || true)
[ -z "$org_files" ] && exit 0

blocked=""
for f in $org_files; do
    added=$(git diff --cached --numstat -- "$f" | awk '{print $1}')
    deleted=$(git diff --cached --numstat -- "$f" | awk '{print $2}')
    [ -z "$added" ] && added=0
    [ -z "$deleted" ] && deleted=0
    net=$((deleted - added))
    if [ "$net" -gt "$threshold" ]; then
        blocked="$blocked  $f (net -${net} lines)\n"
    fi
done

if [ -n "$blocked" ]; then
    echo "ERROR: large net deletion in org file(s) — possible truncation:" >&2
    printf "$blocked" >&2
    echo "Use 'git commit --no-verify' if this is intentional." >&2
    exit 1
fi
exit 0
