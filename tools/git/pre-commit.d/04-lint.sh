#!/bin/sh
staged_files=$(git diff --cached --name-only --diff-filter=ACM)
[ -z "$staged_files" ] && exit 0

status=0

# Python / Snakemake
py_files=$(echo "$staged_files" | grep -E '\.(py|smk)$' || true)
if [ -n "$py_files" ]; then
    if command -v ruff >/dev/null 2>&1; then
        echo "$py_files" | xargs ruff check --force-exclude --no-fix 2>&1
        [ $? -ne 0 ] && status=1
    elif conda run -n basecamp ruff --version >/dev/null 2>&1; then
        echo "$py_files" | xargs conda run -n basecamp ruff check --force-exclude --no-fix 2>&1
        [ $? -ne 0 ] && status=1
    fi
fi

# Shell
sh_files=$(echo "$staged_files" | grep -E '\.sh$' || true)
if [ -n "$sh_files" ]; then
    if command -v shellcheck >/dev/null 2>&1; then
        echo "$sh_files" | xargs shellcheck -S error 2>&1
        [ $? -ne 0 ] && status=1
    fi
fi

# R
r_files=$(echo "$staged_files" | grep -E '\.[rR]$' || true)
if [ -n "$r_files" ]; then
    if command -v Rscript >/dev/null 2>&1 && Rscript -e 'library(lintr)' 2>/dev/null; then
        for f in $r_files; do
            errors=$(Rscript -e "cat(length(lintr::lint('$f', linters=lintr::linters_with_defaults(line_length_linter=NULL, object_name_linter=NULL)))")
            if [ "$errors" != "0" ]; then
                Rscript -e "lintr::lint('$f', linters=lintr::linters_with_defaults(line_length_linter=NULL, object_name_linter=NULL))" >&2
                status=1
            fi
        done
    fi
fi

exit $status
