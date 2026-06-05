#!/bin/sh
staged_files=$(git diff --cached --name-only --diff-filter=ACM)
[ -z "$staged_files" ] && exit 0

# Defaults, overridable per-repo via git config
max_file_length=$(git config hooks.max-file-length 2>/dev/null || echo 350)
max_complexity=$(git config hooks.max-complexity 2>/dev/null || echo 10)
max_params=$(git config hooks.max-params 2>/dev/null || echo 4)

status=0

# --- File length (all code files) ---
code_files=$(echo "$staged_files" | grep -E '\.(py|smk|r|R|sh|el|js|ts)$' || true)
if [ -n "$code_files" ]; then
    for f in $code_files; do
        [ -f "$f" ] || continue
        lines=$(wc -l < "$f")
        if [ "$lines" -gt "$max_file_length" ]; then
            echo "QUALITY: $f is $lines lines (max $max_file_length)" >&2
            status=1
        fi
    done
fi

# --- Python / Snakemake: complexity + param count via ruff ---
py_files=$(echo "$staged_files" | grep -E '\.(py|smk)$' || true)
if [ -n "$py_files" ]; then
    RUFF=""
    if command -v ruff >/dev/null 2>&1; then
        RUFF="ruff"
    else
        for env in basecamp biotools org; do
            if conda run -n "$env" ruff version >/dev/null 2>&1; then
                RUFF="conda run -n $env ruff"
                break
            fi
        done
    fi

    if [ -n "$RUFF" ]; then
        # Write temp config with enforced thresholds
        tmp_cfg=$(mktemp /tmp/ruff-qg-XXXXXX.toml)
        cat > "$tmp_cfg" <<RUFF_EOF
[lint]
select = ["C901", "PLR0913"]

[lint.mccabe]
max-complexity = $max_complexity

[lint.pylint]
max-args = $max_params
RUFF_EOF

        echo "$py_files" | xargs $RUFF check --config "$tmp_cfg" --no-fix --force-exclude 2>&1
        rc=$?
        rm -f "$tmp_cfg"
        [ $rc -ne 0 ] && status=1
    fi
fi

exit $status
