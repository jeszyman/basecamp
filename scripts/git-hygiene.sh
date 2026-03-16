#!/usr/bin/env bash
# ============================================================
# AUTO-GENERATED — DO NOT EDIT DIRECTLY
# Edits will be overwritten on next org-babel tangle.
# 
# Source:  /home/jeszyman/repos/basecamp/basecamp.org
# Author:  Jeffrey Szymanski
# Tangled: 2026-03-16 10:19:36
# ============================================================

set -euo pipefail

TEMPLATE="${HOME}/repos/basecamp/resources/git/jeszyman_gitignore"
SOFT_LIMIT=5000000
HARD_LIMIT=50000000
CLAUDE_SETTINGS_LIMIT=51200
EXIT_CODE=0

print_usage() {
    cat <<EOF
USAGE: git-hygiene [OPTIONS] <PATH>

DESCRIPTION:
  Scans git repositories for hygiene issues. Outputs TSV to stdout:
  REPO<tab>CHECK<tab>SEVERITY<tab>DETAIL

ARGUMENTS:
  <PATH>    Single repo dir or parent dir containing repos

OPTIONS:
  --single          Treat PATH as a single repo (default: scan all subdirs)
  --template <path> Gitignore template path (default: ~/repos/basecamp/resources/git/jeszyman_gitignore)
  -h, --help        Show this help message

EXIT CODES:
  0  Clean — no issues found
  1  Warnings only
  2  Errors found
EOF
}

SINGLE=false

# Portable human-readable file size (numfmt fallback)
human_size() {
    local bytes="$1"
    if command -v numfmt &>/dev/null; then
        numfmt --to=iec "$bytes"
    elif [[ "$bytes" -ge 1073741824 ]]; then
        echo "$((bytes / 1073741824))G"
    elif [[ "$bytes" -ge 1048576 ]]; then
        echo "$((bytes / 1048576))M"
    elif [[ "$bytes" -ge 1024 ]]; then
        echo "$((bytes / 1024))K"
    else
        echo "${bytes}B"
    fi
}

emit() {
    local repo="$1" check="$2" severity="$3" detail="$4"
    printf '%s\t%s\t%s\t%s\n' "$repo" "$check" "$severity" "$detail"
    case "$severity" in
        error) EXIT_CODE=2 ;;
        warn)  [[ $EXIT_CODE -lt 2 ]] && EXIT_CODE=1 ;;
    esac
}

parse_args() {
    while [[ $# -gt 0 ]]; do
        case "$1" in
            -h|--help) print_usage; exit 0 ;;
            --single) SINGLE=true; shift ;;
            --template) TEMPLATE="$2"; shift 2 ;;
            *) TARGET="$1"; shift ;;
        esac
    done
    if [[ -z "${TARGET:-}" ]]; then
        echo "Error: PATH argument required." >&2
        print_usage >&2
        exit 1
    fi
}

check_repo() {
    local repo_dir="$1"
    local repo_name
    repo_name="$(basename "$repo_dir")"

    # Verify this is a git repo
    if [[ ! -d "$repo_dir/.git" ]]; then
        return 0
    fi

    # --- excludesfile-missing ---
    local excludes_file
    excludes_file=$(git config --global core.excludesFile 2>/dev/null || true)
    if [[ -z "$excludes_file" ]]; then
        emit "$repo_name" "excludesfile-missing" "warn" "core.excludesFile not set globally"
    elif [[ ! -f "${excludes_file/#\~/$HOME}" ]]; then
        emit "$repo_name" "excludesfile-missing" "error" "core.excludesFile points to missing file: $excludes_file"
    fi

    # --- hookspath-missing ---
    local hooks_path
    hooks_path=$(git config --global core.hooksPath 2>/dev/null || true)
    if [[ -z "$hooks_path" ]]; then
        emit "$repo_name" "hookspath-missing" "warn" "core.hooksPath not set globally"
    elif [[ ! -d "${hooks_path/#\~/$HOME}" ]]; then
        emit "$repo_name" "hookspath-missing" "error" "core.hooksPath points to missing dir: $hooks_path"
    fi

    # --- large-file-tracked ---
    while IFS= read -r line; do
        [[ -z "$line" ]] && continue
        local hash file_path file_size
        hash=$(echo "$line" | awk '{print $2}')
        file_path=$(echo "$line" | sed 's/^[^\t]*\t//')
        file_size=$(git -C "$repo_dir" cat-file -s "$hash" 2>/dev/null || echo 0)
        if [[ "$file_size" -gt "$HARD_LIMIT" ]]; then
            emit "$repo_name" "large-file-tracked" "error" "${file_path} ($(human_size "$file_size")) exceeds hard limit $(human_size $HARD_LIMIT)"
        elif [[ "$file_size" -gt "$SOFT_LIMIT" ]]; then
            emit "$repo_name" "large-file-tracked" "warn" "${file_path} ($(human_size "$file_size")) exceeds soft limit $(human_size $SOFT_LIMIT)"
        fi
    done < <(git -C "$repo_dir" ls-files -s 2>/dev/null || true)

    # --- uncommitted-changes ---
    local status_output
    status_output=$(git -C "$repo_dir" status --porcelain 2>/dev/null || true)
    if [[ -n "$status_output" ]]; then
        local change_count
        change_count=$(echo "$status_output" | wc -l)
        emit "$repo_name" "uncommitted-changes" "info" "${change_count} uncommitted change(s)"
    fi

    # --- claude-settings-bloat ---
    local settings_file="$repo_dir/.claude/settings.local.json"
    if [[ -f "$settings_file" ]]; then
        local settings_size
        settings_size=$(stat -c%s "$settings_file" 2>/dev/null || stat -f%z "$settings_file" 2>/dev/null || echo 0)
        if [[ "$settings_size" -gt "$CLAUDE_SETTINGS_LIMIT" ]]; then
            emit "$repo_name" "claude-settings-bloat" "warn" "settings.local.json is $(human_size "$settings_size") (limit: $(human_size $CLAUDE_SETTINGS_LIMIT))"
        fi
    fi
}

main() {
    parse_args "$@"

    if [[ "$SINGLE" == "true" ]]; then
        if [[ ! -d "$TARGET" ]]; then
            echo "Error: $TARGET is not a directory." >&2
            exit 1
        fi
        check_repo "$TARGET"
    else
        if [[ ! -d "$TARGET" ]]; then
            echo "Error: $TARGET is not a directory." >&2
            exit 1
        fi
        for dir in "$TARGET"/*/; do
            [[ -d "$dir/.git" ]] || continue
            check_repo "${dir%/}"
        done
    fi

    exit "$EXIT_CODE"
}

main "$@"
