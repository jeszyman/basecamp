#!/usr/bin/env bash

# -----------------------------------------------------------------------------
# daily_git_commit.sh
# Stage, commit, pull, and push all repos under ~/repos/.
# Intended for cron or manual batch sync — commit message is today's date.
# -----------------------------------------------------------------------------

check_prerequisites() {
  if ! command -v git &>/dev/null; then
    echo "Error: git not found" >&2
    exit 1
  fi

  shopt -s nullglob
  local repo_dirs=(~/repos/*)
  shopt -u nullglob

  if [[ ${#repo_dirs[@]} -eq 0 ]]; then
    echo "Error: no repos at ~/repos/" >&2
    exit 1
  fi
}

sync_repos() {
  local date_today
  date_today=$(date +%F)

  for d in "$HOME"/repos/*; do
    [[ ! -d "$d" ]] && continue
    echo "$d"
    cd "$d" \
      && git add -A \
      && git commit -am "$date_today" \
      && git pull \
      && git submodule update --recursive \
      && git submodule update --remote \
      && git push
    cd "$OLDPWD"
  done
}

main() {
  check_prerequisites
  sync_repos
}

main "$@"
