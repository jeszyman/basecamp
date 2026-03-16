#!/usr/bin/env bash
# ============================================================
# AUTO-GENERATED — DO NOT EDIT DIRECTLY
# Edits will be overwritten on next org-babel tangle.
# 
# Source:  /home/jeszyman/repos/basecamp/basecamp.org
# Author:  Jeffrey Szymanski
# Tangled: 2026-03-09 10:37:13
# ============================================================

set -o errexit   # abort on nonzero exitstatus
set -o nounset   # abort on unbound variable
set -o pipefail  # don't hide errors within pipes

variables(){
    repo="${1}"
}

main(){
    variables $1
    git_init
}

git_init(){
    cd "${repo}"
    git init
    touch README.md
    git add README.md
    git commit -m "first commit"
    git branch -M master
    base=$(basename $repo)
    git remote add origin git@github.com:jeszyman/${base}.git
    git push -u origin master
    }

main "$@"
