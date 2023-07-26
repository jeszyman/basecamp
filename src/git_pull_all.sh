# My bash functions
# :PROPERTIES:
# :ID:       569d1afa-e991-47b7-be15-3dd6364f5258
# :END:
# - http://mywiki.wooledge.org/BashGuide
# - [[file:~/repos/dotfiles/bashrc]]
# - Git pull all repos
#   #+name: git_pull_all

#!/bin/bash
#########1#########2#########3#########4#########5#########6#########7#########8
#                                                                              #
#                       Script to automate git pulls                           #
#             from a set of repositories within the same root dir              #
#                                                                              #
#########1#########2#########3#########4#########5#########6#########7#########8
#
# 1. Script Checks
#
## Check if git exist
command -v git >/dev/null 2>&1 # stop if git not found
if [ $? -eq 1 ]; then
    echo "Git not found"
    exit 1; fi
##
## Check if any repos exist
shopt -s nullglob
repo_dirs=(~/repos/*)
if [ ${#repo_dirs[@]} -eq 0 ]; then
    echo "No repos at ~/repos/"
    exit 1; fi
shopt -u nullglob
##
# 2. Pull Function
#
for d in $HOME/repos/*
do
    [[ ! -d "$d" ]] && continue
    echo "$d" &&
    cd "$d" &&
    git pull &&
    git submodule update --recursive &&
    git submodule update --remote &&
    cd "$OLDPWD"
done
#



# #+name: git_pull_all

#!/bin/bash
#########1#########2#########3#########4#########5#########6#########7#########8
#                                                                              #
#                       Script to automate git pulls                           #
#             from a set of repositories within the same root dir              #
#                                                                              #
#########1#########2#########3#########4#########5#########6#########7#########8
#
# 1. Script Checks
#
## Check if git exist
command -v git >/dev/null 2>&1 # stop if git not found
if [ $? -eq 1 ]; then
    echo "Git not found"
    exit 1; fi
##
## Check if any repos exist
shopt -s nullglob
repo_dirs=(~/repos/*)
if [ ${#repo_dirs[@]} -eq 0 ]; then
    echo "No repos at ~/repos/"
    exit 1; fi
shopt -u nullglob
##
# 2. Pull Function
#
for d in $HOME/repos/*
do
    [[ ! -d "$d" ]] && continue
    echo "$d" &&
    cd "$d" &&
    git pull &&
    git submodule update --recursive &&
    git submodule update --remote &&
    cd "$OLDPWD"
done
#
