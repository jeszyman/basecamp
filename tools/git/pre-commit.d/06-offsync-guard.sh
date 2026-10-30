#!/bin/sh
# Refuse a commit that would delete changes made off the Syncthing machines
# (Claude Duet, github.com) from a staged file, in a Syncthing-shared repo.
# The check is ~/.claude/bin/repo-commit-absorb check-staged.
helper="$HOME/.claude/bin/repo-commit-absorb"
[ -x "$helper" ] || exit 0
exec "$helper" check-staged
