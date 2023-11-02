#!/usr/bin/env bash
d=$(date +%Y-%m-%d)
for d in $HOME/repos/*
do
	[[ ! -d "$d" ]] && continue
	echo "$d"
	cd "$d"
        git pull
	git add -A && git commit -m.
        git push
done
