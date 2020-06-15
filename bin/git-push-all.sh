#!/bin/bash
d=$(date +%Y-%m-%d)
for d in $HOME/repos/*
do 
	[[ ! -d "$d" ]] && continue
	echo "$d"
	cd "$d"
	git add -A && git commit -a -m "$d" && git push
done
