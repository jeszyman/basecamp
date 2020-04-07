#!/bin/bash
STAMP=$(date +%Y-%m-%d)
for d in /home/jeszyman/repos*
do
	[[ ! -d "$d" ]] && continue
	echo "$d"
	cd "$d"
	git pull && add * && git commit -am "$STAMP" && git push
	cd "$OLDPWD"
done
