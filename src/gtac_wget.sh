# ============================================================
# AUTO-GENERATED — DO NOT EDIT DIRECTLY
# Edits will be overwritten on next org-babel tangle.
# 
# Source:  /home/jeszyman/repos/basecamp/basecamp.org
# Author:  Jeffrey Szymanski
# Tangled: 2026-03-16 08:17:47
# ============================================================

# Check for parameters, return usage if empty
if [ $# -ne 2 ]; then
    printf "\n usage: gtac_wget.sh <DIR PREFIX> <GTAC HTML>
           \n wget wrapper for WUSTL GTAC downloads
           \n"
elif [[ ! -r $1 ]]; then
    echo "Directory prefix at $1 is not readable."
else
    wget \
        --continue \
        --directory-prefix $1 \
        --execute robots=off \
        --force-directories \
        --no-host-directories \
        --no-parent \
        --recursive \
        --reject="index.html*" \
        --timestamping \
        $2
fi
