# ============================================================
# AUTO-GENERATED — DO NOT EDIT DIRECTLY
# Edits will be overwritten on next org-babel tangle.
# 
# Source:  /home/jeszyman/repos/basecamp/basecamp.org
# Author:  Jeffrey Szymanski
# Tangled: 2026-03-24 10:57:46
# ============================================================

unset repoDIRs
repoDIRs=(data figures imgs method miscellaneous presentations pubs reports results testing tex)
for dir in "${repoDIRs[@]}"
do
    mkdir -p $repo/"$dir"
done
