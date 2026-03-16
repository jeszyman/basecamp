# ============================================================
# AUTO-GENERATED — DO NOT EDIT DIRECTLY
# Edits will be overwritten on next org-babel tangle.
# 
# Source:  /home/jeszyman/repos/basecamp/basecamp.org
# Author:  Jeffrey Szymanski
# Tangled: 2026-03-16 08:17:47
# ============================================================

unset repoDIRs
repoDIRs=(data figures imgs method miscellaneous presentations pubs reports results testing tex)
for dir in "${repoDIRs[@]}"
do
    mkdir -p $repo/"$dir"
done
