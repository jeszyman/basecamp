# ============================================================
# AUTO-GENERATED — DO NOT EDIT DIRECTLY
# Edits will be overwritten on next org-babel tangle.
# 
# Source:  /home/jeszyman/repos/basecamp/basecamp.org
# Author:  Jeffrey Szymanski
# Tangled: 2026-03-16 21:27:15
# ============================================================

check_local_software(){
    [[ $# -eq 0 ]] && {
        printf "\n check_local_software\n Checks list of software in bash array and exits with error if not found\n \$1 = Bash array of software, e.g. try software=(bash, git)\n"
        return
    }
    software=$1
    for i in "${software[@]}"; do
        if command -v $i >/dev/null 2>&1 ; then
            echo "$i installed"
        else
            echo "$i not found, exiting"
            exit 1
        fi
    done
}
