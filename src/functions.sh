check_local_software(){
    if [[ $# -eq 0 ]] ; then
        printf "\n check_local_software 
              \n Checks list of software in array, exits with error message if not found
              \n \$1 = Bash array of software, e.g. software=(bash, git)
              \n "
    else    
        software=$1
        for i in "${software[@]}"; do
            if command -v $i >/dev/null 2>&1 ; then
                echo "$i installed"
            else
                echo "$i not found, exiting"
                exit 1
            fi
        done
    fi
}
#
# check_local_software
# unit test
#softwarecheck=(bash git)
#check_local_software $softwarecheck
