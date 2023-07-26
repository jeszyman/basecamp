

# #+name: check_local_software

check_local_software(){
    # Usage
    ##
    ## Message
    usage(){
                #########1#########2#########3#########4#########5#########6#########7#########8
        printf "\n check_local_software
                \n Checks list of software in bash array and exits with error message if any
 software in the array is not found
                \n \$1 = Bash array of software, e.g. try software=(bash, git)
                \n "
    }
    ##
    ## Check
    [[ $# -eq 0 ]] && usage
    #
    # Variables
    software=$1
    #
    # Run
    for i in "${software[@]}"; do
    if command -v $i >/dev/null 2>&1 ; then
        echo "$i installed"
    else
        echo "$i not found, exiting"
        exit 1
    fi
done
}
