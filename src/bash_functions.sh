#
######################
### symlink-by-csv ###
######################
#
# Purpose: generates symbolic links in bulk from a csv file
#
# Input:
# A .csv file of two columns,
#  Column 1 with source file paths
#  Column 2 with destination symbolic link paths
#
# Function:
symlink_by_csv(){
    # Set internal field separator
    export IFS=","
    # Take csv and pass to while/read
    cat $1 | while read a b; do
        ln -s $a $b; done
}
#
# Example:
# cd /tmp
# mkdir -p csv-symlink-test
# cd csv-symlink-test
# touch csv-for-links.csv
# touch file-to-link-from.txt
# echo "file-to-link-from.txt,path-to-link-to.txt" >> csv-for-links.csv
# echo "test link" > file-to-link-from.txt
# export IFS=","
# cat csv-for-links.csv | while read a b; do
#     ln -s $a $b ; done
# cat path-to-link-to.txt
# cd ../
# rm -rf csv-symlink-test
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
