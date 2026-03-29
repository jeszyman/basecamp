docker_interactive() {
    if [ -f /.dockerenv ]; then
        echo "shell already in docker, exiting"
        exit 1
    else
        account="USER INPUT"
        container="USER INPUT"
        read -p "docker account name: " account
        account="${account:=jeszyman}"
        read -p "container name: " container
        container="${container:=biotools}"
        case $HOSTNAME in
            (radonc-cancerbio) docker run -it \
                                      --env HOME=${HOME} \
                                      --hostname ${HOSTNAME} \
                                      --user $(id -u ${USER}) \
                                      --volume /home/:/home/ \
                                      --volume /mnt/:/mnt/ \
                                      --volume /tmp/:/tmp/ \
                                      $account/$container \
                                      /bin/bash;;
            (acl*) docker run -it \
                          --env HOME=${HOME} \
                          --hostname ${HOSTNAME} \
                          -v /drive3/:/drive3/ \
                          -v /duo4/:/duo4/ \
                          -v /home/:/home/ \
                          -v /tmp/:/tmp/ \
                          -u $(id -u ${USER}) \
                          $account/$container \
                          /bin/bash;;
            (ACL*) docker run -it \
                          --env HOME=${HOME} \
                          --hostname ${HOSTNAME} \
                          -v /home/:/home/ \
                          -v /duo4/:/duo4/ \
                          -u $(id -u ${USER}):$(id -g ${USER}) \
                          $account/$container \
                          /bin/bash;;
            (virtual-workstation*.gsc.wustl.edu) bsub -Is -q docker-interactive -a 'docker($account/'"$container"')' /bin/bash;;
            (*) docker run -it \
                       --env HOME=/home/${USER} \
                       --hostname ${HOSTNAME} \
                       --user $(id -u ${USER}) \
                       --volume /home/:/home/ \
                       --volume /mnt/:/mnt/ \
                       --volume /tmp/:/tmp/ \
                       $account/$container \
                       /bin/bash;;
        esac
    fi
}
# Check for docker environment
docker_check() {
if [ -f /.dockerenv ]; then
    echo "shell already in docker, exiting"
    exit 1
fi
}
#########1#########2#########3#########4#########5#########6#########7#########8
function check_dependencies {
    if [[ $# -eq 0 ]] || [[ $1 = -h ]] || [[ $1 = --help ]] ; then
        printf "\n check_dependencies
                     \n Uses WHICH command to confirm software install
                     \n \$1 = a bash array of software names, e.g. dependencies=("docker" "wget" "bash")
                     \n  "
    else
        local -n arr=$1
        for dependency in "${arr[@]}"
        do
            if [ "$(which $dependency)" = "" ] ; then
                echo "EARLY EXIT- $dependency needs to be installed and on PATH"
                exit 0
            else
                echo "$dependency installed"
            fi
        done
    fi
}
docker_interactive() {
    if [ -f /.dockerenv ]; then
        echo "shell already in docker, exiting"
        exit 1
    else
        account="USER INPUT"
        container="USER INPUT"
        read -p "docker account name: " account
        account="${account:=jeszyman}"
        read -p "container name: " container
        container="${container:=biotools}"
        case $HOSTNAME in
            (radonc-cancerbio) docker run -it \
                                      --env HOME=${HOME} \
                                      --hostname ${HOSTNAME} \
                                      --user $(id -u ${USER}) \
                                      --volume /home/:/home/ \
                                      --volume /mnt/:/mnt/ \
                                      --volume /tmp/:/tmp/ \
                                      $account/$container \
                                      /bin/bash;;
            (acl*) docker run -it \
                          --env HOME=${HOME} \
                          --hostname ${HOSTNAME} \
                          -v /drive3/:/drive3/ \
                          -v /duo4/:/duo4/ \
                          -v /home/:/home/ \
                          -u $(id -u ${USER}) \
                          $account/$container \
                          /bin/bash;;
            (ACL*) docker run -it \
                          --env HOME=${HOME} \
                          --hostname ${HOSTNAME} \
                          -v /home/:/home/ \
                          -v /duo4/:/duo4/ \
                          -u $(id -u ${USER}) \
                          $account/$container \
                          /bin/bash;;
            (virtual-workstation*.gsc.wustl.edu) bsub -Is -q docker-interactive -a 'docker($account/'"$container"')' /bin/bash;;
            (*) docker run -it \
                       --env HOME=/home/${USER} \
                       --hostname ${HOSTNAME} \
                       --user $(id -u ${USER}) \
                       --volume /home/:/home/ \
                       --volume /mnt/:/mnt/ \
                       --volume /tmp/:/tmp/ \
                       $account/$container \
                       /bin/bash;;
        esac
    fi
}
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
