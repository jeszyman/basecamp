# ============================================================
# AUTO-GENERATED — DO NOT EDIT DIRECTLY
# Edits will be overwritten on next org-babel tangle.
# 
# Source:  /home/jeszyman/repos/basecamp/basecamp.org
# Author:  Jeffrey Szymanski
# Tangled: 2026-03-16 21:27:15
# ============================================================

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
#
######################
### symlink-by-csv ###
######################
#
# Purpose: generates symbolic links in bulk from a csv file
# Input: .csv file with source paths in column 1, dest paths in column 2
symlink_by_csv(){
    export IFS=","
    cat $1 | while read a b; do
        ln -s $a $b; done
}
