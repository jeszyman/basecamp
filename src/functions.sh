# run docker interactively
# :PROPERTIES:
# :ID:       cff4499e-b420-4ae7-8a30-cbe04013db60
# :END:
#   #+name: docker_interactive

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


# # Check setup exit status
# if [ "$?" != "0" ]; then
#     echo "setup error" 1>&2
#     exit 1
# else
#     echo "Setup completed successfully"
# fi

# #+name: docker_check

# Check for docker environment
docker_check() {
if [ -f /.dockerenv ]; then
    echo "shell already in docker, exiting"
    exit 1
fi
}



# #+name:check_software_dependencies

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




# - bash functions
#   - docker interactive
#   #+name:

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

# [[file:~/repos/basecamp/src/shell/functions.sh][Common Process Functions]]
# :PROPERTIES:
# :CREATED:  [2020-04-14 Tue 09:05]
# :ID:       15071151-f519-4c7f-b21f-997d6d7b6bc5
# :END:
# #+name: check_local_software

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
