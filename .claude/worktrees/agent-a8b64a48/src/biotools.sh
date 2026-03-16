#########1#########2#########3#########4#########5#########6#########7#########8
#
if [ -f /.dockerenv ]; then
 echo "shell already in docker, exiting"
 exit 1
else
 account="jeszyman"
 container="biotools"
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
source ~/repos/card-rad-bio/src/setup.sh
