# No message simple git commit to github
# :PROPERTIES:
# :ID:       a97c46fe-8a23-4fa1-b9e2-f0edc29ec7f4
# :END:

#!/usr/bin/env bash

if [ $# -ne 1 ];
then
    printf "\n usage: no_message_commit <REPO DIR>
    \n Performs simple stage all and commit to github function for given directory
    \n "
else
    if ! [ -d $1 ];
       then
           echo "No repo at $1"
    else
        cd $1
        git pull
        git add -A
        git commit -m.
        git push
    fi
fi
