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
