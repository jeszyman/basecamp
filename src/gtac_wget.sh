# [[elisp:(with-minibuffer-input (call-interactively 'woman) "wget")][wget]]

# Check for parameters, return usage if empty
if [ $# -ne 2 ]; then
    printf "\n usage: gtac_wget.sh <DIR PREFIX> <GTAC HTML>
           \n wget wrapper for WUSTL GTAC downloads
           \n"
elif [[ ! -r $1 ]]; then
    echo "Directory prefix at $1 is not readable."
else
    wget \
        --continue \
        --directory-prefix $1 \
        --execute robots=off \
        --force-directories \
        --no-host-directories \
        --no-parent \
        --recursive \
        --reject="index.html*" \
        --timestamping \
        $2
fi
