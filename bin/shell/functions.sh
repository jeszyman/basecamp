#+name:symlink-by-csv-script
#+begin_src sh
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
#+end_src
