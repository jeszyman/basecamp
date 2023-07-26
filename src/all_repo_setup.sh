# All repo setup
# :PROPERTIES:
# :ID:       c90aaf71-7311-48d8-98e8-221be4ebc290
# :END:

unset repoDIRs
repoDIRs=(data figures imgs method miscellaneous presentations pubs reports results testing tex)
for dir in "${repoDIRs[@]}"
do
    mkdir -p $repo/"$dir"
done
