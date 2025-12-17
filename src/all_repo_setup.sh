unset repoDIRs
repoDIRs=(data figures imgs method miscellaneous presentations pubs reports results testing tex)
for dir in "${repoDIRs[@]}"
do
    mkdir -p $repo/"$dir"
done
