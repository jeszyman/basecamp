#!/bin/bash
# Check for parameters, return usage if empty
if [[ $# -eq 0 ]] || [[ $1 == "h" ]] ; then
   printf "\n usage: smk_repo_test.sh config-file snakefile
           \n"
   else
eval "$(command conda 'shell.bash' 'hook' 2> /dev/null)"

conda activate snakemake

snakemake \
    --configfile $1 \
    --cores 4 \
    --use-singularity \
    --forceall \
    --snakefile $2
       
fi
