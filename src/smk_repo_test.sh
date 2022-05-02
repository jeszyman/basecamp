eval "$(command conda 'shell.bash' 'hook' 2> /dev/null)"

conda activate snakemake

snakemake \
    --configfile $1 \
    --cores 4 \
    --use-singularity \
    --forceall \
    --snakefile $2
