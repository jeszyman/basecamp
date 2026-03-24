# ============================================================
# AUTO-GENERATED — DO NOT EDIT DIRECTLY
# Edits will be overwritten on next org-babel tangle.
# 
# Source:  /home/jeszyman/repos/basecamp/basecamp.org
# Author:  Jeffrey Szymanski
# Tangled: 2026-03-24 12:27:30
# ============================================================

# For unit testing
indir="test/benchmark"
output="test/analysis/qc/bench_agg.tsv"

if [ -f $output ]; then rm $output; fi

for file in $indir/*
do
    base=$(basename $file)
    cat $file | awk -v OFS='\t' -v var=$base 'NR>1 {print var,$0}' >> $output
done

sed -i '1i\process\tfloat_sec\trun_time\tmax_rss\tmax_vms\tmax_uss\tmax_pss\tio_in\tio_out\tmean_load\tcpu_time' $output
