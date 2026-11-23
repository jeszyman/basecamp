#!/usr/bin/env bash
# Cut one filtered breast EM-seq alignment to the UXM U250 marker regions (padded 1 kb),
# rename contigs to UCSC names, and convert to a wgbs_tools pat file on the hg38 analysis-set genome.
set -euo pipefail
lib=$1
W=/mnt/data/projects/uxm-study
SRC=/mnt/gcs/mbergsag/breast/2nd-line/emseq/v4/bams/$lib.hg38.bwa_meth.coorsort.filt.bam
E=~/miniconda3/envs/emseq/bin
t0=$(date +%s)
if [ ! -s $W/bams/$lib.markers.bam.bai ]; then
  $E/samtools view -@ 2 -M -L $W/ref/markers_pad1000.numeric.bed -b -o $W/bams/$lib.markers.numeric.bam $SRC
  $E/samtools view -H $W/bams/$lib.markers.numeric.bam | sed "s/SN:\([0-9XYM]\)/SN:chr\1/g" | $E/samtools reheader - $W/bams/$lib.markers.numeric.bam > $W/bams/$lib.markers.bam
  $E/samtools index $W/bams/$lib.markers.bam
  rm $W/bams/$lib.markers.numeric.bam
fi
t1=$(date +%s)
PATH=$E:$PATH $E/python ~/repos/wgbs_tools/wgbstools bam2pat --genome hg38 --no_beta -f -o $W/pat $W/bams/$lib.markers.bam
t2=$(date +%s)
echo -e "$lib\tcut_s=$((t1-t0))\tpat_s=$((t2-t1))\treads=$($E/samtools view -c $W/bams/$lib.markers.bam)"
