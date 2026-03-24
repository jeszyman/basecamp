#!/usr/bin/env bash
# ============================================================
# AUTO-GENERATED — DO NOT EDIT DIRECTLY
# Edits will be overwritten on next org-babel tangle.
# 
# Source:  /home/jeszyman/repos/basecamp/basecamp.org
# Author:  Jeffrey Szymanski
# Tangled: 2026-03-24 10:57:46
# ============================================================

set -o errexit   # abort on nonzero exitstatus
set -o nounset   # abort on unbound variable
set -o pipefail  # don't hide errors within pipes

# Script variables
input_read1="${1}"
output_read1="${2}"
output_read2="${3}"
outdir="${4}"

mkdir -p $outdir

input_read2="$(echo $input_read1 | sed "s/_R1/_R2/g")"

ln -sf --relative ${input_read1} ${output_read1}
ln -sf --relative ${input_read2} ${output_read2}
