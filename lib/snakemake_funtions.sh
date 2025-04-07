# smk_unlock() Unlock a snakemake repo
# :PROPERTIES:
# :ID:       a5d747b8-a0d8-4622-95b0-cbdb8360920f
# :END:

smk_unlock() {
    [[ "$1" =~ (-h|--help) || -z "$1" ]] && {
	cat <<- EOF

Usage: smk_unlock <SNAKEFILE> [CONFIGFILE]

Wrapper for snakemake --unlock. --configfile defaults to ./config/${HOSTNAME}.yaml if no second argument is provided.

Example: smk_unlock ./workflow/analysis1.smk
        smk_unlock ./workflow/analysis1.smk ./config/custom_config.yaml

EOF
	return
    }

    local snakefile="${1}"
    local configfile="${2:-./config/${HOSTNAME}.yaml}"

    # Check if variables exist
    if [[ ! -f "$snakefile" ]]; then
	echo "Error: Snakefile '$snakefile' does not exist."
	return 1
    fi

    if [[ ! -f "$configfile" ]]; then
	echo "Error: Config file '$configfile' does not exist."
	return 1
    fi

  # Run
  snakemake --configfile "$configfile" \
    --cores 1 \
    --snakefile "$snakefile" \
    --unlock

  # Check exit code and provide error message
  if [ $? -ne 0 ]; then
    echo "Error: Snakemake run failed."
  fi
}

# smk_draw() Draw a rulegraph
# :PROPERTIES:
# :ID:       04b1ca54-c0d8-4bf6-bc58-ea066b8f5216
# :END:

smk_draw(){
  [[ "$1" =~ (-h|--help) || -z "$1" ]] && {
    cat <<EOF
Usage: smk_draw <CONFIG FILE> <SNAKEFILE>
Implements snakemake's rulegraph to make a DAG. DAG is saved in ./resources/<SNAKEFILE BASENAME>.pdf and .png
EOF
    return
  }
    local snakefile="${1}"
    local snakefile_basename="$(basename "$snakefile")"
    local out_pdf="./resources/${snakefile_basename%.*}_smk.pdf"
    local out_png="${out_pdf%.*}.png"
    snakemake --configfile ./config/${HOSTNAME}.yaml \
              --snakefile "$snakefile" \
              --cores 1 \
              --rerun-incomplete \
              --dry-run \
              --quiet \
              --rulegraph | tee >(dot -Tpdf > "$out_pdf") | dot -Tpng > "$out_png"
}

smk_draw(){
  [[ "$1" =~ (-h|--help) || -z "$1" ]] && {
    cat <<EOF
Usage: smk_draw <CONFIG FILE> <SNAKEFILE>
Implements snakemake's rulegraph to make a DAG. DAG is saved in ./resources/<SNAKEFILE BASENAME>.pdf and .png
EOF
    return
  }
  local snakefile="${1}"
  local snakefile_basename="$(basename "$snakefile")"
  local out_pdf="./resources/${snakefile_basename%.*}_smk.pdf"
  local out_png="${out_pdf%.*}.png"
  snakemake --configfile ./config/${HOSTNAME}.yaml \
            --snakefile "$snakefile" \
            --cores 1 \
            --rerun-incomplete \
            --dry-run \
            --quiet \
            --rulegraph | tee >(dot -Tpdf -Gsize=11,8.5 > "$out_pdf") | dot -Tpng > "$out_png"
}

# smk_dry() Dry run
# :PROPERTIES:
# :ID:       93e3fd9b-cbee-4a5d-8da5-8b54774e2890
# :END:

smk_dry(){
  [[ "$1" =~ (-h|--help) || -z "$1" ]] && {
    cat <<EOF

Usage: smk_dry <SNAKEFILE> [CONFIGFILE]

Wrapper for snakemake --dry-run. --configfile defaults to ./config/${HOSTNAME}.yaml if no second argument is provided.

Example: smk_dry ./workflow/analysis1.smk
         smk_dry ./workflow/analysis1.smk ./config/custom_config.yaml

EOF
	return
    }

  local snakefile="${1}"
  local configfile="${2:-./config/${HOSTNAME}.yaml}"

  # Check if variables exist
  if [[ ! -f "$snakefile" ]]; then
      echo "Error: Snakefile '$snakefile' does not exist."
      return 1
  fi

  if [[ ! -f "$configfile" ]]; then
      echo "Error: Config file '$configfile' does not exist."
      return 1
  fi

  # Run
  snakemake \
      --configfile ./config/${HOSTNAME}.yaml \
      --cores 4 \
      --dry-run \
      --rerun-incomplete \
      --resources concurrency=$(nproc) \
      --snakefile $snakefile

  # Check exit code and provide error message
  if [ $? -ne 0 ]; then
      echo "Error: Snakemake run failed."
  fi
}

# smk_forced() Forced run
# :PROPERTIES:
# :ID:       6b970126-99f6-426b-bcf9-f76d5fea5149
# :END:

smk_forced(){
  [[ "$1" =~ (-h|--help) || -z "$1" ]] && {
    cat <<EOF

Usage: smk_forced <SNAKEFILE> [CONFIGFILE]

Wrapper for a forced snakemake run. --configfile defaults to ./config/${HOSTNAME}.yaml if no second argument is provided. Runs all available cores.

Example: smk_forced ./workflow/analysis1.smk
         smk_forced ./workflow/analysis1.smk ./config/custom_config.yaml

EOF
    return
  }

  local snakefile="${1}"
  local configfile="${2:-./config/${HOSTNAME}.yaml}"
  local cores=$(nproc)

  # Check if variables exist
  if [[ ! -f "$snakefile" ]]; then
      echo "Error: Snakefile '$snakefile' does not exist."
      return 1
  fi

  if [[ ! -f "$configfile" ]]; then
      echo "Error: Config file '$configfile' does not exist."
      return 1
  fi

  # Run
  snakemake \
      --configfile "$configfile" \
      --cores "$cores" \
      --forceall \
      --rerun-incomplete \
      --snakefile "$snakefile"

  # Check exit code and provide error message
  if [ $? -ne 0 ]; then
      echo "Error: Snakemake run failed."
  fi
}

# smk_run() Normal run
# :PROPERTIES:
# :ID:       9e31b37b-0ecc-4157-8705-d9e4d3481607
# :END:

smk_run(){
  [[ "$1" =~ (-h|--help) || -z "$1" ]] && {
    cat <<EOF

Usage: smk_dry <SNAKEFILE> [CONFIGFILE]

Wrapper for normal snakemake. --configfile defaults to ./config/${HOSTNAME}.yaml if no second argument is provided. Runs all available cores.

Example: smk_run ./workflow/analysis1.smk
         smk_run ./workflow/analysis1.smk ./config/custom_config.yaml

EOF
    return
  }

  local snakefile="${1}"
  local configfile="${2:-./config/${HOSTNAME}.yaml}"
  local cores=$(nproc)

  # Check if variables exist
  if [[ ! -f "$snakefile" ]]; then
      echo "Error: Snakefile '$snakefile' does not exist."
      return 1
  fi

  if [[ ! -f "$configfile" ]]; then
      echo "Error: Config file '$configfile' does not exist."
      return 1
  fi

  # Run
  snakemake \
      --configfile "$configfile" \
      --cores "$cores" \
      --keep-going \
      --rerun-incomplete \
      --resources concurrency=100 \
      --snakefile "$snakefile"

  # Check exit code and provide error message
  if [ $? -ne 0 ]; then
      echo "Error: Snakemake run failed."
  fi
}
