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
      --snakefile $snakefile

  # Check exit code and provide error message
  if [ $? -ne 0 ]; then
      echo "Error: Snakemake run failed."
  fi
}

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

smk_touch() {
  [[ "$1" =~ (-h|--help) || -z "$1" ]] && {
    cat <<EOF

Usage: smk_touch <SNAKEFILE> [OPTIONS]

Wrapper for snakemake with optional touch arguments.
--configfile defaults to ./config/${HOSTNAME}.yaml if no config file is explicitly provided.
Runs all available cores. Additional --touch arguments are optional and can be multiple.

Example: smk_touch ./workflow/analysis1.smk
         smk_touch ./workflow/analysis1.smk --config ./config/custom_config.yaml
         smk_touch ./workflow/analysis1.smk --touch /path/to/file1 --touch /path/to/file2

EOF
    return
  }

  local snakefile="${1}"
  shift 1

  local configfile="./config/${HOSTNAME}.yaml"
  local cores=$(nproc)
  local touch_args=()

  while (( "$#" )); do
    case "$1" in
      --config)
        configfile="${2}"
        shift 2
        ;;
      --touch)
        touch_args+=("$2")
        shift 2
        ;;
      *)
        echo "Error: Unexpected argument '$1'."
        return 1
        ;;
    esac
  done

  # Check if variables exist
  if [[ ! -f "$snakefile" ]]; then
      echo "Error: Snakefile '$snakefile' does not exist."
      return 1
  fi

  if [[ ! -f "$configfile" ]]; then
      echo "Error: Config file '$configfile' does not exist."
      return 1
  fi

  local touch_str=""
  for arg in "${touch_args[@]}"; do
    touch_str+="--touch $arg "
  done

  # Run
  snakemake \
      --configfile "$configfile" \
      --cores "$cores" \
      $touch_str \
      --snakefile "$snakefile"

  # Check exit code and provide error message
  if [ $? -ne 0 ]; then
      echo "Error: Snakemake run failed."
      return 1
  fi
}

smk_touch() {
  [[ "$1" =~ (-h|--help) || -z "$1" ]] && {
    cat <<EOF

Usage: smk_touch <SNAKEFILE> [OPTIONS]

Wrapper for snakemake with optional touch arguments.
--configfile defaults to ./config/${HOSTNAME}.yaml if no config file is explicitly provided.
Runs all available cores. Additional --touch arguments are optional and can be multiple.

Example: smk_touch ./workflow/analysis1.smk
         smk_touch ./workflow/analysis1.smk --config ./config/custom_config.yaml
         smk_touch ./workflow/analysis1.smk --touch "/path/to/files*"

EOF
    return
  }

  local snakefile="${1}"
  shift 1

  local configfile="./config/${HOSTNAME}.yaml"
  local cores=$(nproc)
  local touch_flag=""
  local touch_value=""

  while (( "$#" )); do
    case "$1" in
      --config)
        configfile="${2}"
        shift 2
        ;;
      --touch)
        touch_flag="$1"
        touch_value="$2"
        shift 2
        ;;
      *)
        echo "Error: Unexpected argument '$1'."
        return 1
        ;;
    esac
  done

  # Check if variables exist
  if [[ ! -f "$snakefile" ]]; then
      echo "Error: Snakefile '$snakefile' does not exist."
      return 1
  fi

  if [[ ! -f "$configfile" ]]; then
      echo "Error: Config file '$configfile' does not exist."
      return 1
  fi

  # Construct the Snakemake command
  snakemake --configfile "$configfile" --cores "$cores" --snakefile "$snakefile" "${snakemake_args[@]}"

  # Check exit code and provide error message
  if [ $? -ne 0 ]; then
      echo "Error: Snakemake run failed."
      return 1
  fi
}

smk_touch() {
  [[ "$1" =~ (-h|--help) || -z "$1" ]] && {
    cat <<EOF

Usage: smk_touch <SNAKEFILE> [OPTIONS]

Wrapper for snakemake with optional touch arguments.
--configfile defaults to ./config/${HOSTNAME}.yaml if no config file is explicitly provided.
Runs all available cores. Additional --touch arguments are optional and can be multiple.

Example: smk_touch ./workflow/analysis1.smk
         smk_touch ./workflow/analysis1.smk --config ./config/custom_config.yaml
         smk_touch ./workflow/analysis1.smk --touch "/path/to/files*"

EOF
    return
  }

  local snakefile="${1}"
  shift 1

  local configfile="./config/${HOSTNAME}.yaml"
  local cores=$(nproc)
  local snakemake_args=()

  while (( "$#" )); do
    case "$1" in
      --config)
        configfile="${2}"
        shift 2
        ;;
      --touch)
        snakemake_args+=("$1" "$2")
        shift 2
        ;;
      *)
        echo "Error: Unexpected argument '$1'."
        return 1
        ;;
    esac
  done

  # Check if variables exist
  if [[ ! -f "$snakefile" ]]; then
      echo "Error: Snakefile '$snakefile' does not exist."
      return 1
  fi

  if [[ ! -f "$configfile" ]]; then
      echo "Error: Config file '$configfile' does not exist."
      return 1
  fi

  # Construct and run the Snakemake command
  snakemake --configfile "$configfile" --cores "$cores" --snakefile "$snakefile" "${snakemake_args[@]}"

  # Check exit code and provide error message
  if [ $? -ne 0 ]; then
      echo "Error: Snakemake run failed."
      return 1
  fi
}

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
      --snakefile "$snakefile"

  # Check exit code and provide error message
  if [ $? -ne 0 ]; then
      echo "Error: Snakemake run failed."
  fi
}
