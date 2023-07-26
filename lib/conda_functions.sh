# Functions
# :PROPERTIES:
# :header-args:bash: :tangle ./lib/conda_functions.sh
# :END:

function conda_update() {
      [[ "$1" =~ (-h|--help) || -z "$1" ]] && {
    cat <<EOF
Usage: conda_update <ENV YAML>
Updates a conda environment yaml file using mamba
EOF
    return
      }
      local file="${1}"
      source /opt/miniconda3/etc/profile.d/conda.sh
      conda activate base
      mamba env update --file $1
      echo "$1 Environment updated successfully!"
}
