function conda_update() {
    if [[ "$1" =~ ^(-h|--help)$ ]] || [[ -z "$1" ]]; then
        cat <<EOF
Usage: conda_update <ENV YAML>
Updates a conda environment yaml file using mamba
EOF
        return
    fi
    local file="$1"
    source /opt/miniconda3/etc/profile.d/conda.sh
    conda activate base || { echo "Failed to activate base conda environment."; return; }
    if command -v mamba >/dev/null 2>&1; then
        mamba env update --file "$file" && echo "Environment updated successfully with $file!"
    else
        echo "Mamba is not installed. Install mamba or use conda instead."
    fi
}

function conda_update() {
    if [[ "$1" =~ ^(-h|--help)$ ]] || [[ -z "$1" ]]; then
        cat <<EOF
Usage: conda_update <ENV YAML> [ENV NAME]
Updates a conda environment yaml file using mamba. Optionally updates a specific named environment.
EOF
        return
    fi
    local file="$1"
    local env_name="${2:-}" # If $2 is not provided, env_name will be empty
    source /opt/miniconda3/etc/profile.d/conda.sh
    conda activate base || { echo "Failed to activate base conda environment."; return; }
    if command -v mamba >/dev/null 2>&1; then
        if [[ -n "$env_name" ]]; then
            mamba env update -n "$env_name" --file "$file" && echo "Environment '$env_name' updated successfully with $file!"
        else
            mamba env update --file "$file" && echo "Environment updated successfully with $file!"
        fi
    else
        echo "Mamba is not installed. Install mamba or use conda instead."
    fi
    conda deactivate
}
