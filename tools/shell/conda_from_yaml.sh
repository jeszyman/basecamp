# Update environment from yaml

#!/bin/bash

yaml="${1}"

# Name of the Conda environment
env_name=$(cat $yaml | grep name: | sed 's/^.*\ //g')

# Check if the environment already exists
if conda env list | grep -q "^$env_name\s"; then
    # Update the existing environment
    mamba env update --name "$env_name" --file "$yaml" --prune
else
    # Create a new environment
    mamba env create -n "$env_name" -f "$yaml"
fi
