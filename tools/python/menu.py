#!/usr/bin/env python3
import os
import sp
import yaml


def call_shell_function(function_name, repo_dir):
    script_path = f"{repo_dir}/tools/shell/source.sh"
    command = f"source {script_path}; {function_name}"
    sp.run(command, shell=True, executable="/bin/bash")


# Load the configuration from the YAML file
with open("config.yaml", "r") as f:
    config = yaml.safe_load(f)

# Get the repo_dir from the configuration
repo_dir = config["repo_dir"]


def shell_while_loop():
    command = (
        "while sleep 2; do /usr/bin/emacs --batch -l org -eval "
        "'(org-babel-tangle-file \"card-rad-bio.org\")' && "
        "snakemake --dry-run --configfile config/${HOSTNAME}.yaml "
        "--cores 1 --snakefile workflow/int.smk; done"
    )
    try:
        sp.run(command, shell=True, executable="/bin/bash", check=True)
    except KeyboardInterrupt:
        print("\nLoop interrupted. Returning to menu.")


def submenu():
    while True:
        print("\nSubmenu")
        print("1. Option 1")
        print("2. Option 2")
        print("b. Back to main menu")

        choice = input("Enter number of choice: ")

        if choice == "1":
            print("Executing option 1 in submenu")
        elif choice == "2":
            print("Executing option 2 in submenu")
        elif choice.lower() == "b":
            print("Returning to main menu")
            break
        else:
            print("Invalid choice")


def main():
    while True:
        print("\nMPNST Repo Menu")
        print("1. Update conda")
        print("2. Execute function two")
        print("3. Open submenu")
        print("e Exit")

        choice = input("Enter number of choice: ")

        if choice == "1":
            call_shell_function("conda_update")
        elif choice == "2":
            call_shell_function("function_two")
        elif choice == "3":
            submenu()
        elif choice.lower() == "e":
            print("Exiting.")
            break
        else:
            print("Invalid choice")


if __name__ == "__main__":
    main()
