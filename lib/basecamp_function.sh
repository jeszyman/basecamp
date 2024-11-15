# Basecamp General Shell Functions
# connect_scrcpy

# Function: connect_scrcpy
# Usage: connect_scrcpy [-i DEVICE_IP] [-p DEVICE_PORT]

connect_scrcpy() {
    # Default device IP and port
    local DEVICE_IP="192.168.1.12"
    local DEVICE_PORT="5555"

    # Function to display usage information
    local usage
    usage() {
        echo "Usage: connect_scrcpy [-i DEVICE_IP] [-p DEVICE_PORT]"
        return 1
    }

    # Reset OPTIND in case getopts has been used previously in the shell
    OPTIND=1

    # Parse command-line options
    while getopts ":i:p:" opt; do
      case ${opt} in
        i )
          DEVICE_IP=$OPTARG
          ;;
        p )
          DEVICE_PORT=$OPTARG
          ;;
        \? )
          usage
          return 1
          ;;
      esac
    done
    shift $((OPTIND -1))

    # Check if adb is installed
    if ! command -v adb &> /dev/null; then
        echo "Error: 'adb' is not installed. Please install it and try again."
        return 1
    fi

    # Check if scrcpy is installed
    if ! command -v scrcpy &> /dev/null; then
        echo "Error: 'scrcpy' is not installed. Please install it and try again."
        return 1
    fi

    # Attempt to connect to the device
    echo "Connecting to device at ${DEVICE_IP}:${DEVICE_PORT}..."
    adb connect "${DEVICE_IP}:${DEVICE_PORT}" &> /dev/null

    if [ $? -ne 0 ]; then
        echo "Error: Failed to connect to the device at ${DEVICE_IP}:${DEVICE_PORT}."
        return 1
    fi

    # Start scrcpy in the background
    echo "Starting scrcpy..."
    scrcpy &

    if [ $? -ne 0 ]; then
        echo "Error: Failed to start scrcpy."
        return 1
    fi

    echo "Successfully connected and started scrcpy."
    return 0
}
# Simple alias-like functions as program launchers

launch() {
    (nohup "$@" >/dev/null 2>&1 &)
    exit
}

brave() { launch /usr/bin/brave-browser; }
chrome() { launch /usr/bin/brave-browser; }
inkscape() { launch /usr/bin/inkscape; }
okular() { launch /usr/bin/okular; }



emacs() {
    exec /usr/local/bin/emacsclient -c --no-wait &
    disown
    exit
}
# Volume control with amixer


# mute() Mute computer speakers and microphone
# :PROPERTIES:
# :ID:       318be8d0-cfce-495f-90cd-93942c04f3b6
# :END:

mute(){
    amixer set Master mute
    amixer set Capture nocap
}
# unmute()

unmute() {
    amixer set Master unmute
    amixer set Capture cap
}
# volume-up()

volume-up() {
    # Check if speakers are muted
    mute_status=$(amixer -D pulse get Master | grep -o '\[off\]')
    
    if [[ -n "$mute_status" ]]; then
        echo "WARNING: Speakers are currently muted. Unmute them to hear audio."
    else
	amixer -D pulse sset Master 5%+	    
    fi
}
# volume-down

volume-down() {
    # Check if speakers are muted
    mute_status=$(amixer -D pulse get Master | grep -o '\[off\]')
    
    if [[ -n "$mute_status" ]]; then
        echo "WARNING: Speakers are currently muted. Unmute them to hear audio."
    else
	amixer -D pulse sset Master 5%-
    fi
}
# emacs-start()

emacs-start() {
    print_usage(){
    cat <<- EOF

 Usage: emacs-start
 
 Invokes the emacsclient systemd daemon if not already running.
 
 Options:
   -h, --help    Show this help message and exit

 Arguements: This function takes no arguements. 

 Example:
   emacs-start   

EOF
}

    # Return usage if any arguments are provided
    if [[ $# -gt 0 ]]; then
        print_usage
        return 0
    fi

        # Check if emacsclient is running
    if pgrep -x "emacs" > /dev/null
    then
        echo "Emacs daemon is already running."
    else
        echo "Emacs daemon is not running. Starting it now..."
        systemctl start --user emacs
        if [ $? -eq 0 ]; then
            echo "Emacs daemon started successfully."
        else
            echo "Failed to start Emacs daemon. Please check your systemd configuration."
            return 1
        fi
    fi
    
}
# emacs-stop()

emacs-stop() {
    print_usage(){
    cat <<- EOF

 Usage: emacs-stop
 
 Terminates the emacsclient systemd daemon if running.
 
 Options:
   -h, --help    Show this help message and exit

 Arguements: This function takes no arguements. 

 Example:
   emacs-stop

EOF
}

    # Return usage if any arguments are provided
    if [[ $# -gt 0 ]]; then
        print_usage
        return 0
    fi

        # Check if emacsclient is running
    if pgrep -x "emacs" > /dev/null
    then
	systemctl stop --user emacs
    else
        echo "Emacs daemon is not running. Nothing to do."
    fi
    
}
# ubuntu-settings()

ubuntu-settings(){
    env XDG_CURRENT_DESKTOP=GNOME gnome-control-center sound & exit
}
# check_mnt() Check if mount is active
# :PROPERTIES:
# :ID:       c98f8d3a-bdf1-4c91-8ca2-7d4743dace59
# :END:

check_mnt(){
  [[ "$1" =~ (-h|--help) || -z "$1" ]] && {
    cat <<EOF
Usage: check_mnt <DIRECTORY>
Checks if directory is a mount point (must be top directory of mtn).
EOF
    return
  }
    local directory="${1}"
    if mountpoint -q "$directory"; then
        echo "The directory $directory is a mountpoint."
    else
        echo "The directory $directory is not a mountpoint."
    fi
}
# pomo() Pomodoro
# :PROPERTIES:
# :ID:       422747c3-9054-4022-bcc3-e67d0f6fb9aa
# :END:

function pomo() {
    arg1=$1
    shift
    args="$*"

    min=${arg1:?Example: pomo 15 Take a break}
    sec=$((min * 60))
    msg="${args:?Example: pomo 15 Take a break}"

    while true; do
        date '+%H:%M' && sleep "${sec:?}" && notify-send -u critical -t 0 -a pomo "${msg:?}"
    done
}
# convert_pdf_to_png()
# :PROPERTIES:
# :ID:       61cef427-947e-4d91-9dfe-8758ccbe3512
# :END:

convert_pdf_to_png() {
    print_usage() {
        cat <<- EOF

$(tput setaf 12)Usage: convert_pdf_to_png$(tput sgr0) $(tput setaf 11)<INPUT_FILE.pdf>$(tput sgr0)

Converts a PDF to a PNG with a white background.

Example: convert_pdf_to_png $(tput setaf 11)input.pdf$(tput sgr0)

Notes:
- /etc/ImageMagick-6/policy.xml policy change allows work with PDF
- allows file overwrite
- works from /tmp
- set to 600 dpi
EOF
    }

    local input_file="$1"

    if [ -z "$input_file" ]; then
        print_usage
        return
    fi

    local output_file=$(echo "$input_file" | sed 's/\.pdf$/.png/')
    convert -density 600 -background white -flatten "$input_file" "$output_file"

    echo "Conversion complete. Output saved to $output_file"

}
# convert_pdf_to_svg()
# :PROPERTIES:
# :ID:       98421984-2ff6-4ad8-bd7d-4b9f4810f1b1
# :END:

convert_pdf_to_svg() {
    print_usage() {
        cat <<- EOF
Usage: convert_pdf_to_svg <input-file.pdf>

Converts a PDF to an SVG with a white background.

Example: convert_pdf_to_svg input.pdf

Notes:
- Uses pdf2svg for conversion
- Allows file overwrite
- Works from /tmp
EOF
}
    local input_file="$1"

    if [ -z "$input_file" ]; then
	print_usage
        return
    fi

    local output_file=$(echo "$input_file" | sed 's/\.pdf$/.svg/')
    pdf2svg "$input_file" "$output_file"

    # Add a white background rectangle at the beginning of the SVG content
    sed -i '/<svg/a <rect width="100%" height="100%" fill="white"/>' "$output_file"

    echo "Conversion complete. Output saved to $output_file with a white background."
}
# cpout()
# :PROPERTIES:
# :ID:       4548b630-06a9-4480-8e11-6e690d05eabc
# :END:

cpout() {
    if [[ "$@" =~ (-h|--help) || -z "$1" ]]; then
        cat <<EOF

Usage: cpout <COMMAND>

Copies command stdout and stderr to clipboad using xclip.

EOF
        return
    fi
    "$@" 2>&1 | tee >(xclip -selection clipboard)
}
# docx_to_pdf()
# :PROPERTIES:
# :ID:       8240dc4a-a77f-438e-93a4-ed63d5828f7e
# :END:

docx_to_pdf() {
    local in_docx="$1"

    if [ -z "$in_docx" ]; then
	cat <<- EOF
	usage: docx_to_pdf <INPUT DOCX FILE>

	Converts a docx file to a pdf

	example: docx_to_pdf /tmp/test.docx
EOF
	return 1
    fi

    local out_pdf=$(echo "$in_docx" | sed 's/\.docx$/.pdf/')
    libreoffice --headless --convert-to pdf:writer_pdf_Export "$in_docx" --outdir $(dirname "$in_docx")

    echo "Conversion complete"
}
# emacs-latex()
# :PROPERTIES:
# :ID:       ca04cfb0-6ddf-4d33-9e64-81b2f16def79
# :END:

emacs-latex() {
    print_usage() {
	cat <<- EOF

    Usage: emacs-latex <ORG FILE> <ORG HEADER ID>

    Uses Emacs --batch export to compile LaTeX from a specific Org-mode header

    Example: emacs-latex ~/test.org cb3c499d-ed2f-4ee3-9965-ca3e1af81178

EOF
    }

    if [[ "$1" =~ ^(-h|--help)$ ]]; then
        print_usage
        return 0
    fi

    if [[ -z "$1" ]]; then
        print_usage
        return 1
    fi

    local file="$1"
    local id="$2"

    /usr/local/bin/emacs --batch -l "${HOME}/repos/basecamp/emacs/latex_init.el" --eval "(progn
                        (require 'org)
                        (require 'org-id)
                        (setq org-confirm-babel-evaluate nil)
                        (find-file \"$file\")
                        (org-id-goto \"$id\")
                        (org-latex-export-to-pdf nil t)
                        (kill-emacs))"

}
# emacs-save()
# :PROPERTIES:
# :ID:       a79a1266-7168-49da-861a-d06a497a44c8
# :END:

emacs-save(){
    [[ "$1" =~ (-h|--help) ]] && {
    cat <<EOF
Usage: emacs-save
Invokes emacsclient --eval '(save-some-buffers t)' to save all open buffers.
EOF
    return
      }
    emacsclient --eval '(save-some-buffers t)'
}
# find_in_files()
# :PROPERTIES:
# :ID:       bbe30f0b-9836-4459-bf8f-39dca3b38907
# :END:

find_in_files(){
    print_usage(){
        cat <<- EOF

usage: find_in_files <TERM> [<DIRECTORY>]

Use grep to search a term throughout text files of a directory. Searches current directory by default or specify.

examples:
find_in_files test
find_in_files test /path/to/dir

EOF
    }

    if [[ "$@" =~ (-h|--help) || -z "$1" ]]; then
        print_usage
        return
    fi

    local term="$1"
    local dir="${2:-.}" # Default to current directory if not provided

    grep -rnw "${dir}" -e "${term}"
}
# screenshot()
# :PROPERTIES:
# :ID:       f829815e-41cc-4180-b91e-07e00395f473
# :END:

screenshot(){
    print_usage(){
	cat <<- EOF

 Usage: screenshot

      Takes interactive screenshot with the scrot program and saves it to
      clipboard with xclip. The screenshot is also saved as a png file at
      /tmp/screenshot.png. NOTE the /tmp/screenshot.png file is overwritten for
      each use.

      Example: screenshot

EOF
    }

    if [[ "$*" =~ (-h|--help)  ]]; then
	print_usage
	return
    fi

    scrot -s -f --overwrite /tmp/screenshot.png && xclip -selection clipboard -t image/png -i /tmp/screenshot.png

}
# debug()
# :PROPERTIES:
# :ID:       8b0b016c-cdc5-4583-87d2-9b25a0b5fac4
# :END:

debug(){
    print_usage(){
	cat <<- EOF

 Usage: debug <FUNCTION NAME>

      Systematic debugging script for a bash function.
      NOTE: INCOMPLETE, work in progress.

      Example: debug screenshot

EOF
    }

    if [[ "$@" =~ (-h|--help) ]]; then
        print_usage
        return
    fi

    fun_name="$1"

    if [[ -z "$fun_name" ]]; then
	print_usage
	return
    fi

    alias | grep $fun_name

    # Check if the function exists and get its location
    function_info=$(declare -F "$fun_name")
    if [[ -n "$function_info" ]]; then
        function_location=$(echo "$function_info" | awk '{print $3}')
        echo "Function '$fun_name' is defined at: $function_location"
        read -p "Do you want to display the function? (y/n): " choice
        if [[ "$choice" =~ ^[Yy]$ ]]; then
            type "$fun_name"
        fi
    else
        echo "Function '$fun_name' not found."
    fi
}
# git_search_all()
# :PROPERTIES:
# :ID:       91c9ca9e-73ab-4a14-8087-96441b2fe0fc
# :END:

git_search_all(){
    print_usage(){
        cat <<- EOF

    Usage: git_search_all [options] <REGEXP> [<OPTIONAL OPERANDS>]

    Git search all content, all commits

    Performs a regular expression search through the text of all text files and
    all commits in a git repository.

    Example: git_search_all gc5mb

EOF
    }

    if [[ "$@" =~ (-h|--help) || -z "$1" ]]; then
        print_usage
        return
    fi

    local regexp="${1}"

    git grep $regexp $(git rev-list --all)
}
# ppt-to-pdf()



pptx-to-pdf() {
    print_usage(){
    cat <<- EOF

 Usage: pptx-to-pdf PPTX
 
 Converts a Powerpoint ppt file to PDF using unoconv. The PDF file will be
 written to the same location as the pptx, but with a .pdf extension. 

 Options:
   -h, --help    Show this help message and exit

 Arguements:
     PPTX

 Example:
   pptx-to-pdf test.pptx

EOF
}

    # Return usage if number of arguments != 1
    if [ $# -ne 1 ]; then
        print_usage
        return 1
    fi
    
    # Return usage if the argument is a help flag
    if [[ "$1" == "-h" || "$1" == "--help" ]]; then
        print_usage
        return 0
    fi
        
  unoconv -f pdf $1
  
}
# logout

logout() {
    print_usage(){
        cat <<- EOF

 Usage: logout 

 Logs out of i3 using i3lock. If emacsclient is running, will save all buffers prior to
 logout. 

 Options:
   -h, --help    Show this help message and exit

 Arguements: This function takes no arguements. 
       
 Example:
   logout  

EOF
    }

    # Return usage if any arguments are provided
    if [[ $# -gt 0 ]]; then
        print_usage
        return 0
    fi

    pgrep emacsclient && emacs-save && i3lock -c 000000 || i3lock -c 000000
 
}
# ls-size()

ls-size(){
    print_usage(){
        cat <<- EOF
	
 Usage: ls-size

 Lists files in the current directory, sorted by size in descending order, with file
 size in megabases.

 This function takes no arguments.

 Options:
   -h, --help    Show this help message and exit

 Example:
   ls-size
EOF
    }

    # Return usage if any arguments are provided
    if [[ $# -gt 0 ]]; then
        print_usage
        return 0
    fi
    
    ls -lrS1 --block-size=M
}
# ls_recursive()
# :PROPERTIES:
# :ID:       af1385b3-6baf-4e86-80e7-6a178f7623c1
# :END:



ls_recursive() {
    print_usage() {
        cat <<- EOF

    Usage: ls_recursive <REGEXP> [LEVEL]

    options:
        -h, --help   Show this help message

    Recursive ls for a specific regexp in file names

    Example: ls_recursive file
             ls_recursive file 3

EOF
    }

    if [[ "$1" =~ ^(-h|--help)$ ]]; then
        print_usage
        return 0
    fi

    if [[ -z "$1" ]]; then
        print_usage
        return 1
    fi

    local regex="$1"
    local level="${2:-4}"  # Default level is 4 if not specified

    if ! [[ "$level" =~ ^[0-9]+$ ]]; then
        echo "Error: LEVEL must be a number"
        return 1
    fi

    find . -maxdepth "$level" -name "*${regex}*"
}
# git_wkflow_up()
# :PROPERTIES:
# :ID:       656832e2-e6e1-47ee-a1ec-97345e915648
# :END:

git_wkflow_up(){
    print_usage(){
          cat <<- EOF

 Usage: git_wkflow_up ["COMMIT MESSAGE"]

      Wrapper for the git add, commit, and push workflow. Operates on the
      current directory.

      Examples: git_wkflow_up "Message"
                git_wkflow_up

EOF
    }

    if [[ "$@" =~ (-h|--help) ]]; then
        print_usage
        return
    fi

    if ! git rev-parse --is-inside-work-tree > /dev/null 2>&1; then
        echo "Error: Not inside a Git repository."
        return 1  # Exit the function with a non-zero status
    fi

    commit_msg="$1"
 # Add all changes
    git add -A

    # Check if there are changes to commit
    if git diff-index --quiet HEAD --; then
        echo "No changes to commit."
        return 0
    fi

    # Commit and push
    if [ -z "$commit_msg" ]; then
        git commit -m. && git push
    else
        git commit -m "$commit_msg" && git push
    fi
}
# open()
# :PROPERTIES:
# :ID:       2b4acddd-ca9f-488f-aaf1-ecf4f86afdb9
# :END:

open(){
    print_usage(){
	cat <<- EOF

usage: open <FILE>

Open files with specific applications based on file extension. Defaults to xdg-open. Runs open in background using &.

example: test.pdf

EOF
    }

    if [[ "$@" =~ (-h|--help) || -z "$1" ]]; then
        print_usage
        return
    fi

    case "${1##*.}" in
	pdf)   nohup okular "$1" >/dev/null 2>&1 & ;;
	docx)  nohup libreoffice "$1" >/dev/null 2>&1 & ;;
	*)     nohup xdg-open "$1" >/dev/null 2>&1 & ;;
    esac

}
# run_with_nohup()
# :PROPERTIES:
# :ID:       4736e63d-4661-432c-b4d7-79cf7f4684ae
# :END:

run_with_nohup() {

    print_usage() {
        cat <<- EOF
Usage: run_with_nohup <FUNCTION> <FUNCTION'S OPERANDS>

Wrapper to export a function and run it with nohup.

Example: run_with_nohup smk_run ./workflow/analysis1.smk
EOF
    }

    main() {
        # Check for help or no arguments
        if [[ " $* " =~ " -h " ]] || [[ " $* " =~ " --help " ]] || [[ -z "$1" ]]; then
            print_usage
            return
        fi

        local func="$1"
        shift
        local args="$@"

        # Export the function
        export -f "$func"

        # Run the function with nohup
        nohup bash -c "$func $args" &> nohup.out &
        disown
        tail -f nohup.out
    }

    main "$@" & disown
}
# dir_size

dir_size() {
    if [[ "$1" == "-h" || "$1" == "--help" || $# -ne 1 ]]; then
        cat <<EOF

 Usage: ${FUNCNAME[0]} <path_to_directory>

 Prints the size of the specified directory, switching between MB and GB as appropriate.

 Example:
   dir_size /tmp
EOF
        return
    fi

    # Calculate size, suppressing errors
    size=$(du -s -B1 -L "$1" 2>/dev/null | cut -f1)    

    # If size is greater than or equal to 1 GB, show in GB, otherwise in MB
    if [ "$size" -ge $((1024**3)) ]; then
        du -sh --block-size=GB -L "$1"
    else
        du -sh --block-size=MB -L "$1"
    fi
}
# file_size()
# :PROPERTIES:
# :ID:       0ba47641-2a7f-4ec3-b6f9-e3abbdb49864
# :END:

file_sizes() {

    print_usage() {
    cat <<- EOF
Usage: file_sizes [<DIRECTORY>]

Displays file sizes in Mb.

Examples:
file_sizes
file_sizes /path/to/dir
EOF
    }

    main() {
        # Check for help or no arguments and provide usage information
        if [[ " $* " =~ " -h " ]] || [[ " $* " =~ " --help " ]]; then
            print_usage
            return
        fi

        # Default to the current directory if no argument is given
        local dir="${1:-.}"

        ls -tr1l --block-size=M "$dir"
    }

    main "$@"
}
# tangle() Invoke emacs batch to tangle a file
# :PROPERTIES:
# :ID:       0af857f2-01b1-44f5-8063-cfcaa0414745
# :END:

tangle() {
  [[ "$1" =~ (-h|--help) || -z "$1" ]] && {
    cat <<EOF
Usage: tangle <ORG FILE>
Tangle's org-mode file code using a non-interactive emacs batch instance
EOF
    return
  }
    local org_file="$1"
    /usr/bin/emacs --batch -l ~/.emacs.d/tangle.el -l org -eval "(org-babel-tangle-file \"$org_file\")"
}
