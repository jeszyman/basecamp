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

convert_pdf_to_png() {
    local input_file="$1"

    if [ -z "$input_file" ]; then
        cat <<- EOF
        usage: convert_pdf_to_png <input-file.pdf>

        Converts a PDF to a PNG with a white background.

        example: convert_pdf_to_png input.pdf

        Notes:
        - /etc/ImageMagick-6/policy.xml policy change allows work with PDF
        - allows file overwrite
        - works from /tmp
        - set to 600 dpi
EOF
        return 1
    fi

    local output_file=$(echo "$input_file" | sed 's/\.pdf$/.png/')
    convert -density 600 -background white -flatten "$input_file" "$output_file"

    echo "Conversion complete. Output saved to $output_file"
}

# Uncomment below line to use the function with the first script argument
# convert_pdf_to_png "$1"

convert_pdf_to_svg() {
    local input_file="$1"

    if [ -z "$input_file" ]; then
        cat <<- EOF
        usage: convert_pdf_to_svg <input-file.pdf>

        Converts a PDF to an SVG with a white background.

        example: convert_pdf_to_svg input.pdf

        Notes:
        - Uses pdf2svg for conversion
        - Allows file overwrite
        - Works from /tmp
EOF
        return 1
    fi

    local output_file=$(echo "$input_file" | sed 's/\.pdf$/.svg/')
    pdf2svg "$input_file" "$output_file"

    # Add a white background rectangle at the beginning of the SVG content
    sed -i '/<svg/a <rect width="100%" height="100%" fill="white"/>' "$output_file"

    echo "Conversion complete. Output saved to $output_file with a white background."
}

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
