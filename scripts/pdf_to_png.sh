#!/usr/bin/env bash

# /etc/ImageMagick-6/policy.xml policy change allows work with PDF
# allows file overwrite

print_usage(){
    cat <<- EOF

    usage: $0 <input-file.pdf>

    Converts a PDF to a PNG with a white background.

    example: $0 input.pdf

    Notes:
    - /etc/ImageMagick-6/policy.xml policy change allows work with PDF
    - allows file overwrite
    - works from /tmp
    - set to 600 dpi
EOF
}

# Check for input file
if [ -z "$1" ]; then
  print_usage
  exit 1
fi

# Input file
input_file="$1"

# Output file based on input file
output_file=$(echo "$input_file" | sed 's/\.pdf$/.png/')

convert -density 600 -background white -flatten "$input_file" "$output_file"

echo "Conversion complete. Output saved to $output_file"
#!/usr/bin/env bash

# /etc/ImageMagick-6/policy.xml policy change allows work with PDF
# allows file overwrite

print_usage(){
    cat <<- EOF

    usage: $0 <input-file.pdf>

    Converts a PDF to a PNG with a white background.

    example: $0 input.pdf

    Notes:
    - /etc/ImageMagick-6/policy.xml policy change allows work with PDF
    - allows file overwrite
    - works from /tmp
    - set to 600 dpi
EOF
}

# Check for input file
if [ -z "$1" ]; then
  print_usage
  exit 1
fi

# Input file
input_file="$1"

# Output file based on input file
output_file=$(echo "$input_file" | sed 's/\.pdf$/.png/')

convert -density 600 -background white -flatten "$input_file" "$output_file"

echo "Conversion complete. Output saved to $output_file"
