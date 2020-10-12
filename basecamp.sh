#
###################################
### copy-file-text-to-clipboard ###
###################################
#
# Purpose: for pandoc supported file, copies file text to system clipboard
#
# Dependencies:
dependencies=("pandoc" "xclip")
check_dependencies dependencies[@]
#
# Input: 
# Any pandoc supported file 
#
# Function:
text_to_clip() { pandoc -S $1 | xclip -selection clipboard}
#
# Example
cd /tmp/ && rm -rf ./example && mkdir -p ./example && cd ./example 
echo "Hello" | pandoc -o out.docx
chmod 777 ./out.docx
text_to_clip()
./out.docx
test=out.docx
pandoc -S $test | xclip -selection clipboard
