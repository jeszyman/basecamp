#########1#########2#########3#########4#########5#########6#########7#########8#########9#########0#########1
jpg_to_pdf() {
    printf "Function to convert jpeg images to pdf
    Accepts:
    Returns: "
}

#NOTE: Cannot build for local machine while connected to VPN- apt can't install software
cd ~/repos/basecamp
test=~/repos/basecamp

docker build . --file Dockerfile --tag jeszyman/test


