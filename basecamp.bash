chmod 700 ~/.ssh
chmod 600 ~/.ssh/id_rsa
chmod 600 ~/.ssh/id_rsa.pub

#
# Install
cd /opt
sudo git clone https://github.com/andreafrancia/trash-cli.git
cd trash-cli
sudo python setup.py install
#
#

#
# Install
cd /opt
sudo git clone https://github.com/andreafrancia/trash-cli.git
cd trash-cli
sudo python setup.py install
#
#

    cd /tmp/
    rm file1
    rm file2
    touch file2; sleep 2; touch file1
    if [ file1 -nt file2 ]; then
        echo "yes"
    fi

    cd /tmp/
    rm file1
    rm file2
    touch file2; sleep 2; touch file1
    if [ file1 -nt file2 ]; then
        echo "yes"
    fi

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
