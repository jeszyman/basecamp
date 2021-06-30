FROM ubuntu:bionic
#
RUN apt-get update --fix-missing -yq
#
RUN apt-get install -yq --no-install-recommends --allow-unauthenticated \
    build-essential \
    bzip2 \
    cifs.utils \
    cmake \   
    gcc \
    git \
    libfftw3-dev \
    locales \
    nano \
    parallel \
    wget
#
RUN apt-get autoremove -y && \
    apt-get autoclean -y && \
    apt-get clean
#
RUN sed -i -e 's/# en_US.UTF-8 UTF-8/en_US.UTF-8 UTF-8/' /etc/locale.gen && \
    locale-gen
ENV LANG en_US.UTF-8  
ENV LANGUAGE en_US:en  
ENV LC_ALL en_US.UTF-8    
#
RUN cd /tmp && wget --no-check-certificate https://repo.continuum.io/miniconda/Miniconda3-4.3.21-Linux-x86_64.sh
RUN bash /tmp/Miniconda3-4.3.21-Linux-x86_64.sh -b -p /opt/miniconda
ENV PATH="/opt/miniconda/bin:${PATH}"
#
RUN apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
#RUN echo "deb http://cran.cnr.berkeley.edu/bin/linux/ubuntu xenial-cran35/" >> /etc/apt/sources.list
RUN echo "deb http://cran.wustl.edu/bin/linux/ubuntu xenial-cran35/" \
>> /etc/apt/sources.list
#RUN apt upgrade -qq
RUN apt update -qq
RUN apt-get install -qq --no-install-recommends r-base r-base-dev
ENV PATH="/usr/bin:${PATH}"
RUN echo 'local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)})' > ~/.Rprofile
RUN R -e "install.packages('devtools')"
RUN Rscript -e 'install.packages(c("tidyverse", "git2r", "stringr", "devtools", "optparse", "plyr"), repos = c(CRAN="http://cran.rstudio.com"))'
#
#
## Emacs Snapshot
RUN apt-get install -qq --no-install-recommends software-properties-common
RUN add-apt-repository ppa:ubuntu-elisp/ppa
RUN apt-get update
RUN apt-get install -qq --no-install-recommends emacs-snapshot
#
# Above built 2020-12-10
# Dev 
# Ideas
#    texlive-full
#
