FROM ubuntu:xenial
RUN apt-get update -y
RUN apt-get install -y --no-install-recommends \
    build-essential \
    emacs \
    texlive-full
RUN apt-get install -y --no-install-recommends \
    cifs.utils
RUN apt-get autoremove -y && \
    apt-get autoclean -y && \
    apt-get clean
# big downloads/builds should get their own RUN step