FROM ubuntu:xenial
RUN apt-get update -y && apt-get install -y \
build-essential \
emacs
################################################################################
# Input Data:

################################################################################

################################################################################
# References
## https://hub.docker.com/r/mgibio/rnaseq/dockerfile
## https://hub.docker.com/r/genomicpariscentre/htseq/dockerfile

### GenomicAlignments (R)

#   ## Clean up
#   RUN cd / && \
#   rm -rf /tmp/* && \
#   apt-get autoremove -y && \
#   apt-get autoclean -y && \
#   rm -rf /var/lib/apt/lists/* && \
#   apt-get clean
