FROM ubuntu:xenial
MAINTAINER Jeffrey Szymanski <jeszyman@gmail.com>
LABEL \
description="none"
################################################################################
# BUILD STATUS
################################################################################
# External Dependencies:
none
################################################################################
# Internal Dependencies:
RUN apt-get update -y && apt-get install -y \
build-essential \
none
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