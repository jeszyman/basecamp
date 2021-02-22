## CRAN
packages = c(
  "circlize",
  'cowplot',
  "cutpointr",
  "data.table",
  "ggplot2",
  'ggpubr',
  "ggpval",
  "grid",
  "gridExtra",
  "naniar",
  "RColorBrewer",
  "readr",
  "readxl",
  "tidyverse"
)
package.check = lapply(
  packages,
  FUN=function(x) {
    if (!require(x, character.only=T)) {
      install.packages(x, dependencies=T)
      library(x,character.only=T)
    }
  }
)
##
# Set variables
select=dplyr::select
filter=dplyr::filter
