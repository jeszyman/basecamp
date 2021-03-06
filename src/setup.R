#########1#########2#########3#########4#########5#########6#########7#########8
#
# Check CRAN packages and install as needed
packages = c(
"caret",
"circlize",
'cowplot',
"cutpointr",
"data.table",
"factoextra",
"FSA",
"ggplot2",
'ggpubr',
"ggpval",
"ggsci",
"ggvenn",
"ggrepel",
"grid",
"gridExtra",
"kableExtra",
"magrittr",
"naniar",
"pheatmap",
"RColorBrewer",
"readr",
"readxl",
"scales",
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

# Set environmental variables
select=dplyr::select
filter=dplyr::filter

#########1#########2#########3#########4#########5#########6#########7#########8
# Set variables
select=dplyr::select
filter=dplyr::filter
