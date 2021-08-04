# Check CRAN packages, install as needed
##
packages = c(
"caret",
"circlize",
'cowplot',
"cutpointr",
#"data.table",
#"factoextra",
"FSA",
"ggplot2",
#'ggpubr',
"ggpval",
"ggsci",
"ggvenn",
"ggrepel",
"grid",
"gridExtra",
"Hmisc",
"kableExtra",
"magrittr",
"naniar",
"pheatmap",
"RColorBrewer",
"readr",
"readxl",
"scales",
"tidyverse",
"UpSetR",
"viridis"
)
#
package.check = lapply(
  packages,
  FUN=function(x) {
    if (!require(x, character.only=T)) {
      install.packages(x, dependencies=T)
      library(x,character.only=T)
    }
  }
)

#
# Theme
theme_general <- function () { 
  theme_cowplot(16) %+replace%
    theme()
}


#
select=dplyr::select
filter=dplyr::filter
group_by=dplyr::group_by
summarise=dplyr::summarise
