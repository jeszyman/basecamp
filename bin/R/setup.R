#https://stackoverflow.com/questions/9397664/force-r-not-to-use-exponential-notation-e-g-e10
# R setup
#  Load packages:
list.of.packages=c(
    # Import and export:
    #"ascii", # Exports to markup
    "Hmisc",
    "orgutils",
    "googlesheets",
    "xtable", # Coerce data to LaTeX and HTML tables
    # Data structure:
    "data.table",
    "reshape",    
    "plyr",
    "dplyr",
    "tidyr",
    # Data formatting:
    "lubridate",    
    "stringr",
    # Graphics:
    "ggplot2",
    "cowplot",
    "scales",
    "gridExtra",
    "RColorBrewer",
    # Regression:
    "glmnet",
    # Trees and random forest:
    "rpart",
    "rpart.plot",
    "randomForest",
    "zoo" # for month-year dates
    )
new.packages=list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only=T) 

options(scipen=999) # Disables scientific notation
options(asciiType='org')
#https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
