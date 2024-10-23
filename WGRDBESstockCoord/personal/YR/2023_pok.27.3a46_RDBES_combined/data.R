#-*- coding: utf-8 -*-

### File: data.R
### Time-stamp: <2023-10-05 10:53:58 a23579>
###
### Created: 03/10/2023	08:22:31
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
###
####################################################################################################
source("./0_settings.R")

library(icesTAF)
library(dplyr)
library(tidyr)

mkdir("data")



message("Some procedure to load/preprocess data in InterCatch format, then save them in ./data/")

## Temporary emulation... copy IC output files without raising:
file.copy(from = "./bootstrap/data/NumbersAtAgeLength_2022.txt", to = "./data/")
file.copy(from = "./bootstrap/data/StockOverview_2022.txt", to = "./data/")
file.copy(from = "./bootstrap/data/MeanWeigthAtAgeLength_2022.txt", to = "./data/")

file.copy(from = "./bootstrap/data/IC_outputs", to = "./data/", recursive = TRUE)




### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
