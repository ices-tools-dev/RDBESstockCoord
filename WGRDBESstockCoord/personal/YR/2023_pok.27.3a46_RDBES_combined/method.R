#-*- coding: utf-8 -*-

### File: method.R
### Time-stamp: <2023-10-06 11:15:55 a23579>
###
### Created: 03/10/2023	08:20:02
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
###
####################################################################################################
if (!exists("dataYear"))
    source("./0_settings.R")

library(icesTAF)
library(tidyr)
library(data.table)
library(tibble)
library(stringr)
library(abind)

mkdir("model")

dataDir <- "./data"
dataDir <- normalizePath(dataDir)

## Load data:
caton <- read.delim(file = file.path(dataDir, "StockOverview_2022.txt"))

numbers <- read.delim(file.path(dataDir,'NumbersAtAgeLength_2022.txt'),
                      skip=2)

weights <- read.delim(file.path(dataDir,'MeanWeigthAtAgeLength_2022.txt'),
                      skip=1)

## Some data wrangling before matching group definitions:
sourceTAF("./method_1_data_prep.R")


## ##################################################
## Discards, BMS, etc. raising:
sourceTAF("./method_2_group_definitions_Discards.R")

sourceTAF("./method_3_Discards_raising.R")

## ##################################################
## N-at-Age and W-at-age allocations:
sourceTAF("./method_4_group_definitions_Age.R")

sourceTAF("./method_5_Age_allocations.R")

if (isTRUE(getOption("allocateBMS")))
{
    sourceTAF("./method_5_Age_allocations_BMS_alloc.R")
}


### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
