#-*- coding: utf-8 -*-

### File: report.R
### Time-stamp: <2023-10-06 09:32:19 a23579>
###
### Created: 03/10/2023	08:23:59
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
library(dplyr)
library(tidyr)
library(ggplot2)

repDir <- "report"
mkdir(repDir)
repDir <- normalizePath(repDir)

dataICDir <- "./data/IC_outputs/"
dataICDir <- normalizePath(dataICDir)

sourceTAF("./report_1_plots.R")

if (isTRUE(getOption("generateReport")))
{
    sourceTAF("./report_3_doc.R")
}



### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
