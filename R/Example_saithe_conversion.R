
library(data.table)
library(icesVocab)
library(RstoxData)
library(tidyr)
library(plyr)
library(stringr)

## Set appropriate WD, based on some usual EDI R-opening behaviours:
if (basename(getwd()) == "R")
    setwd("..")

path <- "./R" ##"./WGRDBESstockCoord/personal/jost"
dat_path <- "./WGRDBESstockCoord/personal/jost/data_overviews_format/pok"
path_to_data <- "./WGRDBESstockCoord/personal/jost"
#out_path <- "Q:/dfad/users/jostou/home/wg_stock/rcef_intercatch"

source(paste0(path, "/fun_ICout_to_RCEF.R"))
source(paste0(path, "/fun_make_relation.R"))
source(paste0(path, "/fun_intercatch_RCEF.R"))

## Conversion to RCEF v14:
ICout_RCEF(dat_path = dat_path, 
           years = 2022,
           metier6 = "fleet",
           output_format = "to_file",
           out_path = dat_path,
           keep_temp_file = TRUE,
           file_prefix = "pok_2022_")



