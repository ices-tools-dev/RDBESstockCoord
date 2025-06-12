
library(data.table)
library(icesVocab)
library(RstoxData)
library(tidyr)
library(plyr)
library(stringr)

path <- "Q:/20-forskning/20-dfad/users/jostou/home/wg_stock/WGRDBESstockCoord/personal/jost"
dat_path <- "Q:/20-forskning/20-dfad/users/jostou/home/wg_stock/WGRDBESstockCoord/personal/jost/data_overviews_format/cod"
#out_path <- "Q:/dfad/users/jostou/home/wg_stock/rcef_intercatch"

source(paste0(path, "/fun_ICout_to_RCEF.R"))
source(paste0(path, "/fun_make_relation.R"))
source(paste0(path, "/fun_intercatch_RCEF.R"))

ICout_RCEF(dat_path = dat_path, 
           years = 2023,
           output_format = "to_environment")



