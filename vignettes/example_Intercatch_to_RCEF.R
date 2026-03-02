
library(data.table)
library(icesVocab)
library(RstoxData)
library(tidyr)
library(plyr)


path <- "Q:/20-forskning/20-dfad/users/jostou/home/wg_stock/WGRDBESstockCoord/personal/jost"
dat_path <- "Q:/20-forskning/20-dfad/users/jostou/home/wg_stock/WGRDBESstockCoord/personal/jost/data_exchange_format"
#out_path <- "Q:/dfad/users/jostou/home/wg_stock/rcef_intercatch"

source(paste0(path, "/fun_make_relation.R"))
source(paste0(path, "/fun_intercatch_RCEF.R"))

stock_relation <- makeRelation(year(Sys.Date()))
convExcahcnge(dat_path = dat_path, 
              stock_relation = stock_relation, 
              output_format = "to_environment")