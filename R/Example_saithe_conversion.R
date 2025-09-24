
library(data.table)
library(icesVocab)
library(RstoxData)
library(plyr)
library(tidyr)
library(dplyr)
library(stringr)

## Set appropriate WD, based on some usual EDI R-opening behaviours:
if (basename(getwd()) == "R")
    setwd("..")

path <- "./R" ##"./WGRDBESstockCoord/personal/jost"
dat_path <- "./WGRDBESstockCoord/personal/jost/data_overviews_format/pok"
path_to_data <- "./WGRDBESstockCoord/personal/jost"
#out_path <- "Q:/dfad/users/jostou/home/wg_stock/rcef_intercatch"

source(file.path(path, "fun_ICout_to_RCEF.R"))
source(file.path(path, "fun_make_relation.R"))
source(file.path(path, "fun_intercatch_RCEF.R"))
## source(file.path(path, "fun_conversion_RCEF_v14_to_v15.R"))

## ##################################################
## Conversion to RCEF v14:

## To files:
ICout_RCEF(dat_path = dat_path, 
           years = 2022,
           metier6 = "fleet",
           output_format = "to_file",
           out_path = dat_path,
           keep_temp_file = TRUE,
           file_prefix = "pok_2022_")

## Saved as a list:
res <- ICout_RCEF(dat_path = dat_path, 
                  years = 2022,
                  metier6 = "fleet",
                  output_format = "to_list",
                  out_path = dat_path,
                  keep_temp_file = TRUE,
                  file_prefix = "")

res$catches %>%
    group_by(catchCategory) %>%
    slice_head(n = 1) %>%
    as.data.frame()

## res$census_catches %>%
##     group_by(catchCategory) %>%
##     slice_head(n = 1) %>%
##     as.data.frame()

res2 <- res$catches

table(res2$domainCatchDis, res2$catchCategory)
table(res2$domainCatchBMS, res2$catchCategory)

