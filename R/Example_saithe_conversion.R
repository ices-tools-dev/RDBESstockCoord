
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

source(file.path(path, "fun_ICout_to_RCEF.R"))
source(file.path(path, "fun_make_relation.R"))
source(file.path(path, "fun_intercatch_RCEF.R"))
source(file.path(path, "fun_conversion_RCEF_v14_to_v15.R"))

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

res$estimated_catches %>%
    group_by(catchCategory) %>%
    slice_head(n = 1) %>%
    as.data.frame()

res$census_catches %>%
    group_by(catchCategory) %>%
    slice_head(n = 1) %>%
    as.data.frame()

## ##################################################
## Conversion to v14.5:

## From and to files:
res2 <- RCEF_catch_convert_v14_to_v15(file.path(dat_path, "pok_2022_census_catches.csv"),
                                file.path(dat_path, "pok_2022_estimated_catches.csv"))

## Using previous objects results:
res2 <- do.call(RCEF_catch_convert_v14_to_v15,
                res)

set.seed(12345)
res2 %>%
    group_by(catchCategory, variableType) %>%
    slice_head(n = 2) %>%
    as.data.frame()

table(res2$domainCatchDis, res2$catchCategory)
table(res2$domainCatchBMS, res2$catchCategory)

