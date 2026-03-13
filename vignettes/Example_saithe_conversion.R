
library(data.table)
library(icesVocab)
library(RstoxData)
library(plyr)
library(tidyr)
library(dplyr)
library(stringr)

## Also requires recent versions of icesSAG, icesSD and icesVocab:
## install.packages("icesSAG", repos = "https://ices-tools-prod.r-universe.dev")
## install.packages("icesVocab", repos = c("https://ices-tools-prod.r-universe.dev",
##                                         "https://cloud.r-project.org"))
## install.packages("icesSD", repos = c("https://ices-tools-prod.r-universe.dev",
##                                      "https://cloud.r-project.org"))


## Set appropriate WD, based on some usual EDI R-opening behaviours:
if (basename(getwd()) == "R" ||
    basename(getwd()) == "vignettes")
    setwd("..")

## Paths:
scriptPath <- "./R" ##"./WGRDBESstockCoord/personal/jost"

## NS saithe data (CATON and samples randomized,
##   so expect some inconsistencies in caton for a same stratum among tables,
##   or samples where zero-catch!):
dat_path <- "./data/data_overviews_format/pok"
dat_out_path <- "./data/data_CEF_format/pok"

dir.create(path = dat_out_path, recursive = TRUE)

## scriptPath_to_data <- "./WGRDBESstockCoord/personal/jost"
#out_path <- "Q:/dfad/users/jostou/home/wg_stock/rcef_intercatch"

source(file.path(scriptPath, "funICoutCEF.R"))
source(file.path(scriptPath, "funMakeRelation.R"))
source(file.path(scriptPath, "funIntercatchCEF.R"))

## ##################################################
## Conversion to RCEF v16:

## To files:
funICoutCEF(dat_path = dat_path, 
            years = 2022,
            metier6 = "fleet",
            output_format = "to_file",
            out_path = dat_out_path,
            keep_temp_file = TRUE,
            file_prefix = "pok_2022_")

## Returned as a list:
res <- funICoutCEF(dat_path = dat_path, 
                   years = 2022,
                   metier6 = "fleet",
                   output_format = "to_list",
                   out_path = dat_out_path,
                   keep_temp_file = TRUE,
                   file_prefix = "")

attr(res, "RCEF_version")

## Take a peek at the results:
res$catches %>%
  group_by(catchCategory, originType, variableType) %>%
  slice_head(n = 1) %>%
  as.data.frame()

res$distributions %>%
  group_by(catchCategory, variableType) %>%
  slice_head(n = 2) %>%
  as.data.frame()

## Explo composite key matching:
res2 <- res$catches

table(res2$domainCatchDis, res2$catchCategory)
table(res2$domainCatchBMS, res2$catchCategory)

## ## Some discrepancies because of (mostly zero-)Registered discards/BMS
## ##   reported in InterCatch for combinations without landings
## ##   (mostly Sweden and Scotland); for instance:
## res2 %>%
##     filter(vesselFlagCountry == "UK(Scotland)",
##            areaValue == "27.6.a",
##            metier6 == "OTB_DEF_>=120_0_0_all") %>%
##     as.data.frame()

## Note landings and discards reported at the year level, while BMS per quarter!
