

## Chun Chen
## script used to test RDBES format based raising for ple.27.420

library(data.table)
library(icesVocab)
library(stringr)
library(icesVocab) #  ‘1.3.2’
library(icesSD)
library(dplyr)

mypath     <- "C:/Users/chen072/OneDrive - Wageningen University & Research/0_2026_RDBES/FLOW/ple27420/"

setwd(mypath)
scriptPath <- mypath 
dat_path   <- "./data_input"
dat_out_path <- "./data_output"
source(paste0(scriptPath, "funICoutCEF.R"))
source(paste0(scriptPath, "funMakeRelation.R"))
source(paste0(scriptPath, "funIntercatchCEF.R"))


## ##################################################
## Conversion to RCEF v16:

##This function takes the three files (MeanWeightAtAgeLength, NumbersAtAgeLength, StockOverview) that can be extracted by the stock coordinator or assessor, and outputs the data in the CEF format.

stock_relation <-funMakeRelation(2025)
stock_relation[stock_relation$StockCode == "ple.27.420",]

funICoutCEF(dat_path = dat_path, 
            years = 2025,
            stock_relation = stock_relation,
            metier6 = "fleet",
            output_format = "to_file",
            out_path = dat_out_path,
            keep_temp_file = TRUE,
            file_prefix = "ple_2025_")


## Returned as a list:
res <- funICoutCEF(dat_path = dat_path, 
                   years = 2025,
                   stock_relation = stock_relation,
                   metier6 = "fleet",
                   output_format = "to_list",
                   out_path = dat_out_path,
                   keep_temp_file = TRUE,
                   file_prefix = "")

attr(res, "RCEF_version")
names(res)

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

res2[res2$domainCatchDis == "IC_3_27.3.a.20_OTB_CRU_32-69_0_0_all" & !is.na(res2$domainCatchDis),]

## ## Some discrepancies because of (mostly zero-)Registered discards/BMS
## ##   reported in InterCatch for combinations without landings
## ##   (mostly Sweden and Scotland); for instance:
## res2 %>%
##     filter(vesselFlagCountry == "UK(Scotland)",
##            areaValue == "27.6.a",
##            metier6 == "OTB_DEF_>=120_0_0_all") %>%
##     as.data.frame()

## Note landings and discards reported at the year level, while BMS per quarter!