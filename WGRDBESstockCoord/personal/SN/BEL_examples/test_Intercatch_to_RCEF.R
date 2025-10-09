
library(data.table)
library(icesVocab)
library(RstoxData)
library(tidyr)
library(plyr)
library(dplyr)


dat_path <- "C:/Development/RStudio/D1SCI/VISBIO/ndgp.ices.wg.wgrdbes_stock_coord/Conversion/BEL_examples/"

source(paste0(dat_path, "fun_make_relation.R"))
source(paste0(dat_path, "fun_intercatch_RCEF_tidy.R"))

stock_relation <- makeRelation("EGsStocksByYear.csv",
                               "StockAssessmentGraphs_2025124mfqskttyclbazq2wfl5zbnzy.csv",
                               StockListbyEG_path = "C:/Development/RStudio/D1SCI/VISBIO/ndgp.ices.wg.wgrdbes_stock_coord/Conversion/BEL_examples/Stock",
                               StockListbyArea_path = "C:/Development/RStudio/D1SCI/VISBIO/ndgp.ices.wg.wgrdbes_stock_coord/Conversion/BEL_examples/Stock"
                               )
convExchange_tidy2(dat_path = dat_path, 
              stock_relation = stock_relation, 
              output_format = "to_environment")


