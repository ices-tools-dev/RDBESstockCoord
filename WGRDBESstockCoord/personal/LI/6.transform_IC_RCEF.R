# Script info -------------------------------------------------------------

# transform IC data to RCEF using the code in https://github.com/ices-tools-dev/RDBESstockCoord/tree/main/WGRDBESstockCoord/personal/SN/BEL_examples
# this is an updated version created by Sofie to work with new versions of the icesVocab package
# based on test_Intercatch_to_RCEF.R from Sofie

# leire ibaibarriaga, 10/10/2025

# Load libraries ----------------------------------------------------------

library(data.table)
library(icesVocab)
library(plyr)
library(tidyr)
library(dplyr)

# load functions downloaded from RDBESstockCoord github repository (R folder)

source("functions_from_RDBESstockCoord_SOfie/fun_intercatch_RCEF_tidy.R")
source("functions_from_RDBESstockCoord_Sofie/fun_make_relation.R")

# Paths -------------------------------------------------------------------

# path of directory with IC for pil.27.8c9a data (by length)

data.dir <- "data/IC_data_ES"

# Make relation -----------------------------------------------------------

# manually download list of stocks by EG from: https://stockdatabase.ices.dk/default.aspx 
# specifying the active year: e.g. 2024
# and save the file "EGsStocksByYear.csv" in data.dir inside the StockList folder
# 
# manually download from: https://standardgraphs.ices.dk/stockList.aspx
# and save the file StockAssessmentGraphs.csv in data.dir inside the StockList folder

# apply the function makeRelation on these two files

stock_relation <- makeRelation("EGsStocksByYear.csv",
                               "StockAssessmentGraphs_2025124mfqskttyclbazq2wfl5zbnzy.csv",
                               StockListbyEG_path = "functions_from_RDBESstockCoord_SOfie/Stock",
                               StockListbyArea_path = "functions_from_RDBESstockCoord_SOfie/Stock")

# check resulting object

str(stock_relation)
stock_relation %>% 
  filter(EG=="WGHANSA")

# Function to create make_relation from ICES libraries --------------------

source("functions_from_RDBESstockCoord_Leire/fun_make_relation2.R")

# apply new function ------------------------------------------------------

stock_relation2 <- makeRelation2(year=2024)

stock_relation2 %>% 
  filter(EG=="WGHANSA")

# Convert from IC to RCEF -------------------------------------------------

# if output_format="to_environment", it creates 3 objects (catches, distributions and effort) in the global environment 

convExchange_tidy2(dat_path = data.dir, 
              stock_relation = stock_relation2, 
              output_format = "to_environment") 

# if output_format="to_list", it creates a list with 3 data.frames (catches, distributions and effort)

out <- convExchange_tidy2(dat_path = data.dir, 
                   stock_relation = stock_relation2, 
                   output_format = "to_list") 

# if output_format="to_file", it creates three csv files for catches, distributions and effort

convExchange_tidy2(dat_path = data.dir, 
                   stock_relation = stock_relation2, 
                   output_format = "to_file",
                   out_path="output") 

# -------------------------------------------------------------------------

