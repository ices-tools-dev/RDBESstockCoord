library("data.table")
library("icesVocab")
library("RstoxData")
library("tidyr")
library("plyr")
library("stringr")
library("here")
library("glue")


source(here("R/fun_ICout_to_RCEF.R"))
source(here("R/fun_make_relation.R"))
source(here("R/fun_intercatch_RCEF.R"))

path_to_data <- here("WGRDBESstockCoord/personal/JB/data")

ICout_RCEF(
  dat_path = glue("{path_to_data}"),
  years = 2025,
  output_format = "to_environment",
  stock_relation = stock_relation
)


 makeRelation(
  StockListbyEG_file = "EGsStocksByYear.csv",
  StockListbyArea_file = "StockAssessmentGraphs_2025.csv",
  StockListbyEG_path = path_to_data,
  StockListbyArea_path = path_to_data
)
#
# convExcahcnge(
#   dat_path = glue("{path_to_data}/2025"),
#   stock_relation = stock_relation,
#   output_format = "to_environment"
# )
