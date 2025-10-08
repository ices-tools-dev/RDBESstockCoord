library("icesVocab")
library("RstoxData")
library("tidyverse")
library("here")
library("glue")

################################################################################
### From InterCatch to RCEF data format
################################################################################

source(here("R/fun_ICout_RCEF_tidy.R"))
source(here("R/fun_ICout_to_RCEF.R"))
source(here("R/fun_make_relation.R"))
source(here("R/fun_intercatch_RCEF_tidy.R"))
source(here("R/fun_intercatch_RCEF.R"))


path_to_data <- here("WGRDBESstockCoord/personal/JB/data")

# ### Use makeRelation function to create stock_relation data frame
# all_stock_relation <- makeRelation(
#   StockListbyEG_file = "EGsStocksByYear.csv",
#   StockListbyArea_file = "StockAssessmentGraphs_2025.csv",
#   StockListbyEG_path = path_to_data,
#   StockListbyArea_path = path_to_data
# )
#
# stock_relation_sole8ab <- all_stock_relation %>%
#   filter(SpeciesName == "Solea solea")

### Make my own stock relation data frame
df_stock_relation_sole8ab <- data.frame(
  SpeciesName = c("Solea solea", "Solea solea"),
  StockCode = c("sol.27.8ab", "sol.27.8ab"),
  EG = c("WGBIE", "WGBIE"),
  ICESArea = c("27.8.a", "27.8.b"),
  speciesCode = c(198720, 198720),
  Species = "SOL")

list_ICout <- ICout_RCEF_tidy(
  dat_path = glue("{path_to_data}"),
  stock_relation = df_stock_relation_sole8ab,
  output_format = c("to_file", "to_list"),
  years = 2025
)

list_rcef <- convExchange_tidy(dat_path = glue("{path_to_data}/tmp"),
                               stock_relation = df_stock_relation_sole8ab,
                               output_format = "to_list",
                               metier6 = "Fleet")

# estimated_catch should be NA instead of 0 for dicards  ? comes frome si caton
list_rcef$estimated_catches <- list_rcef$estimated_catches %>%
  mutate(total = ifelse(total == 0, NA, total))


################################################################################
### Discards raising
################################################################################

################################################################################
### Add stratum to census data for discards raising
source("WGRDBESstockCoord/personal/JB/fun/census_add_stratum.R")

list_rcef$census_catches <- list_rcef$census_catches %>%
  census_add_stratum()


test <- generate_stratum_combinations(list_rcef$census_catches,
                                      selected_strata = c("stratum_c_q_f",
                                                          "stratum_c_q_a_g",
                                                          "stratum_c_q_a_sg",
                                                          "stratum_c_a_sg",
                                                          "stratum_q_a",
                                                          "stratum_q"),
                                      use_values_as_names = TRUE,
                                      combine_all = FALSE)


grp_catch_raising_by_stratum_col(census_data = list_rcef$census_catches,
                                 catch_estimates = list_rcef$estimated_catches,
                                 stratum_col = "stratum_c_q_f",
                                 groupName = "stratum_c_q_f",
                                 variableType = "scientificWeight_kg",
                                 type = c("discards", "bms"),
                                 verbose = TRUE)

make_stratum <- function () {
  ### (1)  stratum allocation: Country, Season, Fleet (no Area)
  ### (2)  stratum allocation: Country, Season, Area, Gear (e.g. GNS, OTB, ...)
  ### (3)  stratum allocation: Country, Season, Area, Super-Gear (e.g. G, O, ...)
  ### (4)  stratum allocation: Season, Area, Super-Gear (e.g. G, O, ...)
  ### (5)  stratum allocation: Season, Area
  ### (6)  stratum allocation: Season
}

cond_test <- check_group_conditions(census_data = census,
                                    condition_list = strataCond,
                                    logFile = NULL, append = TRUE)

cond_test2 <- check_group_conditions(census_data = census,
                                     condition_list = matchedDataCond,
                                     conditionType = "matched_data",
                                     logFile = NULL, append = TRUE)

test <- raising_cond_loop(census_data = list_rcef$census_catches,
                          catch_estimates = list_rcef$estimated_catches  ,
                          catch_estimates = list_rcef$  ,
                          condition_raising_st_list = strataCond,
                          condition_matched_data_list = matchedDataCond,
                          type = "discards",
                          variableType = "scientificWeight_kg",
                          logFile = "Log.txt",
                          assembled_output = TRUE)

