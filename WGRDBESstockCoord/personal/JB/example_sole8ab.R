library("icesVocab")
library("tidyverse")
library("here")
library("glue")
library("data.table")

################################################################################
### From InterCatch to cef data format
################################################################################
source("R/fun_intercatch_RCEF.R")
path_to_data <- here("WGRDBESstockCoord/personal/JB/data")

### Make my own stock relation data frame
df_stock_relation_sole8ab <- data.frame(
  SpeciesName = c("Solea solea", "Solea solea"),
  StockCode = c("sol.27.8ab", "sol.27.8ab"),
  EG = c("WGBIE", "WGBIE"),
  ICESArea = c("27.8.a", "27.8.b"),
  speciesCode = c(198720, 198720),
  Species = "SOL")

list_cef <- convExchange(dat_path = glue("{path_to_data}/tmp"),
                         stock_relation = df_stock_relation_sole8ab,
                         output_format = "to_list",
                         metier6 = "Fleet")

################################################################################
### Discards raising
################################################################################

### needed functions
source("WGRDBESstockCoord/personal/JB/fun/census_add_strata.R")
source("WGRDBESstockCoord/personal/JB/fun/split_L_with_without_D.R")
source("WGRDBESstockCoord/personal/JB/fun/raise_discards_stratum.R")

### Split data with L and D and with L only, provide tables as wider table (Landings and discards in columns)
list_stock_overview <- split_L_with_without_D(cef_catches = list_cef$catches,
                                              sourceType = "WGValue",
                                              compute_discard_ration = TRUE,
                                              domain = "domainCatchDis",
                                              add_strata = TRUE)

### Explore percentage of coverage
list_stock_overview$dfw_stock_overview <- list_stock_overview$dfw_stock_overview %>%
  mutate(total_landings = sum(LAN_total))


strata_test <- "strata_c"
strata_test_var <- rlang::enquo(strata_test)

dfsw_stock_overview <- list_stock_overview$dfw_stock_overview %>%
  filter(!is.na(DIS_total)) %>%
  dplyr::group_by(strata_c) %>%
  reframe(strata_total_LAN = sum(LAN_total),
  coverage_total_LAN = strata_total_LAN/ total_landings
  ) %>%
  distinct() %>%
  mutate(strata = strata_test,
         values = strata_c,
         sum_coverage_total_LAN = sum(coverage_total_LAN))


test <- strata_coverage_summary(dfw_stock_overview =  list_stock_overview$dfw_stock_overview ,strata_var = "strata_c")

ggplot(data = test, aes(x = strata,
                                       y = coverage_total_LAN,
                                       fill = values )) +
  geom_col() +
  ylim(0, 1)





### raise discards per strata
list_raise_discards_c_q_f <- raise_discards_stratum(stratum_var = "stratum_c_q_f",
                                                    dfw_stock_overview_L_D = dfw_stock_overview_L_D,
                                                    dfw_stock_overview_L_noD = dfw_stock_overview_L_noD)

