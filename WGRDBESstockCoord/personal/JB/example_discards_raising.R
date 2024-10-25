source("boot.R")

### -----------------------------------------------------------------------------
### load IC data
### -----------------------------------------------------------------------------
### read file and make stratum for discards raising
stock_overview_raw <- read_stock_overview(path_data_intercatch = "data")

### -----------------------------------------------------------------------------
### Add strata
dfw_stock_overview <- stock_overview_raw$dfw_stock_overview %>%
  intercatch_add_stratum()

### extract data with landings and discards
dfw_stock_overview_L_D <- dfw_stock_overview %>%
  filter(!is.na(discards_ratio))

### extract data without landings and discards
dfw_stock_overview_L_noD <- dfw_stock_overview %>%
  filter(is.na(discards_ratio)) %>%
  select(-discards_ratio) %>%
  mutate(raised_strata = NA)

stratum_no_discards <- dfw_stock_overview_L_noD %>%
  select(stratum_full)

### -----------------------------------------------------------------------------
### (1) First stratum allocation: Country, Season, Fleet (no Area)
### -----------------------------------------------------------------------------
df_discards_stratum_c_s_f <- raise_discards_strata(
  data_landings_discards = dfw_stock_overview_L_D,
  data_discards_to_raised = dfw_stock_overview_L_noD,
  grouping_strata = "stratum_c_s_f",
  weightingfactor = "CATON"
)


View(df_discards_stratum_c_s_f %>%
  arrange(raised_strata))
