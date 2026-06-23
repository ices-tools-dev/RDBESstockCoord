library("icesVocab")
library("icesSD")
library("tidyverse")
library("here")
library("glue")
library("data.table")

################################################################################
### From InterCatch to cef data format
################################################################################
source("R/funIntercatchCEF.R")
source("R/funICoutCEF.R")
source("R/funMakeRelation.R")
source("R/cef_add_strata.R")
source("WGRDBESstockCoord/personal/JB/fun/cef_compute_ref_numbers.R")
source("WGRDBESstockCoord/personal/JB/fun/cef_allocate_age_strata.R")

path_to_data <- here("WGRDBESstockCoord/personal/JB/data")
path_data_raised <- here("WGRDBESstockCoord/personal/JB/output")
data_year <- 2024
max_agestock <- 8

## Returned as a list:
list_cef <- funICoutCEF(dat_path = glue("{path_to_data}/"),
                        years = data_year,
                        metier6 = "Fleet",
                        output_format = "to_list",
                        out_path = dat_path,
                        keep_temp_file = TRUE,
                        file_prefix = "sol_8ab")

###-----------------------------------------------------------------------------
### extract varialbes used in cef_distribution from cef_catches using domainBiology

## load data raised in sole8ab_discards_raising.R
df_stock_overview_L_D_raised <- readRDS(glue("WGRDBESstockCoord/personal/JB/output/df_stock_overview_L_D_raised{data_year}.rds")) %>%
  cef_remove_strata() %>%
  cef_add_strata() %>%
  mutate(seasonValue = as.character(seasonValue))

### extract combination of domainBiology, catchCategory, strata_full to make the link between catches and distribution
df_catch_domainBiology <- df_stock_overview_L_D_raised %>%
  filter(!is.na(domainBiology)) %>%
  select(domainBiology,
         # vesselFlagCountry,
         # year,
         # workingGroup,
         # stock,
         # speciesCode,
         catchCategory,
         strata_full) %>%
  mutate(catchCategory = case_when(catchCategory == "Dis_total" ~ "Dis",
                                   catchCategory == "BMS_total" ~ "BMS",
                                   catchCategory == "Lan_total" ~ "Lan",
                                   TRUE ~ catchCategory)) %>%
  distinct()

###-----------------------------------------------------------------------------
### Merge distribution and catch
cef_distribution <-  list_cef$distributions %>%
  mutate(value = as.numeric(value),
         catchCategory = case_when(catchCategory == "DIS" ~ "Dis",
                                   catchCategory == "BMS" ~ "BMS",
                                   catchCategory == "LAN" ~ "Lan",
                                   TRUE ~ catchCategory)) %>%
  left_join(.,
            df_catch_domainBiology,
            by = join_by(catchCategory, domainBiology)) %>%
  mutate(importedOrRaised = "imported") %>%
  droplevels()  %>%
  mutate(Age = distributionClass) %>%
  cef_expand_strata_full()

### Pivot wider to have weight and number in columns
df_canum_imported <- pivot_wider( cef_distribution,
                                 values_from = value,
                                 id_cols = c(strata_full, distributionClass, catchCategory),
                                 names_from = c(variableType, valueType),
                                 names_sep = "_",
                                 unused_fn = dplyr::first)  %>%
  cef_expand_strata_full()

### SOP correction
source("WGRDBESstockCoord/personal/JB/sole8ab_age_allocation_sop.R")

###-----------------------------------------------------------------------------
### Fill strata with missing individuals sampling data
###-----------------------------------------------------------------------------

###-----------------------------------------------------------------------------
### Landings and discards with age allocation
df_ref_fleets <- df_stock_overview_L_D_raised %>%
  filter(strata_full %in% unique(df_canum_imported$strata_full)) %>%
  droplevels()
dim(df_ref_fleets)

###-----------------------------------------------------------------------------
### Landings age allocation
df_allocation_fleets <- df_stock_overview_L_D_raised %>%
  filter(!(strata_full %in% unique(df_canum_imported$strata_full))) %>%
  droplevels()
dim(df_allocation_fleets)

dim(df_stock_overview_L_D_raised)

strata_no_age <- df_allocation_fleets %>%
  select(strata_full)

### df_canum_for_allocation use for reference
df_canum_for_allocation <- df_canum_imported_sopr

###-----------------------------------------------------------------------------
# Allocation rules
allocation_specs <- list(
  list(group_strata = "strata_c_q_g", join_by_cols = c("strata_c_q_g", "catchCategory"), source_name = "strata_c_q_g"),
  list(group_strata = "strata_q_g",   join_by_cols = c("strata_q_g",   "catchCategory"), source_name = "strata_q_g"),
  list(group_strata = "strata_q_sg",  join_by_cols = c("strata_q_sg",  "catchCategory"), source_name = "strata_q_sg"),
  list(group_strata = "strata_g",     join_by_cols = c("strata_g",     "catchCategory"), source_name = "strata_g"),
  list(group_strata = "strata_sg",    join_by_cols = c("strata_sg",    "catchCategory"), source_name = "strata_sg"),
  list(group_strata = "strata_c_q",   join_by_cols = c("strata_c_q",   "catchCategory"), source_name = "strata_c_q",
       extra_filter = quote(gear == "MIS")),
  list(group_strata = "strata_c",     join_by_cols = c("strata_c",     "catchCategory"), source_name = "strata_c")
)

allocated_list <- list()
for (i in seq_along(allocation_specs)) {
  spec <- allocation_specs[[i]]

  allocated_list[[i]] <- cef_allocate_strata(
    df_allocation_fleets = df_allocation_fleets,
    df_canum_for_allocation = df_canum_for_allocation,
    strata_no_age = strata_no_age,
    number_col = "Number_Total_SopR",
    group_strata = spec$group_strata,
    join_by_cols = spec$join_by_cols,
    source_name = spec$source_name
  )

  strata_no_age <- strata_no_age %>%
    filter(!(strata_full %in% levels(allocated_list[[i]]$strata_full))) %>%
    droplevels()
}

df_canum_allocated <- bind_rows(allocated_list) %>%
  cef_remove_strata() %>%
  rename(Caton = Catchkg, weights = weights_ref) %>%
  select(-any_of(c("DiscardsImportedOrRaised",
                   "raised_landings",
                   "numbers_ref_tot",
                   "weca_tot",
                   "SoP"))) %>%
  mutate(seasonValue = as.character(seasonValue))

df_canum_for_allocation <- df_canum_for_allocation %>%
  cef_remove_strata()

df_canum <- bind_rows(df_canum_for_allocation,
                      df_canum_allocated) %>%
  cef_remove_strata()

saveRDS(df_canum,
        file = glue("{path_data_raised}/df_canum_all_age_{data_year}.rds"))

df_canum_ageplus <- df_canum %>%
  mutate(Age = ifelse(as.numeric(Age) >= as.numeric(max_agestock), "8", Age)) %>%
  group_by(strata_full, Age, catchCategory) %>%
  mutate(Number_Total = sum(Number_Total)) %>%
  # select(-c("weights", "numbers_imported")) %>%
  distinct()

saveRDS(df_canum,
        file = glue("{path_data_raised}/df_canum_ageplus_{data_year}.rds"))

###-----------------------------------------------------------------------------
df_weights_strata_CatchCat_Age <- df_canum %>%
  group_by(strata_full, catchCategory, Age) %>%
  mutate(mean_weights = sum(weights * numbers) /  sum(numbers))

saveRDS(df_weights_strata_CatchCat_Age,
        file = glue("{path_data_raised}/df_weights_strata_{data_year}.rds"))

df_weights_CatchCat_Age <- df_canum %>%
  filter(as.numeric(Age) < as.numeric(max_agestock)) %>%
  group_by(catchCategory, Age) %>%
  mutate(mean_weights = sum(weights * numbers) /  sum(numbers),
         Age = as.character(Age))

df_weights_CatchCat_AgePlus <- df_canum %>%
  filter(as.numeric(Age) >= as.numeric(max_agestock)) %>%
  group_by(catchCategory) %>%
  mutate(mean_weights = sum(weights * numbers) / sum(numbers),
         Age = as.character(max_agestock))

df_weights_CatchCat_Age <- bind_rows(df_weights_CatchCat_Age,
                                     df_weights_CatchCat_AgePlus)

saveRDS(df_weights_CatchCat_Age,
        file = glue("{path_data_raised}/df_weights_CatchCat_Age_{data_year}.rds"))

df_weights_Catch_Age <- df_canum %>%
  filter(as.numeric(Age) < as.numeric(max_agestock)) %>%
  group_by(Age) %>%
  mutate(mean_weights = sum(weights * numbers) /  sum(numbers),
         Age = as.character(max_agestock))

df_weights_Catch_AgePlus <- df_canum %>%
  filter(as.numeric(Age) >= as.numeric(max_agestock)) %>%
  mutate(mean_weights = sum(weights * numbers) / sum(numbers),
         Age = as.character(max_agestock))

df_weights_Catch_Age <- bind_rows(df_weights_Catch_Age,
                                  df_weights_Catch_AgePlus)

saveRDS(df_weights_Catch_Age,
        file = glue("{path_data_raised}/df_weights_Catch_Age_{data_year}.rds"))

###-----------------------------------------------------------------------------
### Summary df
dfs_canum <- df_canum %>%
  group_by(year, vesselFlagCountry, fleetValue, Age, seasonValue, catchCategory) %>%
  summarise(numbers = sum(numbers, na.rm = TRUE))

dfs_canum_ageplus <- df_canum_ageplus %>%
  group_by(year, vesselFlagCountry, fleetValue, Age, seasonValue, catchCategory) %>%
  summarise(numbers = sum(numbers, na.rm = TRUE))

dfs_canum_ageplus_allocation <- df_canum_ageplus %>%
  group_by(year, Source) %>%
  summarise(numbers = sum(numbers, na.rm = TRUE)) %>%
  group_by(year) %>%
  mutate(numbers_prop = numbers / sum(numbers))

###-----------------------------------------------------------------------------
### plot number allocated per year, fleetValue, catchCategory, Source
dfs_canum_allocated <- df_canum_allocated %>%
  group_by(year, fleetValue, catchCategory, Source) %>%
  summarise(numbers = sum(numbers))

plot_canum_allocated <- ggplot(dfs_canum_allocated,
                               aes(fleetValue, numbers, fill = Source)) +
  geom_col() +
  facet_wrap(catchCategory ~ year, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))

saveRDS(plot_canum_allocated,
        file = glue("{path_data_raised}/plot_canum_allocated.rds"))

saveRDS(dfs_canum_allocated,
        file = glue("{path_data_raised}/dfs_canum_allocated.rds"))

###-----------------------------------------------------------------------------
### Comparison with IC
###-----------------------------------------------------------------------------

### Load age allocation in IC
source(here("WGRDBESstockCoord/personal/JB/load_IC_age_allocated_2024.R"))

plot_canum_allocated_comp <- ggplot(dfs_canum_allocated_comp,
                               aes(fleetValue, numbers, fill = origin)) +
  geom_col(position = position_dodge()) +
  facet_wrap(catchCategory ~ year, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))

plot_canum_allocated_comp
