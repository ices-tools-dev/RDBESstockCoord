library("tidyverse")
library("here")
library("glue")

################################################################################
### Age allocation from the RDBES CEF download
### (same age allocation as sole8ab_cef_ic_age_allocation.R, but the
### distributions are the DistributionNatEst table downloaded from the RDBES,
### see sol8ab_dl_cef.R, and the catches are the ones raised in
### sole8ab_cef_rdbes_discards_raising.R)
################################################################################
source("R/cef_add_strata.R")
source("R/cef_compute_ref_numbers.R")
source("R/cef_allocate_age_strata.R")

data_year <- 2025
max_agestock <- 8

path_to_data <- here("WGRDBESstockCoord/personal/JB/data")
path_data_cef <- here(glue("{path_to_data}/rdbes_cef/{data_year}"))
path_data_raised <- here(
  glue("WGRDBESstockCoord/personal/JB/output/{data_year}/cef_rdbes"))
dir.create(path_data_raised, showWarnings = FALSE, recursive = TRUE)

###-----------------------------------------------------------------------------
### Load the RDBES distribution table (downloaded and unzipped by
### sol8ab_dl_cef.R)
df_rdbes_dn <- read_csv(file = glue("{path_data_cef}/HNI/DistributionNatEst.csv"),
                        col_names = TRUE,
                        col_types = cols(.default = col_character()))

### Same column names as the distributions returned by funICoutCEF()
names(df_rdbes_dn) <- str_remove(names(df_rdbes_dn), "^DN")

### Same units and conventions as the IC conversion:
### numbers in N (RDBES: NE3 = thousands), mean weights in g (RDBES: kg),
### age only, DisBMS compared as Dis
df_rdbes_dn_age <- df_rdbes_dn |>
  filter(distributionType == "Age") |>
  mutate(year = as.character(year),
         across(c(speciesCode, ageGroupPlus, variance, numPSUs, numTrips,
                  numMeasurements),
                as.numeric),
         value = as.numeric(value),
         distributionClass = as.numeric(distributionClass),
         catchCategory = if_else(catchCategory == "DisBMS", "Dis",
                                 catchCategory))

stopifnot(
  all(df_rdbes_dn_age$variableUnit[df_rdbes_dn_age$variableType == "Number"]
      %in% c("N", "NE3")),
  all(df_rdbes_dn_age$variableUnit[df_rdbes_dn_age$variableType == "WeightLive"]
      %in% c("g", "kg")))

df_rdbes_dn_age <- df_rdbes_dn_age |>
  mutate(value = case_when(variableType == "Number" &
                             variableUnit == "NE3" ~ value * 1000,
                           variableType == "WeightLive" &
                             variableUnit == "kg" ~ value * 1000,
                           TRUE ~ value),
         variableUnit = case_when(variableType == "Number" ~ "N",
                                  variableType == "WeightLive" ~ "g",
                                  TRUE ~ variableUnit))

list_cef <- list(distributions = as.data.frame(df_rdbes_dn_age))

###-----------------------------------------------------------------------------
### extract varialbes used in cef_distribution from cef_catches using domainBiology

## load data raised in sole8ab_cef_rdbes_discards_raising.R
df_stock_overview_L_D_raised <- readRDS(
  glue("{path_data_raised}/df_stock_overview_L_D_raised{data_year}.rds")) |>
  cef_remove_strata() |>
  cef_add_strata() |>
  mutate(year = as.character(year),
         seasonValue = as.character(seasonValue))

### extract combination of domainBiology, catchCategory, strata_full to make the link between catches and distribution
df_catch_domainBiology <- df_stock_overview_L_D_raised |>
  filter(!is.na(domainBiology)) |>
  select(domainBiology,
         catchCategory,
         strata_full) |>
  mutate(catchCategory = case_when(catchCategory == "Dis_total" ~ "Dis",
                                   catchCategory == "BMS_total" ~ "BMS",
                                   catchCategory == "Lan_total" ~ "Lan",
                                   TRUE ~ catchCategory)) |>
  distinct()

###-----------------------------------------------------------------------------
### Merge distribution and catch
### A domainBiology can be used by several strata in the RDBES: each of them
### gets the distribution (the SOP correction below rescales it to its catch)
cef_distribution <- list_cef$distributions |>
  left_join(df_catch_domainBiology |>
              mutate(vesselFlagCountry = str_split_i(strata_full, ";", 2)),
            by = join_by(catchCategory, domainBiology, vesselFlagCountry),
            relationship = "many-to-many")

### Distributions of a domainBiology that is not in the (WGEstimate) catches
### have no stratum and cannot be used
n_no_stratum <- sum(is.na(cef_distribution$strata_full))
if (n_no_stratum > 0) {
  message(n_no_stratum, " distribution rows have no catch stratum and are ",
          "left out")
}

cef_distribution <- cef_distribution |>
  filter(!is.na(strata_full)) |>
  mutate(importedOrRaised = "imported") |>
  droplevels() |>
  mutate(Age = distributionClass)

### Pivot wider to have weight and number in columns
df_canum_domain <- pivot_wider(cef_distribution,
                               values_from = value,
                               id_cols = c(strata_full, domainBiology,
                                           distributionClass, catchCategory,
                                           attributeValue),
                               names_from = c(variableType, valueType),
                               names_sep = "_",
                               unused_fn = dplyr::first)

### The stratum components (country, season, area, fleet, ...) come from the
### catches. They are not parsed from strata_full, because a record can list
### several areas or fleets separated by ";" (as strata_full itself)
df_strata_info <- df_stock_overview_L_D_raised |>
  distinct(strata_full, stock, vesselFlagCountry, seasonValue, areaValue,
           fleetValue, metier6)

### Several domainBiology can end in the same stratum (metier6 = fleet):
### numbers are added up, mean weights are weighted by the numbers
df_canum_imported <- df_canum_domain |>
  summarise(
    WeightLive_Mean = if (sum(Number_Total, na.rm = TRUE) > 0) {
      sum(WeightLive_Mean * Number_Total, na.rm = TRUE) /
        sum(Number_Total[!is.na(WeightLive_Mean)], na.rm = TRUE)
    } else {
      mean(WeightLive_Mean, na.rm = TRUE)
    },
    Number_Total = sum(Number_Total, na.rm = TRUE),
    across(-c(domainBiology, Number_Total, WeightLive_Mean), dplyr::first),
    .by = c(strata_full, distributionClass, catchCategory, attributeValue)) |>
  select(-any_of(c("stock", "vesselFlagCountry", "seasonValue", "areaValue",
                   "fleetValue", "metier6"))) |>
  left_join(df_strata_info, by = join_by(strata_full)) |>
  cef_add_strata()

### SOP correction
source("WGRDBESstockCoord/personal/JB/sole8ab_age_allocation_sop.R")

###-----------------------------------------------------------------------------
### Fill strata with missing individuals sampling data
###-----------------------------------------------------------------------------

###-----------------------------------------------------------------------------
### Landings and discards with age allocation
df_strata_with_age <- df_canum_imported |>
  distinct(strata_full, catchCategory)

df_ref_fleets <- df_stock_overview_L_D_raised |>
  semi_join(df_strata_with_age, by = join_by(strata_full, catchCategory)) |>
  droplevels()
dim(df_ref_fleets)

###-----------------------------------------------------------------------------
### Landings and discards age allocation (per stratum x catch category)
df_allocation_fleets <- df_stock_overview_L_D_raised |>
  anti_join(df_strata_with_age, by = join_by(strata_full, catchCategory)) |>
  droplevels()
dim(df_allocation_fleets)

dim(df_stock_overview_L_D_raised)

strata_no_age <- df_allocation_fleets |>
  distinct(strata_full, catchCategory)

### df_canum_for_allocation use for reference
df_canum_for_allocation <- df_canum_imported_sopr

###-----------------------------------------------------------------------------
# Allocation rules
# Ordered from most specific to most aggregated; strata_y (year) mops up the rest
strata_raising_ordered_full <- c(
  "strata_c_q_g", "strata_q_g", "strata_q_sg", "strata_g",
  "strata_sg", "strata_c_q", "strata_c", "strata_y"
)

allocation_specs <- map(
  strata_raising_ordered_full,
  \(x) list(group_strata = x,
            join_by_cols = c(x, "catchCategory"),
            source_name  = x)
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

  strata_no_age <- strata_no_age |>
    anti_join(distinct(allocated_list[[i]], strata_full, catchCategory),
              by = join_by(strata_full, catchCategory))
}

df_canum_allocated <- bind_rows(allocated_list) |>
  cef_remove_strata() |>
  rename(Caton = Catchkg, weights = weights_ref) |>
  select(-any_of(c("DiscardsImportedOrRaised",
                   "raised_landings",
                   "numbers_ref_tot",
                   "weca_tot",
                   "SoP"))) |>
  mutate(seasonValue = as.character(seasonValue))

df_canum_for_allocation <- df_canum_for_allocation |>
  cef_remove_strata()

df_canum <- bind_rows(df_canum_for_allocation,
                      df_canum_allocated) |>
  cef_remove_strata()

saveRDS(df_canum,
        file = glue("{path_data_raised}/df_canum_all_age_{data_year}.rds"))

df_canum_ageplus <- df_canum |>
  mutate(Age = ifelse(as.numeric(Age) >= as.numeric(max_agestock), "8", Age)) |>
  group_by(strata_full, Age, catchCategory) |>
  mutate(Number_Total = sum(Number_Total)) |>
  distinct()

saveRDS(df_canum,
        file = glue("{path_data_raised}/df_canum_ageplus_{data_year}.rds"))

###-----------------------------------------------------------------------------
df_weights_strata_CatchCat_Age <- df_canum |>
  group_by(strata_full, catchCategory, Age) |>
  mutate(mean_weights = sum(weights * numbers) /  sum(numbers))

saveRDS(df_weights_strata_CatchCat_Age,
        file = glue("{path_data_raised}/df_weights_strata_{data_year}.rds"))

df_weights_CatchCat_Age <- df_canum |>
  filter(as.numeric(Age) < as.numeric(max_agestock)) |>
  group_by(catchCategory, Age) |>
  mutate(mean_weights = sum(weights * numbers) /  sum(numbers),
         Age = as.character(Age))

df_weights_CatchCat_AgePlus <- df_canum |>
  filter(as.numeric(Age) >= as.numeric(max_agestock)) |>
  group_by(catchCategory) |>
  mutate(mean_weights = sum(weights * numbers) / sum(numbers),
         Age = as.character(max_agestock))

df_weights_CatchCat_Age <- bind_rows(df_weights_CatchCat_Age,
                                     df_weights_CatchCat_AgePlus)

saveRDS(df_weights_CatchCat_Age,
        file = glue("{path_data_raised}/df_weights_CatchCat_Age_{data_year}.rds"))

df_weights_Catch_Age <- df_canum |>
  filter(as.numeric(Age) < as.numeric(max_agestock)) |>
  group_by(Age) |>
  mutate(mean_weights = sum(weights * numbers) /  sum(numbers),
         Age = as.character(max_agestock))

df_weights_Catch_AgePlus <- df_canum |>
  filter(as.numeric(Age) >= as.numeric(max_agestock)) |>
  mutate(mean_weights = sum(weights * numbers) / sum(numbers),
         Age = as.character(max_agestock))

df_weights_Catch_Age <- bind_rows(df_weights_Catch_Age,
                                  df_weights_Catch_AgePlus)

saveRDS(df_weights_Catch_Age,
        file = glue("{path_data_raised}/df_weights_Catch_Age_{data_year}.rds"))

###-----------------------------------------------------------------------------
### Summary df
dfs_canum <- df_canum |>
  group_by(year, vesselFlagCountry, fleetValue, Age, seasonValue, catchCategory) |>
  summarise(numbers = sum(numbers, na.rm = TRUE))

dfs_canum_ageplus <- df_canum_ageplus |>
  group_by(year, vesselFlagCountry, fleetValue, Age, seasonValue, catchCategory) |>
  summarise(numbers = sum(numbers, na.rm = TRUE))

dfs_canum_ageplus_allocation <- df_canum_ageplus |>
  group_by(year, Source) |>
  summarise(numbers = sum(numbers, na.rm = TRUE)) |>
  group_by(year) |>
  mutate(numbers_prop = numbers / sum(numbers))

###-----------------------------------------------------------------------------
### plot number allocated per year, fleetValue, catchCategory, Source
dfs_canum_allocated <- df_canum_allocated |>
  group_by(year, fleetValue, catchCategory, Source) |>
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

### IC uses full country names, RDBES uses codes: recode IC to the RDBES codes
country_code <- c(Belgium = "BE", Denmark = "DK", Estonia = "EE",
                  Finland = "FI", France = "FR", Germany = "DE",
                  Ireland = "IE", Latvia = "LV", Lithuania = "LT",
                  Netherlands = "NL", Norway = "NO", Poland = "PL",
                  Portugal = "PT", Spain = "ES", Sweden = "SE",
                  "UK (England)" = "GB-ENG", "UK(England)" = "GB-ENG",
                  "UK(Northern Ireland)" = "GB-NIR",
                  "UK(Scotland)" = "GB-SCT", "UK(Wales)" = "GB-WLS")

recode_country <- function(df) {
  if (!"vesselFlagCountry" %in% names(df)) return(df)
  df |>
    ungroup() |>
    mutate(vesselFlagCountry = as.character(vesselFlagCountry),
           vesselFlagCountry = coalesce(unname(country_code[vesselFlagCountry]),
                                        vesselFlagCountry))
}

df_canum_comp <- recode_country(df_canum_comp)
dfs_canum_comp <- recode_country(dfs_canum_comp)
dfs_canum_ageplus_comp <- recode_country(dfs_canum_ageplus_comp)

### The data from the RDBES is labelled CEF_RDBES
relabel_origin <- function(df) {
  df |> mutate(origin = if_else(origin == "CEF", "CEF_RDBES", origin))
}

df_canum_comp <- relabel_origin(df_canum_comp)
dfs_canum_comp <- relabel_origin(dfs_canum_comp)
dfs_canum_ageplus_comp <- relabel_origin(dfs_canum_ageplus_comp)
dfs_canum_ageplus_allocation_comp <-
  relabel_origin(dfs_canum_ageplus_allocation_comp)
dfs_canum_allocated_comp <- relabel_origin(dfs_canum_allocated_comp)

plot_canum_allocated_comp <- ggplot(dfs_canum_allocated_comp,
                               aes(fleetValue, numbers, fill = origin)) +
  geom_col(position = position_dodge()) +
  facet_wrap(catchCategory ~ year, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))

plot_canum_allocated_comp
