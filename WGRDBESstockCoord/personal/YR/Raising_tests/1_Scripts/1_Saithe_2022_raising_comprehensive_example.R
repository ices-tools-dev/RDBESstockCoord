#-*- coding: utf-8 -*-

### File: 1_discard_raising_saithe_2022_test.R
### Time-stamp: <2026-02-27 15:33:21 a23579>
###
### Created: 16/06/2025	13:33:57
### Author: Yves Reecht
###
####################################################################################################
### Description:
### 
### 
####################################################################################################


library(dplyr)
library(tidyr)
library(readr)
library(rlang)
## library(lubridate)

scriptDir <- "./1_Scripts"
dataDir <- "./2_Data"
resDir <- "./3_Results"

## Set appropriate WD, based on some usual EDI R-opening behaviours:
if (basename(getwd()) == "1_Scripts")
    setwd("..")

if (dir.exists("./WGRDBESstockCoord/personal/YR/Raising_tests"))
    setwd("./WGRDBESstockCoord/personal/YR/Raising_tests")

## The raising functions:
source(file.path(scriptDir, "0_Functions.R"))
source(file.path(scriptDir, "0_Functions_discards_raising.R"))
source(file.path(scriptDir, "0_Functions_age-length_alloc.R"))

## census <- read_csv(file.path(dataDir, "pok_2022_census_catches.csv"))

## catch_estimates <- read_csv(file.path(dataDir, "pok_2022_estimated_catches.csv"))

distributions <- read_csv(file.path(dataDir, "pok_2022_distributions_CEF_v17.0.csv"))

catch_data <- read_csv(file.path(dataDir, "pok_2022_catches_CEF_v17.0.csv"))

names(distributions)
## names(catch_estimates)
names(catch_data)

## catch_data %>% head(2) %>% as.data.frame()
## distributions %>% group_by(variableUnit) %>% slice_head(n = 1) %>% as.data.frame()

## ##################################################
## Data wrangling for group definitions:
mainCo <- c("France", "Norway", "Germany") # for raising groups.

catch_data <- catch_data %>%
    mutate(Country = vesselFlagCountry,
           Season = seasonValue,
           ## That could be in a function as Metier6 is standard:------------------------
           gear = gsub("^(([^_]+)_([^_]+))_([^_]+)_.*$", "\\2", fleetValue),
           target = gsub("^(([^_]+)_([^_]+))_([^_]+)_.*$", "\\3", fleetValue),
           gear_target = gsub("^(([^_]+)_([^_]+))_([^_]+)_.*$", "\\1", fleetValue),
           mesh = gsub("^(([^_]+)_([^_]+))_([^_]+)_.*$", "\\4", fleetValue),
           Area1 = gsub("27\\.([[:digit:]]+)(\\..*)?", "\\1", areaValue),
           ## ---------------------------------------------------------------------------
           ## TR1 def.:
           FleetType = case_when((gear_target %in% c("OTB_DEF", "OTT_DEF") |
                                  gear %in% c("SDN", "SSC", "PTB")) &
                                 mesh %in% c(">=120", ">=220", "100-119", "120-219") ~ "TR1",
                                 TRUE ~ "Other"))

head(catch_data, 2) %>% as.data.frame()

## ###########################################################################
## Raising discards:

## ##################################################
## group definitions for raising discards:

## Raising strata:
strataCond <-
    list(G1 = quo(Country %in% mainCo & FleetType == "TR1" & Season == 1),
         G2 = quo(Country %in% mainCo & FleetType == "TR1" & Season == 2),
         G3 = quo(Country %in% mainCo & FleetType == "TR1" & Season == 3),
         G4 = quo(Country %in% mainCo & FleetType == "TR1" & Season == 4),
         ## Other métiers, all seasons, areas 46:
         G5 = quo(FleetType == "Other" & Area1 %in% c("4", "6")),
         ## TR1 other countries, area 46 (per quarter or year):
         G6 = quo((! Country %in% mainCo) & FleetType == "TR1" & Area1 %in% c("4", "6") & Season == 1),
         G7 = quo((! Country %in% mainCo) & FleetType == "TR1" & Area1 %in% c("4", "6") & Season == 2),
         G8 = quo((! Country %in% mainCo) & FleetType == "TR1" & Area1 %in% c("4", "6") & Season == 3),
         G9 = quo((! Country %in% mainCo) & FleetType == "TR1" & Area1 %in% c("4", "6") & Season == 4),
         G10 = quo((! Country %in% mainCo) & FleetType == "TR1" & Area1 %in% c("4", "6") & Season == 2022), 
         ## TR1 other countries, area 3 (per quarter):
         G11 = quo((! Country %in% mainCo) & FleetType == "TR1" & Area1 %in% c("3") & Season == 1),
         G12 = quo((! Country %in% mainCo) & FleetType == "TR1" & Area1 %in% c("3") & Season == 2),
         G13 = quo((! Country %in% mainCo) & FleetType == "TR1" & Area1 %in% c("3") & Season == 3),
         G14 = quo((! Country %in% mainCo) & FleetType == "TR1" & Area1 %in% c("3") & Season == 4),
         ## Other métiers, areas 3 (per quarter):
         G15 = quo(FleetType == "Other" & Area1 %in% c("3") & Season == 1),
         G16 = quo(FleetType == "Other" & Area1 %in% c("3") & Season == 2),
         G17 = quo(FleetType == "Other" & Area1 %in% c("3") & Season == 3),
         G18 = quo(FleetType == "Other" & Area1 %in% c("3") & Season == 4))

## Matched data (same length as raising strata above):
matchedDataCond <-
    list(G1 = quo(Country %in% mainCo & FleetType == "TR1" & Season == 1),
         G2 = quo(Country %in% mainCo & FleetType == "TR1" & Season == 2), 
         G3 = quo(Country %in% mainCo & FleetType == "TR1" & Season == 3),
         G4 = quo(Country %in% mainCo & FleetType == "TR1" & Season == 4),
         ## ===========================================================
         ## ! for groups 5 to 9, exclusion of discards from SCO
         ## ===========================================================
         ## Other métiers, all seasons, areas 46:
         G5 = quo(FleetType == "Other" & Area1 %in% c("4", "6") &
                  (! Country %in% "UK(Scotland)")),
         ## TR1 other countries, area 46 (per quarter or year):
         G6 = quo((! Country %in% c(mainCo, "UK(Scotland)")) & FleetType == "TR1" &
                  Area1 %in% c("4", "6") & Season == 1),
         G7 = quo((! Country %in% c(mainCo, "UK(Scotland)")) & FleetType == "TR1" &
                  Area1 %in% c("4", "6") & Season == 2),
         G8 = quo((! Country %in% c(mainCo, "UK(Scotland)")) & FleetType == "TR1" &
                  Area1 %in% c("4", "6") & Season == 3),
         G9 = quo((! Country %in% c(mainCo, "UK(Scotland)")) & FleetType == "TR1" &
                  Area1 %in% c("4", "6") & Season == 4),
         G10 = quo((! Country %in% mainCo) & FleetType == "TR1" & Area1 %in% c("4", "6") & Season == 2022),  
         ## TR1 other countries, area 3 (per quarter):
         G11 = quo((! Country %in% mainCo) & FleetType == "TR1" & Area1 %in% c("3") & Season == 1),
         G12 = quo((! Country %in% mainCo) & FleetType == "TR1" & Area1 %in% c("3") & Season == 2),
         G13 = quo((! Country %in% mainCo) & FleetType == "TR1" & Area1 %in% c("3") & Season == 3),
         G14 = quo((! Country %in% mainCo) & FleetType == "TR1" & Area1 %in% c("3") & Season == 4),
         ## Other métiers, areas 3 (per quarter):
         G15 = quo(FleetType == "Other" & Area1 %in% c("3") & Season == 1),
         G16 = quo(FleetType == "Other" & Area1 %in% c("3") & Season == 2),
         G17 = quo(FleetType == "Other" & Area1 %in% c("3") & Season == 3),
         G18 = quo(FleetType == "Other" & Area1 %in% c("3") & Season == 4))

## ##################################################
## Condition tests (optional automatically ran with
##   raising, but good practice):

cond_test <- check_group_conditions(catch_data = catch_data,
                                    condition_list = strataCond,
                                    logFile = NULL, append = TRUE)

cond_test2 <- check_group_conditions(catch_data = catch_data,
                                     condition_list = matchedDataCond,
                                     conditionType = "matched_data",
                                     logFile = NULL, append = TRUE)

## ##################################################
## Discards raising:

catch_data_raised <-
    raising_cond_loop(catch_data = catch_data,
                      condition_raising_st_list = strataCond,
                      condition_matched_data_list = matchedDataCond, # Optional if same as
                                        # raising strata (condition_raising_st_list)!
                      type = "discards",
                      originType = "WGValue",
                      variableType = "WeightLive", 
                      logFile = "Log.txt",
                      assembled_output = TRUE)


## ###########################################################################
## Age allocation:

## ##################################################
## Group definitions for age allocation:

allocStrataCond <-
    list(GA1 = quo(catchCategory == "LAN" & Area1 == "4" & Season == 1),
         GA2 = quo(catchCategory == "LAN" & Area1 == "4" & Season == 2),
         GA3 = quo(catchCategory == "LAN" & Area1 == "4" & Season == 3),
         GA4 = quo(catchCategory == "LAN" & Area1 == "4" & Season == 4),
         GA5 = quo(catchCategory == "LAN" & Area1 == "4" & Season == 2022),
         ## Landings area 3, per season:
         GA5 = quo(catchCategory == "LAN" & Area1 == "3" & Season == 1),
         GA6 = quo(catchCategory == "LAN" & Area1 == "3" & Season == 2),
         GA7 = quo(catchCategory == "LAN" & Area1 == "3" & Season == 3),
         GA8 = quo(catchCategory == "LAN" & Area1 == "3" & Season == 4),
         ## Landings area 6, all seasons matched together:
         GA9 = quo(catchCategory == "LAN" & Area1 == "6"),
         ## Discards and BMS land., per area groups, all seasons:
         GA10 = quo(catchCategory %in% c("DIS", "BMS") & Area1 %in% c("3", "6")),
         GA11 = quo(catchCategory %in% c("DIS", "BMS") & Area1 == "4"))

bioMatchedCond <-
    list(## Landings area 4, per season:
        GA1 = quo(catchCategory == "LAN" & Area1 == "4" & Season == 1),
        GA2 = quo(catchCategory == "LAN" & Area1 == "4" & Season == 2),
        GA3 = quo(catchCategory == "LAN" & Area1 == "4" & Season == 3),
        GA4 = quo(catchCategory == "LAN" & Area1 == "4" & Season == 4),
        GA5 = quo(catchCategory == "LAN" & Area1 == "4"), # 2022 matched to all seasons
        ## Landings area 3, per season:
        GA6 = quo(catchCategory == "LAN" & Area1 == "3" & Season == 1),
        GA7 = quo(catchCategory == "LAN" & Area1 == "3" & Season == 2),
        GA8 = quo(catchCategory == "LAN" & Area1 == "3" & Season == 3),
        GA9 = quo(catchCategory == "LAN" & Area1 == "3" & Season == 4),
        ## Landings area 6, all seasons together (too few samples):
        GA10 = quo(catchCategory == "LAN" & Area1 == "6"), # All seasons matched to all seasons
        ## Discards, etc., per area groups:
        GA11 = quo(catchCategory %in% c("DIS", "BMS") & Area1 %in% c("3", "6")),
        GA12 = quo(catchCategory %in% c("DIS", "BMS") & Area1 == "4"))

## Check conditions (optional - automatically done during allocation):

cond_bio_test <- check_group_conditions(catch_data = catch_data_raised,
                                        condition_list = allocStrataCond,
                                        logFile = NULL, append = TRUE,
                                        domain = "domainBiology")

sapply(cond_bio_test, head, simplify = FALSE)

cond_bio_test2 <- check_group_conditions(catch_data = catch_data_raised,
                                         condition_list = bioMatchedCond,
                                         conditionType = "matched_data",
                                         logFile = NULL, append = TRUE,
                                         domain = "domainBiology")

## Allocation of catch numbers and mean weights at age:
CaAallocTest <-
    catch_at_AoL_cond_loop(catch_data = catch_data_raised,
                           distribution_data = distributions,
                           condition_alloc_st_list = allocStrataCond,
                           condition_matched_data_list = bioMatchedCond, # Optional if same as
                                        # allocation strata (condition_raising_st_list)!
                           originType_catch = "WGValue", # fraction to apply it to.
                           variableType_catch = "WeightLive",
                           distributionType = "Age",
                           variableType_mean = "WeightLive",
                           logFile = "Log.txt", ## append = TRUE,
                           assembled_output = TRUE)

## Returns a list of table -> extraction of those:
distribution_alloc <- CaAallocTest$distribution
catch_alloc <- CaAallocTest$catch

## ##################################################
## Aggregated catch numbers at age:

## Catch number at age 0 to 10 (not a plus group):
catch_numbers_at_AoL_per_category(distribution_alloc,
                                  catch_alloc,
                                  grouping = c("catchCategory", "importedOrRaised"),
                                  maxAoL = 10,
                                  plusGroup = FALSE) %>% as.data.frame()

## Catch number at age 3 to 10+ (as used in assessment)
##   +default grouping (catchCategory and attributes):
catch_numbers_at_AoL_per_category(distribution_alloc,
                                  catch_alloc,
                                  minAoL = 3,
                                  maxAoL = 10,
                                  plusGroup = TRUE,
                                  round = 0) %>% as.data.frame()


## ##################################################
## Aggregated catch weights at age:

mean_WoL_at_AoL_per_category(distribution_alloc,
                             catch_alloc,
                             minAoL = 3,
                             maxAoL = 10,
                             plusGroup = TRUE,
                             unit = "g",
                             round = 0) %>% as.data.frame()

mean_WoL_at_AoL_per_category(distribution_alloc,
                             catch_alloc,
                             minAoL = 3,
                             maxAoL = 10,
                             plusGroup = TRUE,
                             unit = "kg",
                             round = 3) %>% as.data.frame()

## All catch:
mean_WoL_at_AoL_per_category(distribution_alloc,
                             catch_alloc,
                             grouping = c(##"catchCategory",
                                          "attributeType",
                                          "attibuteValue"),
                             minAoL = 3,
                             maxAoL = 10,
                             plusGroup = TRUE,
                             unit = "g",
                             round = 0) %>% as.data.frame()

## All ages, in kg:
mean_WoL_at_AoL_per_category(distribution_alloc,
                             catch_alloc,
                             grouping = c("catchCategory",
                                          "attributeType",
                                          "attibuteValue"),
                             minAoL = 3,
                             maxAoL = NA,
                             plusGroup = FALSE,
                             unit = "kg",
                             round = 3) %>% as.data.frame()





## ###########################################################################
## Explore and compare results:

## ##################################################
## Discards:
catch_data_raised %>%
    group_by(cc = catchCategory, importedOrRaised) %>%
    slice_sample(n = 1) %>%
    as.data.frame()


catch_data_raised %>%
    group_by(catchCategory) %>%
    summarize(catch_t = sum(total, na.rm = TRUE) * 1e-3)

catch_data_raised %>%
    group_by(catchCategory, importedOrRaised) %>%
    summarize(catch_t = sum(total, na.rm = TRUE) * 1e-3)

## From Intercatch(-like) TAF reproduced raising:
if (all(file.exists(file.path(dataDir,
                              c("pok_2022_CatchAndSampleDataTables-1.csv",
                                "pok_2022_catonR_tot.csv")))))
{
    overview <- read_tsv(file = file.path(dataDir,
                                          "pok_2022_CatchAndSampleDataTables-1.csv")) %>%
        mutate(gear = gsub("^(([^_]+)_([^_]+))_([^_]+)_.*$", "\\2", Fleet),
               target = gsub("^(([^_]+)_([^_]+))_([^_]+)_.*$", "\\3", Fleet),
               gear_target = gsub("^(([^_]+)_([^_]+))_([^_]+)_.*$", "\\1", Fleet),
               mesh = gsub("^(([^_]+)_([^_]+))_([^_]+)_.*$", "\\4", Fleet),
               Area1 = gsub("27\\.([[:digit:]]+)(\\..*)?", "\\1", Area),
               ## TR1 def.:
               FleetType = case_when((gear_target %in% c("OTB_DEF", "OTT_DEF") |
                                      gear %in% c("SDN", "SSC", "PTB")) &
                                     mesh %in% c(">=120", ">=220", "100-119", "120-219") ~ "TR1",
                                     TRUE ~ "Other"))

    ## From WKRDBES_Raise&TAF outputs:
    overview2 <- read_csv(file.path(dataDir, "pok_2022_catonR_tot.csv"))

    head(overview, 2) %>% as.data.frame()
    head(overview2, 2) %>% as.data.frame()
    head(catch_data_raised, 2) %>% as.data.frame()

    overview %>%
        filter(CatchCategory %in% "Discards") %>%
        group_by(CatchCategory, CATONRaisedOrImported, FleetType) %>%
        summarize(catch_t = sum(CATON, na.rm = TRUE) * 1e-3)

    overview2 %>%
        filter(Catch.Cat. %in% "D") %>%
        group_by(Catch.Cat., Discards.Imported.Or.Raised, FleetType) %>%
        summarize(catch_t = sum(Catch..kg, na.rm = TRUE) * 1e-3) %>%
        tail(4)

    catch_data_raised %>%
        filter(catchCategory %in% "DIS") %>%
        group_by(catchCategory, importedOrRaised, FleetType) %>%
        summarize(catch_t = sum(total, na.rm = TRUE) * 1e-3)

    catch_data_raised %>%
        filter(catchCategory %in% "DIS") %>%
        group_by(catchCategory, importedOrRaised, FleetType, Season) %>%
        summarize(catch_t = sum(total, na.rm = TRUE) * 1e-3)

    overview %>%
        filter(CatchCategory %in% "Discards") %>%
        group_by(CatchCategory, CATONRaisedOrImported, FleetType, Season) %>%
        summarize(catch_t = sum(CATON, na.rm = TRUE) * 1e-3)

    catch_data_raised %>% group_by(Season) %>% slice_sample(n = 1) %>% as.data.frame()

    catch_data_raised %>%
        filter(catchCategory %in% "DIS",
               ! Country %in% mainCo) %>%
        group_by(catchCategory, importedOrRaised, FleetType, Season) %>%
        summarize(catch_t = sum(total, na.rm = TRUE) * 1e-3) %>%
        tail(4)

    overview %>%
        filter(CatchCategory %in% "Discards",
               ! Country %in% mainCo) %>%
        group_by(CatchCategory, CATONRaisedOrImported, FleetType, Season) %>%
        summarize(catch_t = sum(CATON, na.rm = TRUE) * 1e-3) %>%
        tail(4)

    overview2 %>%
        filter(Catch.Cat. %in% "D",
               ! Country %in% mainCo) %>%
        group_by(Catch.Cat., Discards.Imported.Or.Raised, FleetType, Season) %>%
        summarize(catch_t = sum(Catch..kg, na.rm = TRUE) * 1e-3) %>%
        tail(4)

    ##
    Comparisons_pok_2022 <- catch_data_raised %>%
        filter(catchCategory %in% "DIS") %>%
        group_by(catchCategory, importedOrRaised, DrGroup) %>%
        summarize(catch_t = sum(total, na.rm = TRUE) * 1e-3) %>%
        inner_join(overview2 %>%
                   filter(Catch.Cat. %in% "D") %>%
                   group_by(Catch.Cat., Discards.Imported.Or.Raised, DrGroup) %>%
                   summarize(catch_t = sum(Catch..kg, na.rm = TRUE) * 1e-3) %>%
                   mutate(Discards.Imported.Or.Raised = tolower(Discards.Imported.Or.Raised)),
                   by = c("importedOrRaised" = "Discards.Imported.Or.Raised",
                          "DrGroup" = "DrGroup"),
                   suffix = c(".new", ".IC")) %>%
        mutate(grN = as.numeric(sub("G", "", DrGroup)),
               Catch.Cat. = NULL,
               perc.change = round(100 * (catch_t.IC - catch_t.new) / catch_t.IC,
                                   2)) %>%
        arrange(grN) %>%
        mutate(grN = NULL)

    Comp_overview_pok_2022 <- catch_data_raised %>%
        group_by(catchCategory, importedOrRaised, originType, variableType) %>%
        summarize(catch_t = sum(total, na.rm = TRUE) * 1e-3) %>%
        mutate(importedOrRaised = ifelse(importedOrRaised %in% c("estimated", "reported"),
                                         "imported", importedOrRaised),
               catchCategoryIC = sub("^(.).*$", "\\1", catchCategory),
               catchCategoryIC = if_else(catchCategory %in% "DIS" &
                                         originType %in% "Official",
                                         "R", catchCategoryIC)) %>%
        full_join(overview2 %>%
                  group_by(Catch.Cat., Discards.Imported.Or.Raised) %>%
                  summarize(catch_t = sum(Catch..kg, na.rm = TRUE) * 1e-3) %>%
                  mutate(Discards.Imported.Or.Raised = tolower(Discards.Imported.Or.Raised)),
                  by = c("importedOrRaised" = "Discards.Imported.Or.Raised",
                         "catchCategoryIC" = "Catch.Cat."),
                  suffix = c(".new", ".IC")) %>%
        arrange(catchCategory) %>%
        mutate(perc.change = round(100 * (catch_t.IC - catch_t.new) / catch_t.IC,
                                   2)) %>%
        select(catchCategory, originType, variableType, importedOrRaised, catchCategoryIC, catch_t.new, catch_t.IC,
               perc.change) %>%
        as.data.frame()

    table(catch_data_raised$DrGroup, catch_data_raised$catchCategory)

    print(Comp_overview_pok_2022)

    print(Comparisons_pok_2022 %>%
          filter(importedOrRaised %in% "raised"),
          n = 1000)
}

## ##################################################
## Age allocations:

stockOverviewRaised <- read_csv(file.path("../2023_pok.27.3a46_RDBES_combined/output",
                                          "StockOverview_raised.txt"))

## stockOverviewRaised %>% head(1) %>% as.data.frame()

stockOverviewRaised %>%
    filter(CatchCat %in% "Discards",
           is.na(Caton)) %>%
    group_by(AGroup, estimated = is.na(Caton)) %>%
    dplyr::summarize(across(starts_with("N.UndeterminedAge"),
                            ~ round(sum(.x, na.rm = TRUE)))) %>%
    rename_with(~sub("N.UndeterminedAge", "N_Age_", .x)) %>%
    select(1:9) %>% as.data.frame()

catch_numbers_at_AoL_per_category(distribution_alloc,
                                  catch_alloc,
                                  grouping = c("catchCategory", "allocGroup", "sampledOrEstimated"),
                                  maxAoL = 6,
                                  plusGroup = FALSE,
                                  round = 0) %>%
    filter(catchCategory %in% "DIS",
           sampledOrEstimated %in% "estimated") %>% as.data.frame()

## CANUM data:
Canum_ICTAF <- read_csv(file.path("../2023_pok.27.3a46_RDBES_combined/output",
                                  "canum_table.csv"))

Canum_New <- catch_numbers_at_AoL_per_category(distribution_alloc,
                                               catch_alloc,
                                               grouping = c("catchCategory"),
                                               maxAoL = 10,
                                               plusGroup = TRUE,
                                               round = NULL) %>%
    bind_rows(catch_numbers_at_AoL_per_category(distribution_alloc,
                                                catch_alloc,
                                                grouping = c("attributeType", "attibuteValue"),
                                                maxAoL = 10,
                                                plusGroup = TRUE,
                                                round = NULL) %>%
              mutate(catchCategory = "CAT")) %>%
    mutate(Catch.Cat. = sub("^(.).*$", "\\1", catchCategory)) %>%
    rename_with(~sub("N_A", "a", .x))

## WECA data:
Weca_ICTAF <- read_csv(file.path("../2023_pok.27.3a46_RDBES_combined/output",
                                  "weca_table.csv"))

Weca_New <- mean_WoL_at_AoL_per_category(distribution_alloc,
                                         catch_alloc,
                                         grouping = c("catchCategory",
                                                      "attributeType",
                                                      "attibuteValue"),
                                         maxAoL = 10,
                                         plusGroup = TRUE,
                                         round = NULL) %>%
    bind_rows(mean_WoL_at_AoL_per_category(distribution_alloc,
                                           catch_alloc,
                                           grouping = c("attributeType", "attibuteValue"),
                                           maxAoL = 10,
                                           plusGroup = TRUE,
                                           round = NULL) %>%
              mutate(catchCategory = "CAT")) %>%
    mutate(Catch.Cat. = sub("^(.).*$", "\\1", catchCategory)) %>%
    rename_with(~sub("W_A", "a", .x))

Weca_New

library(scales)

## Deviations:
Canum_ICTAF %>%
    select(Catch.Cat.) %>%
    left_join(Canum_New %>%
              select(c(Catch.Cat., catchCategory))) %>%
    bind_cols(Canum_ICTAF %>% select(Catch.Cat.) %>%
              left_join(Canum_New %>%
                        select(all_of(colnames(Canum_ICTAF)))) %>%
              select(-Catch.Cat.) /
              Canum_ICTAF %>% select(-Catch.Cat.)) %>%
    mutate(across(where(is.numeric),
                  ~percent(.x - 1, accuracy = 2))) %>%
    as.data.frame()


Weca_ICTAF %>%
    select(Catch.Cat.) %>%
    left_join(Weca_New %>%
              select(c(Catch.Cat., catchCategory))) %>%
    bind_cols(Weca_ICTAF %>% select(Catch.Cat.) %>%
              left_join(Weca_New %>%
                        select(all_of(colnames(Weca_ICTAF)))) %>%
              select(-Catch.Cat.) /
              Weca_ICTAF %>% select(-Catch.Cat.)) %>%
    mutate(across(where(is.numeric),
                  ~percent(.x - 1, accuracy = 2))) %>%
    as.data.frame()

## ####################################################################################################
## ####################################################################################################
## Examples other functions:

## ##################################################
## Unit conversion:
distribution_alloc %>%
    group_by(variableType, sampledOrEstimated) %>%
    slice_head(n = 2) %>%
    ## convert_field("value") %>%
    as.data.frame()

##  Conversion to kg and 1000_pcs:
distribution_alloc %>%
    group_by(variableType, sampledOrEstimated) %>%
    slice_head(n = 2) %>%
    convert_field(valueField = "value",
                  to = c("kg", "1000_pcs")) %>% # ignores silently invalid conversions.
    as.data.frame()

## ###########################################################################
## Example of groups based on a simple strata (one or several fields),
## matched to same:
colnames(catch_data)

testCondGear <- fieldsToStrata(catch_data, # here for discard raising conditions;
                                        # should use catch_data_raised if groups for allocations. 
                               "gear")

## First six elements of each group:
sapply(testCondGear, head)

## Same with a combination of 2 fields
##   (use any tidy synthaxe compatible with dplyr::select()):
testCondGearA <- fieldsToStrata(catch_data,
                                c(gear, Area1))

## The returned object is a list of logical vector
##   (indicating which rows are included in which group):
head(sapply(testCondGearA, head, n = 10, simplify = FALSE))

nrow(catch_data)
sapply(testCondGearA, length)


### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
