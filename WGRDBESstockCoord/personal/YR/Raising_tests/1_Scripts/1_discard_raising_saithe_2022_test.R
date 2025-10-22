#-*- coding: utf-8 -*-

### File: 1_discard_raising_saithe_2022_test.R
### Time-stamp: <2025-10-22 08:27:44 a23579>
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

## census <- read_csv(file.path(dataDir, "pok_2022_census_catches.csv"))

## catch_estimates <- read_csv(file.path(dataDir, "pok_2022_estimated_catches.csv"))

distributions <- read_csv(file.path(dataDir, "pok_2022_distributions_RCEF_v16.0.csv"))

catch_data <- read_csv(file.path(dataDir, "pok_2022_catches_RCEF_v16.0.csv"))

names(distributions)
## names(catch_estimates)
names(catch_data)


## ##################################################
## Group definitions:
mainCo <- c("France", "Norway", "Germany") # for raising groups.

## census <- census %>%
##     mutate(Country = vesselFlagCountry,
##            Season = as.character(coalesce(quarter, year)),
##            gear = gsub("^(([^_]+)_([^_]+))_([^_]+)_.*$", "\\2", fleet),
##            target = gsub("^(([^_]+)_([^_]+))_([^_]+)_.*$", "\\3", fleet),
##            gear_target = gsub("^(([^_]+)_([^_]+))_([^_]+)_.*$", "\\1", fleet),
##            mesh = gsub("^(([^_]+)_([^_]+))_([^_]+)_.*$", "\\4", fleet),
##            Area1 = gsub("27\\.([[:digit:]]+)(\\..*)?", "\\1", area),
##            ## TR1 def.:
##            FleetType = case_when((gear_target %in% c("OTB_DEF", "OTT_DEF") |
##                                   gear %in% c("SDN", "SSC", "PTB")) &
##                                  mesh %in% c(">=120", ">=220", "100-119", "120-219") ~ "TR1",
##                                  TRUE ~ "Other"))

## head(census, 2) %>% as.data.frame()

## census$total


catch_data <- catch_data %>%
    mutate(Country = vesselFlagCountry,
           Season = seasonValue,
           gear = gsub("^(([^_]+)_([^_]+))_([^_]+)_.*$", "\\2", metier6),
           target = gsub("^(([^_]+)_([^_]+))_([^_]+)_.*$", "\\3", metier6),
           gear_target = gsub("^(([^_]+)_([^_]+))_([^_]+)_.*$", "\\1", metier6),
           mesh = gsub("^(([^_]+)_([^_]+))_([^_]+)_.*$", "\\4", metier6),
           Area1 = gsub("27\\.([[:digit:]]+)(\\..*)?", "\\1", areaValue),
           ## TR1 def.:
           FleetType = case_when((gear_target %in% c("OTB_DEF", "OTT_DEF") |
                                  gear %in% c("SDN", "SSC", "PTB")) &
                                 mesh %in% c(">=120", ">=220", "100-119", "120-219") ~ "TR1",
                                 TRUE ~ "Other"))

head(catch_data, 2) %>% as.data.frame()

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
## Condition tests (automatically ran with raising):

cond_test <- check_group_conditions(catch_data = catch_data,
                                    condition_list = strataCond,
                                    logFile = NULL, append = TRUE)

cond_test2 <- check_group_conditions(catch_data = catch_data,
                                     condition_list = matchedDataCond,
                                     conditionType = "matched_data",
                                     logFile = NULL, append = TRUE)

## ##################################################
## Discards raising:

discRaisedTest <-
    raising_cond_loop(catch_data = catch_data,
                      condition_raising_st_list = strataCond,
                      condition_matched_data_list = matchedDataCond, # Optional if same as
                                        # raising strata (condition_raising_st_list)!
                      type = "discards",
                      variableType = "WGWeight",
                      logFile = "Log.txt",
                      assembled_output = TRUE)

## ##################################################
## Explore and compare 
discRaisedTest %>%
    group_by(catchCategory) %>%
    slice_sample(n = 1) %>%
    as.data.frame()


discRaisedTest %>%
    group_by(catchCategory) %>%
    summarize(catch_t = sum(total, na.rm = TRUE) * 1e-3)

discRaisedTest %>%
    group_by(catchCategory, dataType) %>%
    summarize(catch_t = sum(total, na.rm = TRUE) * 1e-3)

## From Intercatch:
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
head(discRaisedTest, 2) %>% as.data.frame()

overview %>%
    filter(CatchCategory %in% "Discards") %>%
    group_by(CatchCategory, CATONRaisedOrImported, FleetType) %>%
    summarize(catch_t = sum(CATON, na.rm = TRUE) * 1e-3)

overview2 %>%
    filter(Catch.Cat. %in% "D") %>%
    group_by(Catch.Cat., Discards.Imported.Or.Raised, FleetType) %>%
    summarize(catch_t = sum(Catch..kg, na.rm = TRUE) * 1e-3) %>%
    tail(4)

discRaisedTest %>%
    filter(catchCategory %in% "DIS") %>%
    group_by(catchCategory, dataType, FleetType) %>%
    summarize(catch_t = sum(total, na.rm = TRUE) * 1e-3)

discRaisedTest %>%
    filter(catchCategory %in% "DIS") %>%
    group_by(catchCategory, dataType, FleetType, Season) %>%
    summarize(catch_t = sum(total, na.rm = TRUE) * 1e-3)

overview %>%
    filter(CatchCategory %in% "Discards") %>%
    group_by(CatchCategory, CATONRaisedOrImported, FleetType, Season) %>%
    summarize(catch_t = sum(CATON, na.rm = TRUE) * 1e-3)

discRaisedTest %>% group_by(Season) %>% slice_sample(n = 1) %>% as.data.frame()

discRaisedTest %>%
    filter(catchCategory %in% "DIS",
           ! Country %in% mainCo) %>%
    group_by(catchCategory, dataType, FleetType, Season) %>%
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
Comparisons_pok_2022 <- discRaisedTest %>%
    filter(catchCategory %in% "DIS") %>%
    group_by(catchCategory, dataType, DrGroup) %>%
    summarize(catch_t = sum(total, na.rm = TRUE) * 1e-3) %>%
    inner_join(overview2 %>%
               filter(Catch.Cat. %in% "D") %>%
               group_by(Catch.Cat., Discards.Imported.Or.Raised, DrGroup) %>%
               summarize(catch_t = sum(Catch..kg, na.rm = TRUE) * 1e-3) %>%
               mutate(Discards.Imported.Or.Raised = tolower(Discards.Imported.Or.Raised)),
               by = c("dataType" = "Discards.Imported.Or.Raised",
                      "DrGroup" = "DrGroup"),
               suffix = c(".new", ".IC")) %>%
    mutate(grN = as.numeric(sub("G", "", DrGroup)),
           Catch.Cat. = NULL,
           perc.change = round(100 * (catch_t.IC - catch_t.new) / catch_t.IC,
                               2)) %>%
    arrange(grN) %>%
    mutate(grN = NULL)

Comp_overview_pok_2022 <- discRaisedTest %>%
    group_by(catchCategory, dataType, variableType) %>%
    summarize(catch_t = sum(total, na.rm = TRUE) * 1e-3) %>%
    mutate(dataType = ifelse(dataType %in% c("estimated", "reported"),
                             "imported", dataType),
           catchCategoryIC = sub("^(.).*$", "\\1", catchCategory),
           catchCategoryIC = if_else(catchCategory %in% "DIS" &
                                     variableType %in% "OfficialWeight",
                                     "R", catchCategoryIC)) %>%
    full_join(overview2 %>%
              group_by(Catch.Cat., Discards.Imported.Or.Raised) %>%
              summarize(catch_t = sum(Catch..kg, na.rm = TRUE) * 1e-3) %>%
              mutate(Discards.Imported.Or.Raised = tolower(Discards.Imported.Or.Raised)),
              by = c("dataType" = "Discards.Imported.Or.Raised",
                     "catchCategoryIC" = "Catch.Cat."),
               suffix = c(".new", ".IC")) %>%
    arrange(catchCategory) %>%
    mutate(perc.change = round(100 * (catch_t.IC - catch_t.new) / catch_t.IC,
                               2)) %>%
    select(catchCategory, variableType, dataType, catchCategoryIC, catch_t.new, catch_t.IC, perc.change)
    

### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
