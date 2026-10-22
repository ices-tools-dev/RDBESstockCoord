## Preprocess data, write TAF data tables

## Before:
## After:

library(icesTAF)
library(dplyr)
library(tidyr)
library(grid) # for unit()
library(RColorBrewer)
library(readr)
library(ggplot2)
library(rcef)
library(here)

## The project folder, wherever the repo (RDBESstockCoord) is cloned: every path below is relative to it
here::i_am("WGRDBESstockCoord/personal/Damian/2025/model_discard_raising.R")
setwd(here("WGRDBESstockCoord", "personal", "Damian", "2025"))

mkdir("model")
mkdir("output")

## Every figure of this script is written to output/figures/2_discard_raising (the folders of output/figures are numbered in the
## order the scripts make them), numbered in the order it is drawn.
## What an earlier run left there goes first, and so do the folder this script had before it was numbered and the figures it
## wrote to output/figures itself before it had a folder.
## A figure the report shows has a name as well - fig("name") - and the report finds it by the name, not by the number.
fig <- local({
  dir <- "output/figures/2_discard_raising"
  unlink(c(dir, "output/figures/discard_raising", list.files("output/figures", "^discard_raising_[0-9]+[.]png$", full.names = TRUE)), recursive = TRUE)
  mkdir(dir)
  n <- 0
  function(name = NULL) sprintf("%s/discard_raising_%02d%s.png", dir, n <<- n + 1, if (is.null(name)) "" else paste0("_", name))
})

catch_data <- read.taf(file = "data/catch_data_CEF.csv")

head(catch_data, 2) %>% as.data.frame()
table(catch_data$metier4, useNA = "always")

catch_data_BMS <- catch_data %>%
  filter(catchCategory == "BMS") %>%
  mutate(importedOrRaised  = "imported",
         DrGroup = NA,
         fleetGroup = "BMS")

catch_data <- catch_data %>%
  filter(catchCategory != "BMS") %>%
  mutate(fleetGroup = case_when (metier4 =="TBB" ~"G1_TBB",
                                 metier4 %in% c("GTR", "GNS") ~"G2_GTR_GNS",
                                 metier4 %in% c("OTB", "SDN", "SSC") ~"G3_OTB_SSC_SDN",
                                 !metier4 %in% c("TBB", "GTR", "GNS", "OTB", "SSC", "SDN") & fleetValue != "MIS_MIS_0_0_0_IBC" ~"G4_REST",
                                 fleetValue == "MIS_MIS_0_0_0_IBC" ~"G5_IBC",
                                 TRUE~ metier4) )

## ###########################################################################
## Exploration of the discard coverage and discard rate data to define goups:
## ###########################################################################

### Catch weight per fleet group as reported: the title says "only imported", nothing
### is raised yet. After the raising the same call shows what was raised (see below).
plot_catches(catch = catch_data, x = "metier4", facet = "catchCategory", save = fig("catch_metier"))

### 74 % pf the landing have estimate for discards, which is good.
plot_coverage_bar(catch = catch_data, y = "metier5", save = fig("coverage_metier"))


plot_coverage_bar(catch = catch_data, y = "metier6", facet = "fleetGroup", fill="mesh", palette = "default", save = fig())
### All three G1-3 comply with the minimum 50% coverage to be raised as a group.

plot_coverage_grid(catch = catch_data, y = "metier4", x = "mesh", facet = "fleetGroup", thresholds = list(cov = 50, trips = NA, fish = NA), save = fig("coverage_heat"))


### For the G1_TBB fleet:
###             most discard sampling comes from 70-99 mesh
###             The >120 mm are only partially sampled

### For the G2_GTR_GNS fleet:
###             most discard sampling comes from >220 mesh
###             The 100-219 mm are only partially sampled and represent the second group

### For the G3_OTB_SSC_SDN fleet:
###             most discard sampling comes from the OTB part of the group/

### For the G4_REST fleet:
###             very little sampling, will require using all other values.


plot_coverage_map(catch = catch_data, area = "areaValue", group = "fleetGroup", save = fig())

### For the G1_TBB fleet:
###             discard data is well sampled across the 7.de and 4

### For the G2_GTR_GNS fleet:
###             Mostly well sampled in the 7.de area, only about 25% in 4

### For the G3_OTB_SSC_SDN fleet:
###             Mostly well sampled in the 7.de area, only about 40% in 4

### For the G4_REST fleet:
###             very little sampling, will require using all other values.

######## DISCARD RATE ###############

### AFTER LOOKING AT THE

#### Check discard rates that might not be representative
fleet_high_DR <- build_coverage_strata(catch = catch_data) %>%
  filter (rate  > 0.5 & !metier4 %in% c("MIS")) %>%
  pull(Stratum)

plot_DR(catch = catch_data, x = "areaValue",colour = "mesh", facet = "fleetGroup",jitter = TRUE, rate = "ratio",reference = 0.5, save = fig("dr_area"))

### For the G1_TBB fleet:
###             quite constant discard rate across areas

### For the G2_GTR_GNS fleet:
###             fairly largeer discard rate for the 27.3.21 and some higher values also for 7.d-7.e.

### For the G3_OTB_SSC_SDN fleet:
###             fairly largeer discard rate for the 27.3.21 and some higher values also for 7.d-7.e.


discard_clusters <- build_discard_clusters(catch_data, dims = c("metier4","mesh", "vesselFlagCountry", "seasonValue", "areaValue"), k = 4)
discard_clusters$importance

#### although discard rate are only explained in about 18% by Dim 1 and 2:
     # - the importance of the variables is as such

# variable cramers_v
# areaValue areaValue 0.3284239
# mesh           mesh 0.3191076
# metier4     metier4 0.2940502
# target       target 0.2494560
# Country     Country 0.2059246
# Season       Season 0.1006265

### probably joining Season and Country should happen the first.

plot_discard_clusters(discard_clusters, save = fig())
plot_discard_cluster_variables(discard_clusters, save = fig("cluster_variables"))

## ###########################################################################
## Group definitions for raising discards:
## ###########################################################################

strataCond <-
  list(G1_TBB = quo(metier4 == "TBB"),
       G2_GTR_GNS = quo(metier4 %in% c("GTR", "GNS")),
       G3_OTB_SSC_SDN = quo(metier4 %in% c("OTB", "SSC", "SDN") ),
       G4_REST = quo(!metier4 %in% c("TBB", "GTR", "GNS", "OTB", "SSC", "SDN") & fleetValue != "MIS_MIS_0_0_0_IBC"),
       G5_IBC = quo(fleetValue == "MIS_MIS_0_0_0_IBC" ))

matchedDataCond <-
  list(G1_TBB = quo(metier4 == "TBB"),
       G2_GTR_GNS = quo(metier4 %in% c("GTR", "GNS")),
       G3_OTB_SSC_SDN = quo(metier4 %in% c("OTB", "SSC", "SDN")),
       ## REST group: too little discard data of its own, so it draws on ALL metier4 -- minus the strata with implausible discard ratios.
       G4_REST = quo(fleetValue != "MIS_MIS_0_0_0_IBC" & !Stratum %in% !!fleet_high_DR),
       G5_IBC = quo(fleetValue == "MIS_MIS_0_0_0_IBC"))

## ##################################################
## Condition tests (optional, automatically run with raising, but good practice):
##
## NOTE: check_group_conditions() tests catchCategory %in% "LAN", while this data
##   uses "Lan" -> the coverage warnings can never fire. The duplicate-assignment
##   error still works. Do the manual checks below as well.

invisible(check_group_conditions(catch_data = catch_data,
                                 condition_list = strataCond,
                                 logFile = NULL, append = TRUE))

invisible(check_group_conditions(catch_data = catch_data,
                                 condition_list = matchedDataCond,
                                 conditionType = "matched_data",
                                 logFile = NULL, append = TRUE))


catch_data_raised <-
  catchRaising(catch_data = catch_data,
               condition_raising_st_list = strataCond,
               condition_matched_data_list = matchedDataCond,
               type = "discards",
               originType = "WGEstimate",
               variableType = "WeightLive",
               logFile = "Log.txt",
               assembled_output = TRUE)

catch_data_raised <- catch_data_raised %>%
  rbind(catch_data_BMS)

write.taf(catch_data_raised, file = "model/catch_data_raised.csv")

### The groups the catch was raised with, for the CAR and the CAS of output.R (the condition of G4 carries the strata it leaves out)
saveRDS(list(strataCond = strataCond, matchedDataCond = matchedDataCond), file = "model/discard_raising_groups.rds")

dim(catch_data)
dim(catch_data_raised)

sum(catch_data$total)
sum(catch_data_raised$total, na.rm = TRUE)

catch_data %>%
  group_by(catchCategory) %>%
  summarise(total = sum(total, na.rm = TRUE))

catch_data_raised %>%
  group_by(catchCategory) %>%
  summarise(total = sum(total, na.rm = TRUE))

plot_DR(catch = catch_data_raised, x = "fleetGroup",colour = "mesh",jitter = TRUE, rate = "ratio",reference = 0.5, save = fig("dr_usual"))


catch_data_raised %>%
  group_by(importedOrRaised, catchCategory) %>%
  summarise(tot = sum(total, na.rm = TRUE), .groups = "drop")

catch_data_BMS %>%
  group_by(importedOrRaised, catchCategory) %>%
  summarise(tot = sum(total, na.rm = TRUE), .groups = "drop")


## ###########################################################################
## Look at the raised catch: imported (solid) against raised (transparent)
## ###########################################################################

### Catch weight per fleet group: landings (green) and discards (red), the
### discards that were raised are the transparent part.
plot_catches(catch = catch_data_raised, x = "fleetGroup", save = fig())

### Only the discards, per metier and country, where landings would hide them:
plot_catches(catch = catch_data_raised, x = "metier4", facet = "vesselFlagCountry", catchCategory = "Dis", save = fig())

### The discard ratio of every stratum, imported (solid) and raised (transparent):
### the raised ones should sit among the imported ones of their own group.
plot_DR(catch = catch_data_raised, x = "DrGroup", colour = "mesh", rate = "ratio", reference = 0.5, save = fig())

plot_DR(catch = catch_data_raised, x = "areaValue", colour = "mesh", facet = "fleetGroup", rate = "ratio", reference = 0.5, save = fig())


######################### ALTERNATIVE RAISING ##################
### Section 7: the Kattegat (27.3.a.21) raised on its own. Its objects carry `_3a21`; the raised catch is saved for model_discard_raising_compare.R
### (the report shows this section only when the file is there, so a year that does not run it leaves it out).

## ###########################################################################
## Exploration of the discard coverage and discard rate data to define goups:
## ###########################################################################

### 7
plot_coverage_bar(catch = catch_data %>% filter(areaValue == "27.3.a.21"), y = "metier4", facet = "fleetGroup", save = fig("coverage_3a21"))

plot_coverage_bar(catch = catch_data %>% filter(areaValue %in% c("27.7.d", "27.7.e")), y = "metier4", facet = "fleetGroup", fill= "areaValue", save = fig())


strataCond_3a21 <-
  list(G1_TBB             = quo(metier4 == "TBB"),
       G2_GTR_GNS         = quo(metier4 %in% c("GTR", "GNS") & areaValue != "27.3.a.21"),
       G2b_GTR_GNS_3a21   = quo(metier4 %in% c("GTR", "GNS") & areaValue == "27.3.a.21"),
       G3_OTB_SSC_SDN     = quo(metier4 %in% c("OTB", "SSC", "SDN") & areaValue != "27.3.a.21"),
       G3b_OTB_SSC_SDN_3a21 = quo(metier4 %in% c("OTB", "SSC", "SDN") & areaValue == "27.3.a.21"),
       G4_REST            = quo(!metier4 %in% c("TBB", "GTR", "GNS", "OTB", "SSC", "SDN") & fleetValue != "MIS_MIS_0_0_0_IBC"),
       G5_IBC             = quo(fleetValue == "MIS_MIS_0_0_0_IBC"))

matchedDataCond_3a21 <-
  list(G1_TBB             = quo(metier4 == "TBB"),
       G2_GTR_GNS         = quo(metier4 %in% c("GTR", "GNS") & areaValue != "27.3.a.21"),
       ## 27.3.a.21 raised independently: donors only from the same gears within 27.3.a.21
       G2b_GTR_GNS_3a21   = quo(metier4 %in% c("GTR", "GNS") & areaValue == "27.3.a.21"),
       G3_OTB_SSC_SDN     = quo(metier4 %in% c("OTB", "SSC", "SDN") & areaValue != "27.3.a.21"),
       G3b_OTB_SSC_SDN_3a21 = quo(metier4 %in% c("OTB", "SSC", "SDN") & areaValue == "27.3.a.21"),
       ## REST group: too little discard data of its own, so it draws on ALL metier4 -- minus the strata with implausible discard ratios.
       G4_REST            = quo(fleetValue != "MIS_MIS_0_0_0_IBC" & !Stratum %in% fleet_high_DR),
       G5_IBC             = quo(fleetValue == "MIS_MIS_0_0_0_IBC"))


## ##################################################
## Condition tests (optional, automatically run with raising, but good practice):
##
## NOTE: check_group_conditions() tests catchCategory %in% "LAN", while this data
##   uses "Lan" -> the coverage warnings can never fire. The duplicate-assignment
##   error still works. Do the manual checks below as well.

invisible(check_group_conditions(catch_data = catch_data,
                                 condition_list = strataCond_3a21,
                                 logFile = NULL, append = TRUE))

invisible(check_group_conditions(catch_data = catch_data,
                                 condition_list = matchedDataCond_3a21,
                                 conditionType = "matched_data",
                                 logFile = NULL, append = TRUE))


catch_data_raised_3a21 <-
  catchRaising(catch_data = catch_data,
               condition_raising_st_list = strataCond_3a21,
               condition_matched_data_list = matchedDataCond_3a21,
               type = "discards",
               originType = "WGEstimate",
               variableType = "WeightLive",
               logFile = "Log.txt",
               assembled_output = TRUE)

## Kept for model_discard_raising_compare.R, with the BMS put back as for the usual method
write.taf(rbind(catch_data_raised_3a21, catch_data_BMS), file = "model/catch_data_raised_3a21.csv")


catch_data_raised_3a21 %>%
  group_by(importedOrRaised, catchCategory) %>%
  summarise(tot = sum(total, na.rm = TRUE), .groups = "drop")

catch_data_BMS %>%
  group_by(importedOrRaised, catchCategory) %>%
  summarise(tot = sum(total, na.rm = TRUE), .groups = "drop")



## ###########################################################################
## Look at the raised catch of the alternative raising
## ###########################################################################

plot_catches(catch = catch_data_raised_3a21, x = "fleetGroup", save = fig())

plot_catches(catch = catch_data_raised_3a21, x = "metier4", facet = "vesselFlagCountry", catchCategory = "Dis", save = fig())

plot_DR(catch = catch_data_raised_3a21, x = "DrGroup", colour = "mesh", rate = "ratio", reference = 0.5, save = fig("dr_3a21"))

plot_DR(catch = catch_data_raised_3a21, x = "areaValue", colour = "mesh", facet = "fleetGroup", rate = "ratio", reference = 0.5, save = fig())


## The two scenarios are compared with each other, and with the hierarchical groups, in model_discard_raising_compare.R.
