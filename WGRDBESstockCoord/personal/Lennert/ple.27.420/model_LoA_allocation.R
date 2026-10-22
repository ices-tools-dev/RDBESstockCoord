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
here::i_am("WGRDBESstockCoord/personal/Lennert/2025/model_LoA_allocation.R")
setwd(here("WGRDBESstockCoord", "personal", "Lennert", "2025"))

mkdir("model")
mkdir("output")

## Every figure of this script is written to output/figures/3_age_allocation (the folders of output/figures are numbered in the
## order the scripts make them), numbered in the order it is drawn.
## What an earlier run left there goes first, and so do the folder this script had before it was numbered and the figures it
## wrote to output/figures itself before it had a folder.
fig_dir <- "output/figures/3_age_allocation"
unlink(c(fig_dir, "output/figures/age_allocation", list.files("output/figures", "^age_allocation_[0-9]+[.]png$", full.names = TRUE)), recursive = TRUE)
mkdir(fig_dir)
fig_n <- 0
fig <- function() sprintf("%s/age_allocation_%02d.png", fig_dir, fig_n <<- fig_n + 1)

catch_data_raised <- read.taf(file = "model/catch_data_raised.csv")
distributions_data <- read.taf(file = "data/distributions_data_CEF.csv")

head(catch_data_raised, 2) %>% as.data.frame()
table(catch_data_raised$metier4, useNA = "always")

catch_data_raised_BMS <- catch_data_raised %>%
  filter(catchCategory == "BMS")

catch_data_raised <- catch_data_raised %>%
  filter(catchCategory != "BMS") %>%
  mutate(fleetGroup = case_when (metier4 == "TBB" & mesh == "70-99" & catchCategory == "Lan" ~ "G1",
                                 metier4 == "TBB" & mesh %in% c("100-119", ">=120") & catchCategory == "Lan" ~ "G2",
                                 !(metier4 == "TBB" & mesh %in% c("70-99", "100-119", ">=120")) & catchCategory == "Lan" ~ "G3",
                                 metier4 == "TBB" & mesh == "70-99" & catchCategory == "Dis" ~ "G4",
                                 metier4 == "TBB" & mesh %in% c("100-119", ">=120") & catchCategory == "Dis" ~ "G5",
                                 !(metier4 == "TBB" & mesh %in% c("70-99", "100-119", ">=120")) & catchCategory == "Dis" ~ "G6",
                                 TRUE~ catchCategory))

## ###########################################################################
## Exploration of the discard coverage and discard rate data to define goups:
## ###########################################################################


############### LANDINGS ###############
### 60 % pf the landing have samples for age
plot_coverage_bar(catch = catch_data_raised, distributions = distributions_data, y = "metier4", fill= "mesh",what = "age", catchCategory = "Lan", save = fig())

### All three G1-2 comply with the minimum 50% coverage to be raised as a group.
plot_coverage_bar(catch = catch_data_raised, distributions = distributions_data, y = "metier4", facet = "fleetGroup", fill= "mesh", what = "age", catchCategory = "Lan", save = fig())

plot_coverage_bar(catch = catch_data_raised, distributions = distributions_data, y = "metier4", facet = "fleetGroup", fill= "ICESSubArea", what = "age", catchCategory = "Lan", save = fig())

### Bptj G 1-2 also have at least 50% coverage and 100 measurements of ages. hence they can be allocated independently.
plot_coverage_heat(catch = catch_data_raised, distributions = distributions_data, , y = "fleetGroup",  facet = "fleetGroup", x= "mesh", what = "age", catchCategory = "Lan", min_cov = 50, min_fish =  NA, save = fig())


plot_coverage_map(catch = catch_data_raised, distributions = distributions_data, area = "areaValue", group = "fleetGroup",what = "age", catchCategory = "Lan", save = fig())





plot_coverage_bar(catch = catch_data_raised%>% filter(fleetGroup == "G1"), distributions = distributions_data, y = "metier4", facet = "ICESDivision", fill= "mesh", what = "age", catchCategory = "Lan", save = fig())


######## Numbers at Age ###############

#################### LANDINGS ####################

plot_NaLoA(data = catch_data_raised, distributions = distributions_data, distributionType = "Age", catchCategory = "Lan", fill = "mesh", facet = "fleetGroup",scales = "free",stacked = TRUE, save = fig())

plot_NaLoA(data = catch_data_raised %>% filter(fleetGroup == "G1"), distributions = distributions_data, distributionType = "Age", catchCategory = "Lan", fill = "vesselFlagCountry", facet = "mesh", scales = "free",stacked = TRUE, save = fig())
###
plot_NaLoA(data = catch_data_raised %>% filter(fleetGroup == "G1"), distributions = distributions_data, distributionType = "Age", catchCategory = "Lan", fill =  "mesh", facet ="vesselFlagCountry", scales = "free",stacked = FALSE, save = fig())
###            msot lmength of landings come from the 70-99 mesh. There are differences between countries
###            Do we truest the high peaks in distribution from the DE data? These are rather significant peaks in terms of Numbers.



plot_NaLoA(data = catch_data_raised %>% filter(fleetGroup == "G2"), distributions = distributions_data, distributionType = "Age", catchCategory = "Lan", fill = "mesh", stacked = TRUE, save = fig())

### For the G2_GTR_GNS fleet:
###             landings start from 35 cm onwards and peak at arround 50cm. distributions from 100-219 mm mbarely land any brill.

plot_NaLoA(data = catch_data_raised %>% filter(fleetGroup == "G3"), distributions = distributions_data, distributionType = "Age", catchCategory = "Lan", fill = "metier4", facet = "mesh" ,stacked = TRUE, save = fig())
plot_NaLoA(data = catch_data_raised %>% filter(fleetGroup == "G3"), distributions = distributions_data,  distributionType = "Age", catchCategory = "Lan", fill = "mesh", facet = "areaValue" ,stacked = FALSE, save = fig())





############### DISCARDS ###############

### Not sampled

### 39 % pf the discards have samples for ages
plot_coverage_bar(catch = catch_data_raised, distributions = distributions_data, y = "metier4", what = "age", catchCategory = "Dis", save = fig())

#However the Q1 has more than 50% of the discards with respective samples. So could be allocated independently.
plot_coverage_bar(catch = catch_data_raised, distributions = distributions_data, y = "metier4", facet = "fleetGroup", what = "age", catchCategory = "Dis", save = fig())


plot_coverage_heat(catch = catch_data_raised, distributions = distributions_data, , y = "fleetGroup",  facet = "fleetGroup",  x= "mesh", what = "age", catchCategory = "Dis", min_cov = 50, min_fish =  100, save = fig())


plot_coverage_map(catch = catch_data_raised, distributions = distributions_data, area = "areaValue", group = "fleetGroup",what = "age", catchCategory = "Dis", save = fig())

#################### DISCARDS ####################

plot_NaLoA(data = catch_data_raised, distributions = distributions_data, distributionType = "Age", catchCategory = "Dis", fill = "mesh", facet = "fleetGroup",scales = "free",stacked = TRUE, save = fig())

## ###########################################################################
## Group definitions for raising discards:
## ###########################################################################


allocStrataCond <-  list(# Landings
    G1 = quo(metier4 == "TBB" & mesh == "70-99" & catchCategory == "Lan"),
    G2 =  quo(metier4 == "TBB" & mesh %in% c("100-119", ">=120") & catchCategory == "Lan"),
    G3 = quo(!(metier4 == "TBB" & mesh %in% c("100-119", ">=120")) & !(metier4 == "TBB" & mesh == "70-99") & catchCategory == "Lan"),
    # Discards
    G4 = quo(metier4 == "TBB" & mesh == "70-99" & catchCategory == "Dis"),
    G5 =  quo(metier4 == "TBB" & mesh %in% c("100-119", ">=120") & catchCategory == "Dis"),
    G6 = quo(!(metier4 == "TBB" & mesh %in% c("100-119", ">=120")) & !(metier4 == "TBB" & mesh == "70-99") & catchCategory == "Dis"))

bioMatchedCond <- allocStrataCond

## Labels for the age comparison (6 groups):
allocLabels <- c(G1 = "TBB 70-99 Lan",
                 G2 = "TBB >100 Lan",
                 G3 = "Other Lan",
                 G4 = "TBB 70-99 Dis",
                 G5 = "TBB >100 Dis",
                 G6 = "Other Dis")

## ##################################################
## Condition tests (optional, automatically run with raising, but good practice):
##
## NOTE: check_group_conditions() tests catchCategory %in% "LAN", while this data
##   uses "Lan" -> the coverage warnings can never fire. The duplicate-assignment
##   error still works. Do the manual checks below as well.

cond_bio_test <- check_group_conditions(catch_data = catch_data_raised,
                                        condition_list = allocStrataCond,
                                        logFile = NULL, append = TRUE,
                                        domain = "domainBiology")

cond_bio_test2 <- check_group_conditions(catch_data = catch_data_raised,
                                         condition_list = bioMatchedCond,
                                         conditionType = "matched_data",
                                         logFile = NULL, append = TRUE,
                                         domain = "domainBiology")


distributions_allocated <-
  catchAllocationAoL(catch_data = catch_data_raised,
                     distribution_data = distributions_data,
                     condition_alloc_st_list = allocStrataCond,
                     condition_matched_data_list = bioMatchedCond,
                     originType_catch = "WGEstimate",
                     variableType_catch = "WeightLive",
                     distributionType = "Age",
                     variableType_mean = "WeightLive",
                     weighting = "NumberAtAoL",
                     logFile = "Log.txt",
                     verbose = TRUE,
                     assembled_output = TRUE)

distribution_alloc <- distributions_allocated$distribution
catch_alloc        <- distributions_allocated$catch


distribution_alloc %>% group_by(variableType, catchCategory) %>%
  summarise(n = sum(value, na.rm = T))


## Both tables of the allocation are kept: the distributions, and the catch they belong to - the
## distributions estimated for strata without a biological domain carry a domain the allocation
## made up, which only its own catch has (output.R needs it).
write.taf(distributions_allocated$distribution, file = "model/distributions_data_allocated.csv")
write.taf(distributions_allocated$catch, file = "model/catch_data_allocated.csv") ### LVDP: raised changed to allocated

## ###########################################################################
## Look at the allocation: sampled (solid) against estimated (transparent)
## ###########################################################################

### The plots are the ones used before the allocation, given the two tables of
### the allocation result (its catch AND its distribution), so the distributions
### estimated for the strata without samples are in them - they carry a domain
### the allocation made up, which the catch that went in does not know.

### Numbers at age
plot_NaLoA(distributions_allocated$catch, distributions_allocated$distribution, distributionType = "Age",catchCategory = "Lan", facet = c("mesh", "allocGroup"), scales = "free", stacked = TRUE, save = fig())

plot_NaLoA(distributions_allocated$catch, distributions_allocated$distribution, distributionType = "Age", catchCategory = "Lan", fill = "mesh", facet = c("ICESDivision", "allocGroup"), scales = "free", stacked = TRUE, save = fig())

plot_NaLoA(distributions_allocated$catch, distributions_allocated$distribution, distributionType = "Age", catchCategory = "Lan", fill = "ICESDivision", facet = c("mesh", "allocGroup"), scales = "free", stacked = TRUE, save = fig())



plot_NaLoA(distributions_allocated$catch, distributions_allocated$distribution, distributionType = "Age", catchCategory = "Dis", facet = c("mesh", "allocGroup"), scales = "free", stacked = TRUE, save = fig())

plot_NaLoA(distributions_allocated$catch, distributions_allocated$distribution, distributionType = "Age", catchCategory = "Dis", fill = "mesh", facet = c("ICESDivision", "allocGroup"), scales = "free", stacked = TRUE, save = fig())

plot_NaLoA(distributions_allocated$catch, distributions_allocated$distribution, distributionType = "Age", catchCategory = "Dis", fill = "ICESDivision", facet = c("mesh", "allocGroup"), scales = "free", stacked = TRUE, save = fig())




### Mean weight at age,
plot_MWaLoA(distributions_allocated$catch, distributions_allocated$distribution, distributionType = "Age", catchCategory = "Lan", colour  = "mesh", facet = c("fleetGroup"),plus_group = 8, save = fig())

plot_MWaLoA(distributions_allocated$catch, distributions_allocated$distribution, distributionType = "Age", catchCategory = "Dis", colour = "sampledOrEstimated", , facet = c("mesh", "fleetGroup"), save = fig())


