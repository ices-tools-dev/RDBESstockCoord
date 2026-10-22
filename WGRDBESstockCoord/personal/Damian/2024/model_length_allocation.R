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
here::i_am("WGRDBESstockCoord/personal/Damian/2024/model_length_allocation.R")
setwd(here("WGRDBESstockCoord", "personal", "Damian", "2024"))

mkdir("model")
mkdir("output")

## Every figure of this script is written to output/figures/3_length_allocation (the folders of output/figures are numbered in the
## order the scripts make them), numbered in the order it is drawn.
## What an earlier run left there goes first, and so do the folder this script had before it was numbered and the figures it
## wrote to output/figures itself before it had a folder.
## A figure the report shows has a name as well - fig("name") - and the report finds it by the name, not by the number.
fig <- local({
  dir <- "output/figures/3_length_allocation"
  unlink(c(dir, "output/figures/length_allocation", list.files("output/figures", "^length_allocation_[0-9]+[.]png$", full.names = TRUE)), recursive = TRUE)
  mkdir(dir)
  n <- 0
  function(name = NULL) sprintf("%s/length_allocation_%02d%s.png", dir, n <<- n + 1, if (is.null(name)) "" else paste0("_", name))
})

catch_data_raised <- read.taf(file = "model/catch_data_raised.csv")
distributions_data <- read.taf(file = "data/distributions_data_CEF.csv")

head(catch_data_raised, 2) %>% as.data.frame()
table(catch_data_raised$metier4, useNA = "always")


catch_data_raised <- catch_data_raised %>%
  mutate(fleetGroup = case_when (metier4 =="TBB" ~"G1_TBB",
                                 metier4 %in% c("GTR", "GNS") ~"G2_GTR_GNS",
                                 # metier4 %in% c("OTB", "SDN", "SSC") ~"G3_OTB_SSC_SDN",
                                 !metier4 %in% c("TBB", "GTR", "GNS") & fleetValue != "MIS_MIS_0_0_0_IBC" ~"G4_REST",
                                 fleetValue == "MIS_MIS_0_0_0_IBC" ~"G5_IBC",
                                 TRUE~ metier4) )

## ###########################################################################
## Exploration of the discard coverage and discard rate data to define goups:
## ###########################################################################


############### LANDINGS ###############
### 60 % pf the landing have samples for lengths
plot_coverage_bar(catch = catch_data_raised, distributions = distributions_data, y = "metier4", fill= "mesh",what = "length", catchCategory = "Lan", save = fig())

### All three G1-2 comply with the minimum 50% coverage to be raised as a group.
plot_coverage_bar(catch = catch_data_raised, distributions = distributions_data, y = "metier4", facet = "fleetGroup", fill= "mesh", what = "length", catchCategory = "Lan", save = fig("len_lan"))

plot_coverage_bar(catch = catch_data_raised, distributions = distributions_data, y = "metier4", facet = "fleetGroup", fill= "ICESSubArea", what = "length", catchCategory = "Lan", save = fig())

### Bptj G 1-2 also have at least 50% coverage and 100 measurements of lengths. hence they can be allocated independently.
plot_coverage_grid(catch = catch_data_raised, distributions = distributions_data, y = "fleetGroup",  facet = "fleetGroup", x= "mesh", what = "length", catchCategory = "Lan", thresholds = list(cov = 50, trips = NA, fish = 100), save = fig("len_heat"))


plot_coverage_map(catch = catch_data_raised, distributions = distributions_data, area = "areaValue", group = "fleetGroup",what = "length", catchCategory = "Lan", save = fig())





plot_coverage_bar(catch = catch_data_raised%>% filter(fleetGroup == "G3_OTB_SSC_SDN"), distributions = distributions_data, y = "metier4", facet = "ICESDivision", fill= "mesh", what = "length", catchCategory = "Lan", save = fig())

### For the G1_TBB fleet:
###             length data is well sampled across the 7.de and 4

### For the G2_GTR_GNS fleet:
###             Mostly well sampled in the 7.de area

### For the G3_OTB_SSC_SDN fleet:
###             Mostly well sampled in the 7.de area

### For the G4_REST fleet:
###             very little sampling, will require using all other values.

######## Numbers at Length ###############

#################### LANDINGS ####################

plot_NaLoA(data = catch_data_raised, distributions = distributions_data, catchCategory = "Lan", fill = "mesh", facet = "fleetGroup",scales = "free",stacked = TRUE, save = fig())

plot_NaLoA(data = catch_data_raised %>% filter(fleetGroup == "G1_TBB"), distributions = distributions_data, catchCategory = "Lan", fill = "vesselFlagCountry", facet = "mesh", scales = "free",stacked = TRUE, save = fig())
### For the G1_TBB fleet:
####                Onlyy legnths from >120 are the Belgian one. Thesestart from 30 cm while the 70-99 start with length from 24cm.
###
plot_NaLoA(data = catch_data_raised %>% filter(fleetGroup == "G1_TBB"), distributions = distributions_data, catchCategory = "Lan", fill =  "mesh", facet ="vesselFlagCountry", scales = "free",stacked = FALSE, save = fig())
###            msot lmength of landings come from the 70-99 mesh. There are differences between countries
###            Do we truest the high peaks in distribution from the DE data? These are rather significant peaks in terms of Numbers.



plot_NaLoA(data = catch_data_raised %>% filter(fleetGroup == "G2_GTR_GNS"), distributions = distributions_data, catchCategory = "Lan", fill = "mesh", stacked = TRUE, save = fig())

### For the G2_GTR_GNS fleet:
###             landings start from 35 cm onwards and peak at arround 50cm. distributions from 100-219 mm mbarely land any brill.

plot_NaLoA(data = catch_data_raised %>% filter(fleetGroup == "G3_OTB_SSC_SDN"), distributions = distributions_data, catchCategory = "Lan", fill = "metier4", facet = "mesh" ,stacked = TRUE, save = fig())
plot_NaLoA(data = catch_data_raised %>% filter(fleetGroup == "G3_OTB_SSC_SDN"), distributions = distributions_data, catchCategory = "Lan", fill = "mesh", facet = "areaValue" ,stacked = FALSE, save = fig())
### For the G3_OTB_SSC_SDN fleet:
###             all length samples come from the OTB fleet and nothing from the seines.
###             distribution caries quite a bit across mesh sizes, with landings startin,g at around 30cm for the mesh larger than 100 and at around 25cm fdor the 70-99.





############### DISCARDS ###############

### 39 % pf the discards have samples for lengths
plot_coverage_bar(catch = catch_data_raised, distributions = distributions_data, y = "metier4", what = "length", catchCategory = "Dis", save = fig())

#However the Q1 has more than 50% of the discards with respective samples. So could be allocated independently.
plot_coverage_bar(catch = catch_data_raised, distributions = distributions_data, y = "metier4", facet = "fleetGroup", what = "length", catchCategory = "Dis", save = fig("len_dis"))


plot_coverage_grid(catch = catch_data_raised, distributions = distributions_data, y = "fleetGroup",  facet = "fleetGroup",  x= "mesh", what = "length", catchCategory = "Dis", thresholds = list(cov = 50, trips = NA, fish = 100), save = fig())


### For the G1_TBB fleet:
###             most landings length sampling comes from 70-99 mesh
###             The >120 mm are sampled only marginally. The rest isn't

### For the G2_GTR_GNS fleet:
###             most landings length sampling comes from >220 mesh
###             The 100-219 mm are only partially sampled and represent the second group

plot_coverage_map(catch = catch_data_raised, distributions = distributions_data, area = "areaValue", group = "fleetGroup",what = "length", catchCategory = "Dis", save = fig())

### For the G1_TBB fleet:
###             length data is well sampled across the 7.de and 4

### For the G2_GTR_GNS fleet:
###            Most samples comes from the 7.e, but only about 30% of landings. No samples coming from other areas.

### For the G3_OTB_SSC_SDN fleet:
###             Very little samples coming from 7.e and 4.

### For the G4_REST fleet:
###             very little sampling, will require using all other values.

#################### DISCARDS ####################

plot_NaLoA(data = catch_data_raised, distributions = distributions_data, catchCategory = "Dis", fill = "mesh", facet = "fleetGroup",scales = "free",stacked = TRUE, save = fig())

plot_NaLoA(data = catch_data_raised %>% filter(fleetGroup == "G1_TBB"), distributions = distributions_data, catchCategory = "Dis", facet = "mesh", scales = "free",stacked = TRUE, save = fig())
### For the G1_TBB fleet:
####                TBB 70-99 Dis is weel sampled. >120 is only one fish probably.
###

plot_NaLoA(data = catch_data_raised %>% filter(fleetGroup == "G2_GTR_GNS"), distributions = distributions_data, catchCategory = "Dis", fill = "mesh", stacked = TRUE, save = fig())

### For the G2_GTR_GNS fleet:
###             landings start from 35 cm onwards and peak at arround 50cm. distributions from 100-219 mm mbarely land any brill.

plot_NaLoA(data = catch_data_raised %>% filter(fleetGroup == "G3_OTB_SSC_SDN"), distributions = distributions_data, catchCategory = "Dis", fill = "metier4", facet = "mesh" ,stacked = TRUE, save = fig())

### For the G3_OTB_SSC_SDN fleet:
###             all length samples come from the OTB fleet and nothing from the seines.
###             distribution caries quite a bit across mesh sizes, with landings startin,g at around 30cm for the mesh larger than 100 and at around 25cm fdor the 70-99.


## ###########################################################################
## Group definitions for raising discards:
## ###########################################################################

allocStrataCond <-
  list(G1_TBB_LAN = quo(catchCategory == "Lan" & metier4 == "TBB"),
       G2_GTR_GNS_LAN = quo(catchCategory == "Lan" & metier4 %in% c("GTR", "GNS")),
       G3_REST_LAN = quo(catchCategory == "Lan" & !metier4 %in% c("TBB", "GTR", "GNS") &
                   fleetValue != "MIS_MIS_0_0_0_IBC"),
       G5_DIS = quo(catchCategory %in% c("Dis", "BMS") &  fleetValue != "MIS_MIS_0_0_0_IBC"),
       G6_IBC = quo(fleetValue == "MIS_MIS_0_0_0_IBC"))

bioMatchedCond <-
  list(G1_TBB_LAN = quo(catchCategory == "Lan" &  metier4== "TBB"),
       G2_GTR_GNS_LAN = quo(catchCategory == "Lan" & metier4 %in% c("GTR", "GNS")),
       G3_REST_LAN = quo(catchCategory == "Lan"),
       G5_DIS = quo(catchCategory %in% c("Dis", "BMS")),
       G6_IBC = quo(catchCategory %in% c("Lan", "Dis", "BMS")))


## ##################################################
## Condition tests (optional, automatically run with raising, but good practice):
##
## NOTE: check_group_conditions() tests catchCategory %in% "LAN", while this data
##   uses "Lan" -> the coverage warnings can never fire. The duplicate-assignment
##   error still works. Do the manual checks below as well.

invisible(check_group_conditions(catch_data = catch_data_raised,
                                 condition_list = allocStrataCond,
                                 logFile = NULL, append = TRUE,
                                 domain = "domainBiology"))

invisible(check_group_conditions(catch_data = catch_data_raised,
                                 condition_list = bioMatchedCond,
                                 conditionType = "matched_data",
                                 logFile = NULL, append = TRUE,
                                 domain = "domainBiology"))


allocation <-
  catchAllocationAoL(catch_data = catch_data_raised,
                     distribution_data = distributions_data,
                     condition_alloc_st_list = allocStrataCond,
                     condition_matched_data_list = bioMatchedCond,
                     originType_catch = "WGEstimate",
                     variableType_catch = "WeightLive",
                     distributionType = "LengthTotal",
                     variableType_mean = "WeightLive",
                     weighting = "NumberAtAoL",
                     logFile = "Log.txt",
                     verbose = TRUE,
                     assembled_output = TRUE)

allocation$distribution %>% group_by(variableType, catchCategory) %>%
  summarise(n = sum(value, na.rm = TRUE))


## Both tables of the allocation are kept: the distributions, and the catch they belong to - the
## distributions estimated for strata without a biological domain carry a domain the allocation
## made up, which only its own catch has (output.R needs it).
write.taf(allocation$distribution, file = "model/distributions_data_allocated.csv")
write.taf(allocation$catch, file = "model/catch_data_raised.csv")

## ###########################################################################
## Look at the allocation: sampled (solid) against estimated (transparent)
## ###########################################################################

### The plots are the ones used before the allocation, given the two tables of
### the allocation result (its catch AND its distribution), so the distributions
### estimated for the strata without samples are in them - they carry a domain
### the allocation made up, which the catch that went in does not know.

### Numbers at length
plot_NaLoA(allocation$catch, allocation$distribution, catchCategory = "Lan", facet = c("mesh", "allocGroup"), scales = "free", stacked = TRUE, save = fig())

plot_NaLoA(allocation$catch, allocation$distribution, catchCategory = "Lan", fill = "mesh", facet = c("ICESDivision", "allocGroup"), scales = "free", stacked = TRUE, save = fig("nal_lan"))

plot_NaLoA(allocation$catch, allocation$distribution, catchCategory = "Lan", fill = "ICESDivision", facet = c("mesh", "allocGroup"), scales = "free", stacked = TRUE, save = fig())



plot_NaLoA(allocation$catch, allocation$distribution, catchCategory = "Dis", facet = c("mesh", "allocGroup"), scales = "free", stacked = TRUE, save = fig())

plot_NaLoA(allocation$catch, allocation$distribution, catchCategory = "Dis", fill = "mesh", facet = c("ICESDivision", "allocGroup"), scales = "free", stacked = TRUE, save = fig("nal_dis"))

plot_NaLoA(allocation$catch, allocation$distribution, catchCategory = "Dis", fill = "ICESDivision", facet = c("mesh", "allocGroup"), scales = "free", stacked = TRUE, save = fig())




### Mean weight at length
plot_MWaLoA(allocation$catch, allocation$distribution, catchCategory = "Lan", colour  = "mesh", facet = c("fleetGroup"), save = fig("mw_lan"))

plot_MWaLoA(allocation$catch, allocation$distribution, catchCategory = "Dis", colour = "sampledOrEstimated",  facet = c("mesh", "fleetGroup"), save = fig())


