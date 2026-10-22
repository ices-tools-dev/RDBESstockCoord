## Allocate the lengths with groups chosen from a hierarchy of variables

## Before: model/catch_data_raised_hierarchical.csv (model_discard_raising_hierarchical.R), data/distributions_data_CEF.csv
## After:  model/catch_data_allocated_hierarchical.csv, model/distributions_data_allocated_hierarchical.csv

## The same allocation as model_length_allocation.R (catchAllocationAoL()), but the groups are not written by hand:
## recommend_groups() chooses them, in the same order as the groups of the raising (model_discard_raising_hierarchical.R), from the
## raised catch and the distributions: you name the variables that matter, from the one that matters most to the one that matters
## least, and how well sampled a pool has to be; every stratum is allocated from the narrowest pool that is.
## The two plans are compared in model_length_allocation_compare.R.

library(icesTAF)
library(dplyr)
library(rcef)
library(here)

## The project folder, wherever the repo (RDBESstockCoord) is cloned: every path below is relative to it
here::i_am("WGRDBESstockCoord/personal/Damian/2025/model_length_allocation_hierarchical.R")
setwd(here("WGRDBESstockCoord", "personal", "Damian", "2025"))

mkdir("model")
mkdir("output")

## Every figure of this script is written to output/figures/3b_length_allocation_hierarchical (the folders of output/figures are
## numbered in the order the scripts make them), numbered in the order it is drawn. What an earlier run left there goes first.
## A figure the report shows has a name as well - fig("name") - and the report finds it by the name, not by the number.
fig <- local({
  dir <- "output/figures/3b_length_allocation_hierarchical"
  unlink(dir, recursive = TRUE)
  mkdir(dir)
  n <- 0
  function(name = NULL) sprintf("%s/length_allocation_hierarchical_%02d%s.png", dir, n <<- n + 1, if (is.null(name)) "" else paste0("_", name))
})

### The raised catch of the hierarchical groups (model_discard_raising_hierarchical.R), so that this plan is the hierarchical one
### from the raising to the allocation. BMS is not allocated.
catch_data_raised <- read.taf(file = "model/catch_data_raised_hierarchical.csv") %>%
  filter(catchCategory != "BMS")
distributions_data <- read.taf(file = "data/distributions_data_CEF.csv")

## ###########################################################################
## Exploration: how well are the lengths sampled along the variables of the hierarchy
## ###########################################################################

### The share of the landings, and then of the discards, that has a length distribution (with at least 100 fish measured behind
### it): where the strata can stand on their own and where they have to borrow from a wider pool.
plot_coverage_grid(catch = catch_data_raised, distributions = distributions_data, y = "areaValue", x = "mesh",
                   what = "length", catchCategory = "Lan", thresholds = list(cov = 50, trips = NA, fish = 100), save = fig())

plot_coverage_grid(catch = catch_data_raised, distributions = distributions_data, y = "areaValue", x = "mesh",
                   what = "length", catchCategory = "Dis", thresholds = list(cov = 50, trips = NA, fish = 100), save = fig())

## ###########################################################################
## The groups
## ###########################################################################

### The variables the groups are made by, one to a line, from the one that matters most to the one that matters least. Nothing is
### pooled until it has to be: what already has a length distribution is kept as sampled (L1), the reported strata are summed into
### strata of these variables and tested against the thresholds before anything else is pooled (L2), the ones that fall short are
### pooled across the last variable first, and the variable that matters most is the last one to go (All by All).
hierarchy <- c("areaValue",           # matters most: the last one to be pooled across
               "mesh",
               "vesselFlagCountry",
               "seasonValue")         # matters least: the first one to be pooled across

### How well sampled a pool has to be: cov = the share (%) of its weight that has a length distribution (cannot be empty),
### fish = the least number of fish measured behind it (NA = not used, 100 is a usual floor).
thresholds_length <- list(cov = 50, trips = NA, fish = NA)

### The landings and the discards are grouped on their own, and the catch part is the last variable to be pooled across: the pool of
### the whole catch part is tested first, and only what still falls short is allocated from landings and discards together
### (All by All). With drop_catchCategory = FALSE the catch part is always kept - the landings are never allocated from the discards,
### nor the other way round - and the last level, All by All within the catch part, is accepted whether or not it reaches the
### thresholds.
rec_length <- recommend_groups(catch_data_raised, hierarchy, thresholds_length,
                               distribution_data = distributions_data,
                               drop_catchCategory = TRUE)

### What has a length distribution already, and how the strata fell into the levels (for the landings and for the discards).
rec_length

head(rec_length$groups)

### The same as figures: the weight, by the level its lengths come from. The solid part has a length distribution of its own (L1);
### the hatched parts have none and are allocated from the level in the colour. The landings and the discards are two panels.
plot_recommended_groups(rec_length, save = fig())

plot_recommended_groups(rec_length, share = TRUE, save = fig())

### ... and where the coarse pools are needed:
plot_recommended_groups(rec_length, x = "areaValue", save = fig())

plot_recommended_groups(rec_length, x = "vesselFlagCountry", share = TRUE, save = fig())

plot_river(rec_length, left_var = "vesselFlagCountry", save = fig())

## ###########################################################################
## The allocation
## ###########################################################################

allocation_hierarchical <-
  catchAllocationAoL(catch_data = catch_data_raised,
                     distribution_data = distributions_data,
                     condition_alloc_st_list = rec_length$strataCond,
                     condition_matched_data_list = rec_length$matchedDataCond,
                     originType_catch = "WGEstimate",
                     variableType_catch = "WeightLive",
                     distributionType = "LengthTotal",
                     variableType_mean = "WeightLive",
                     weighting = "NumberAtAoL",
                     logFile = "Log.txt",
                     verbose = TRUE,
                     assembled_output = TRUE)

allocation_hierarchical$distribution %>%
  group_by(variableType, catchCategory) %>%
  summarise(n = sum(value, na.rm = TRUE), .groups = "drop")

## Both tables of the allocation are kept: the distributions, and the catch they belong to - the distributions estimated for
## strata without a biological domain carry a domain the allocation made up, which only its own catch has.
write.taf(allocation_hierarchical$distribution, file = "model/distributions_data_allocated_hierarchical.csv")
write.taf(allocation_hierarchical$catch, file = "model/catch_data_allocated_hierarchical.csv")

## ###########################################################################
## Look at the allocation: sampled (solid) against estimated (transparent)
## ###########################################################################

### The plots are the ones used before the allocation, given the two tables of the allocation result (its catch AND its
### distribution), so the distributions estimated for the strata without samples are in them.

### Numbers at length
plot_NaLoA(allocation_hierarchical$catch, allocation_hierarchical$distribution, catchCategory = "Lan", fill = "mesh", facet = "metier4", scales = "free", stacked = TRUE, save = fig())

plot_NaLoA(allocation_hierarchical$catch, allocation_hierarchical$distribution, catchCategory = "Lan", fill = "mesh", facet = "vesselFlagCountry", scales = "free", stacked = TRUE, save = fig())

plot_NaLoA(allocation_hierarchical$catch, allocation_hierarchical$distribution, catchCategory = "Dis", fill = "mesh", facet = "metier4", scales = "free", stacked = TRUE, save = fig())

plot_NaLoA(allocation_hierarchical$catch, allocation_hierarchical$distribution, catchCategory = "Dis", fill = "mesh", facet = "vesselFlagCountry", scales = "free", stacked = TRUE, save = fig())

### Mean weight at length
plot_MWaLoA(allocation_hierarchical$catch, allocation_hierarchical$distribution, catchCategory = "Lan", colour = "mesh", facet = "metier4", save = fig())

plot_MWaLoA(allocation_hierarchical$catch, allocation_hierarchical$distribution, catchCategory = "Dis", colour = "sampledOrEstimated", facet = c("mesh", "metier4"), save = fig())

### Next: model_length_allocation_compare.R (this plan against the groups written by hand in model_length_allocation.R).
