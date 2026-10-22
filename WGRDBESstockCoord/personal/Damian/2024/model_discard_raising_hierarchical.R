## Raise the discards with groups chosen from a hierarchy of variables

## Before: data/catch_data_CEF.csv
## After:  model/catch_data_raised_hierarchical.csv

## The same catch and the same raising as model_discard_raising.R (catchRaising()), but the groups are not written by hand:
## recommend_groups() chooses them. You name the variables that matter, from the one that matters most to the one that matters
## least, and how well sampled a pool has to be; every stratum is raised from the narrowest pool that is.
## This raising is compared with the two of model_discard_raising.R in model_discard_raising_compare.R, and the same hierarchy
## makes the groups of the lengths in model_length_allocation_hierarchical.R.

library(icesTAF)
library(dplyr)
library(rcef)
library(here)

## The project folder, wherever the repo (RDBESstockCoord) is cloned: every path below is relative to it
here::i_am("WGRDBESstockCoord/personal/Damian/2024/model_discard_raising_hierarchical.R")
setwd(here("WGRDBESstockCoord", "personal", "Damian", "2024"))

mkdir("model")
mkdir("output")

## Every figure of this script is written to output/figures/2b_discard_raising_hierarchical (the folders of output/figures are
## numbered in the order the scripts make them), numbered in the order it is drawn. What an earlier run left there goes first.
## A figure the report shows has a name as well - fig("name") - and the report finds it by the name, not by the number.
fig <- local({
  dir <- "output/figures/2b_discard_raising_hierarchical"
  unlink(dir, recursive = TRUE)
  mkdir(dir)
  n <- 0
  function(name = NULL) sprintf("%s/discard_raising_hierarchical_%02d%s.png", dir, n <<- n + 1, if (is.null(name)) "" else paste0("_", name))
})

catch_data <- read.taf(file = "data/catch_data_CEF.csv")

## BMS is not a discard: it stays out of the raising and is put back after it
catch_data_BMS <- catch_data %>%
  filter(catchCategory == "BMS") %>%
  mutate(importedOrRaised = "imported",
         DrGroup = NA)

catch_data <- catch_data %>%
  filter(catchCategory != "BMS")

## ###########################################################################
## Exploration: which variables matter for the discards, and how well are they sampled
## ###########################################################################

### The variables that matter most for the discard rate are the ones to keep apart the longest: how strongly each one
### separates the clusters of strata (the higher, the more it matters).
discard_clusters <- build_discard_clusters(catch_data, dims = c("metier4", "mesh", "vesselFlagCountry", "seasonValue", "areaValue"), k = 4)
discard_clusters$importance

plot_discard_clusters(discard_clusters, save = fig())
plot_discard_cluster_variables(discard_clusters, save = fig())

### How much of the landings has a discard estimate along the variables of the hierarchy: where the strata can stand on their own
### data (the coverage threshold, the same that the groups below use) and where they have to borrow from a wider pool.
plot_coverage_grid(catch = catch_data, y = "areaValue", x = "mesh", thresholds = list(cov = 50, trips = NA, fish = NA), save = fig())

plot_coverage_grid(catch = catch_data, y = "vesselFlagCountry", x = "seasonValue", thresholds = list(cov = 50, trips = NA, fish = NA), save = fig())

### And how alike the discard ratios are inside a variable: the pool of a variable is only worth sharing if they are.
plot_DR(catch = catch_data, x = "areaValue", colour = "mesh", jitter = TRUE, rate = "ratio", reference = 0.5, save = fig())

## ###########################################################################
## The groups
## ###########################################################################

### The variables the groups are made by, one to a line, from the one that matters most to the one that matters least (the area
### matters more than the mesh, the mesh more than the country ...). Nothing is pooled until it has to be, and the levels are
### numbered from the narrowest:
###   L1  the strata as they are reported (area, season, country, fleet): a landing that has a discard estimate is never raised;
###   L2  the reported strata summed into strata of these variables - what is not named, the metier, is summed over - and each of
###       them tested against the thresholds before anything else is pooled: the ones that reach them are raised from themselves
###       (when the variables are the reported stratum already there is nothing to sum: L2 is skipped and L1 is the own stratum);
###   L3  the ones that fall short pooled across the last variable (the same area, mesh and country in all seasons) and tested
###       again, then L4 across the country as well, L5 across the mesh, and in the end
###   L6  All by All: everything.
### So the variable that matters most, the area, is the last one to be pooled across.
hierarchy <- c("areaValue",           # matters most: the last one to be pooled across
               "mesh",
               "vesselFlagCountry",
               "seasonValue")         # matters least: the first one to be pooled across

### How well sampled a pool has to be: cov = the share (%) of its landings that has a discard estimate (cannot be empty);
### trips = the least number of trips behind it and fish = the least number of fish measured (NA = not used).
thresholds <- list(cov = 50, trips = NA, fish = NA)

rec <- recommend_groups(catch_data, hierarchy, thresholds)

### First what needs no raising at all (the landings that have a discard estimate), then how the strata fell into the levels: how
### many groups and strata each level takes, how much of the landings rests on it, and how much of what has to be raised (the
### landings without a discard estimate of their own) it settles.
rec

### One row for each group (its pool and what it holds) and one for each stratum (the level and the group it is in, and what the
### pool it was given stands on): open them whole with View(rec$groups) and View(rec$strata).
head(rec$groups)
head(rec$strata)

### The same as figures: the landings, by the level their discards come from. The solid part is the landings that have a discard
### estimate of their own (L1); the hatched parts have none and are raised from the level in the colour ...
plot_recommended_groups(rec, save = fig())

### ... as a share of the landings ...
plot_recommended_groups(rec, share = TRUE, save = fig())

### ... and where the coarse pools are needed: the areas, the meshes, the countries ...
plot_recommended_groups(rec, x = "areaValue", save = fig())

plot_recommended_groups(rec, x = "mesh", save = fig())

plot_recommended_groups(rec, x = "vesselFlagCountry", share = TRUE, save = fig())

### ... and the flow of every area, or every country, to the level it is raised at.
plot_river(rec, left_var = "areaValue", save = fig())

plot_river(rec, left_var = "vesselFlagCountry", save = fig())

### What catchRaising() takes: the strata each group raises and the data its discard ratio is taken from (the first three of each).
rec$strataCond[1:3]
rec$matchedDataCond[1:3]

catch_data_raised <-
  catchRaising(catch_data = catch_data,
               condition_raising_st_list = rec$strataCond,
               condition_matched_data_list = rec$matchedDataCond,
               type = "discards",
               originType = "WGEstimate",
               variableType = "WeightLive",
               logFile = "Log.txt",
               assembled_output = TRUE)

catch_data_raised <- catch_data_raised %>%
  rbind(catch_data_BMS)

write.taf(catch_data_raised, file = "model/catch_data_raised_hierarchical.csv")

dim(catch_data)
dim(catch_data_raised)

catch_data %>%
  group_by(catchCategory) %>%
  summarise(total = sum(total, na.rm = TRUE))

catch_data_raised %>%
  group_by(importedOrRaised, catchCategory) %>%
  summarise(tot = sum(total, na.rm = TRUE), .groups = "drop")

## ###########################################################################
## Look at the raised catch: imported (solid) against raised (transparent)
## ###########################################################################

### Catch weight per area: landings and discards, the discards that were raised are the transparent part.
plot_catches(catch = catch_data_raised, x = "areaValue", save = fig())

### Only the discards, per mesh and country, where landings would hide them:
plot_catches(catch = catch_data_raised, x = "mesh", facet = "vesselFlagCountry", catchCategory = "Dis", save = fig())

### The discard ratio of every stratum, imported (solid) and raised (transparent): the raised ones should sit among the
### imported ones of their own area and mesh.
plot_DR(catch = catch_data_raised, x = "areaValue", colour = "mesh", rate = "ratio", reference = 0.5, save = fig())

### Next: model_discard_raising_compare.R (this raising against the two of model_discard_raising.R) and
### model_length_allocation_hierarchical.R (the same hierarchy for the lengths).
