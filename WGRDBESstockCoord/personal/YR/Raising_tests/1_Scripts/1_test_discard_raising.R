#-*- coding: utf-8 -*-

### File: 1_test_discard_raising.R
### Time-stamp: <2026-02-27 15:49:09 a23579>
###
### Created: 23/10/2024	08:49:07
### Author: Yves Reecht
###
####################################################################################################
### Description:
### 
### NOT UP-TO-DATE
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

if (basename(getwd()) == "1_Scripts") setwd("..")

source(file.path(scriptDir, "0_Functions_other.R"))
source(file.path(scriptDir, "0_Functions_discards_raising.R"))

census <- read_csv(file.path(dataDir, "census_data_v14.csv"))

catch_estimates <- read_csv(file.path(dataDir, "estimated_data_v14.csv"))

census %>% head(3) %>% as.data.frame()

catch_estimates %>% head(2) %>% as.data.frame()

dim(census)

## ###########################################################################
## Step 1: extract data to raise and to estimate discards ratios.
##         This would later be wrapped in a function that takes two conditions

## Need to think about a proper nomenclature for groups!

## Census DF for one group in which the same discard ratio will be applied.
##   contains bot landings with discards estimates and landings without (see domainCatchDis key).
##
## Based on condition 1:
raising_grp_cdf <- census %>%
    filter(stock == "cod.27.21",
           fleet == "Passive")

raising_grp_cdf %>% group_by(is.na(domainCatchDis)) %>%
    sample_n(1) %>% as.data.frame()

## Census DF with data used to estimate the discard ratio:
##  * most commonly the same as `estimation_grp_cdf`, but might include extra data.
##  * must make sure that all data corresponding to a given domainCatchDis key are included
##    (otherwise the corresponding ratio estimate is wrong).
##
## Based on condition 2:

## To be able to check whether all data included, we store a group index
gidx <- census$stock == "cod.27.21" &
    census$fleet == "Passive" |> replace_na(replace = FALSE)

data_grp_cdf <- census[gidx, ]

data_grp_cdf <- data_grp_cdf %>%
    ## Add any missing data with the same domainCatchDis key:
    bind_rows(census[! gidx, ] %>%
              ## Should additionnaly match on stock and year as can be duplicates otherwise.
              filter(domainCatchDis %in% na.omit(data_grp_cdf$domainCatchDis))) %>%
    ## Filter out data without discard estimates:
    filter(! is.na(domainCatchDis))


## ###########################################################################
## Step 2: raising...


## debugonce(grp_catch_raising)  # to stop in the function environment and execute step by step.

grp_raised_census <- raising_grp_cdf %>%
    mutate(dataType = "census") %>%
    bind_rows(grp_catch_raising(raising_st_census = raising_grp_cdf,
                                matched_data_census = data_grp_cdf,
                                catch_estimates = catch_estimates)) ## %>%
    ## bind_rows(grp_catch_raising(raising_st_census = raising_grp_cdf,
    ##                             matched_data_census = data_grp_cdf,
    ##                             catch_estimates = catch_estimates,
    ##                             type = "BMS"))


grp_raised_census %>%
    arrange(vesselFlagCountry, year,
            workingGroup, stock, speciesCode,
            quarter, metier6) %>%
    head(5) %>% as.data.frame()

grp_raised_census %>%
    arrange(vesselFlagCountry, year,
            workingGroup, stock, speciesCode,
            quarter, metier6) %>%
    tail(5) %>% as.data.frame()

## ###########################################################################
## Step 3: Function that wraps step 1, given a set of conditions

grp_catch_raising_condition(census_data = census,
                            estimated_data = catch_estimates,
                            condition_raising_st = quo(stock == "cod.27.21" &
                                                       fleet == "Passive")) %>%
    head() %>% as.data.frame()


## ###########################################################################
## Step 4: wrapper function to loop through a list of conditions



strataCond <- list(G1 = quo(stock == "cod.27.21" &
                            fleet == "Passive"),
                   G2 = quo(stock == "cod.27.21" &
                            fleet == "Active"))

cond_tets <- check_group_conditions(census_data = census,
                                    condition_list = strataCond,
                                    logFile = NULL, append = TRUE)

test <- raising_cond_loop(census_data = census,
                          estimated_data = catch_estimates,
                          condition_raising_st_list = strataCond,
                          logFile = "Log.txt")


test %>%
    group_by(dataType) %>%
    sample_n(2) %>%
    as.data.frame()

table(test$variableType)

raising_cond_loop(census_data = census,
                  estimated_data = catch_estimates,
                  condition_raising_st_list = strataCond,
                  logFile = "Log.txt", assembled_output = FALSE) %>%
    group_by(dataType) %>%
    sample_n(2) %>%
    as.data.frame()


### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
