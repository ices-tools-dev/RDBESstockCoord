#-*- coding: utf-8 -*-

### File: 1_test_discard_raising.R
### Time-stamp: <2024-10-23 14:32:17 a23579>
###
### Created: 23/10/2024	08:49:07
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
library(lubridate)

scriptDir <- "./1_Scripts"
dataDir <- "./2_Data"
resDir <- "./3_Results"

census <- read_csv(file.path(dataDir, "census_data_v9.csv"))

catch_estimates <- read_csv(file.path(dataDir, "estimated_data_v9.csv"))

census %>% head() %>% as.data.frame()

catch_estimates %>% head() %>% as.data.frame()

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
##  * must make sure the all data corresponding to a given domainCatchDis key are included
##    (otherwise the estimatedcorresponding ratio estimate is wrong).
##
## Based on condition 2:

## To be able to check whether all data included, we store a group index
gidx <- census$stock == "cod.27.21" &
    census$fleet == "Passive" |> replace_na(replace = FALSE)

data_grp_cdf <- census[gidx, ]

data_grp_cdf <- data_grp_cdf %>%
    ## Add any missing data with the same domainCatchDis key:
    bind_rows(census[! gidx, ] %>%
              filter(domainCatchDis %in% na.omit(data_grp_cdf$domainCatchDis))) %>%
    ## Filter out data without discard estimates:
    filter(! is.na(domainCatchDis))

##' Catch raising for one raising group
##'
##' .. content for \details{} ..
##' @title
##' @param raising_census Census data frame for the raised group, including data with and without DIS/BMS estimates.
##' @param data_census Census data used to estimate the discard/BMS ratio. Commonly the same as raising_census (in which
##'     case it can be ignored), but possibility to borrow information from a wider stratum.
##' @param catch_estimates The complete catch estimate data.frame (no subset needed).
##' @param type The type of catch data to estimate. One of "discards" or "BMS".
##' @return A tibble with `raising_census`, and appended estimated catches + a dataType field indicating "census" or
##'     "raised".
##' @author Yves Reecht
grp_catch_raising <- function(raising_census,
                              data_census,
                              catch_estimates,
                              type = c("discards", "BMS"),
                              verbose = TRUE)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 23 Oct 2024, 12:23

    type <- match.arg(tolower(type), c("discards", "bms"))

    if (type == "bms") stop("BMS not implemented yet!")
    ## Needs to implement matching key switch +
    ##   figure out how to handle possible BMS census information.

    ## Basic check: must work within one year and stock
    if (nrow(unique(raising_census %>% select(year, stock))) > 1)
    {
        stop("Several years and/or stocks in the raising group")
    }

    if (missing(data_census))
    {
        if (isTRUE(verbose))
        {
            message("matching to \"same\" (data_census = raising_census)")
        }
        data_census <- raising_census
    }

    ## Separating landings with and without provided discards (or BMS when generic):
    land_w_est <- raising_census %>%
        filter(! is.na(domainCatchDis),
               CatchCategory %in% c("LAN"))

    land_wo_est <- raising_census %>%
        filter(is.na(domainCatchDis),
               CatchCategory %in% c("LAN"))

    ## set.seed(123)
    ## land_wo_est %>% sample_n(2) %>% as.data.frame()

    ## Data used for estimating the ratio (possibly borrowing info from a wider grouping):
    data_ratio <- data_census %>%
        filter(CatchCategory %in% c("LAN")) %>% # landings on one side...
        dplyr::group_by(VesselFlagCountry, year, species, stock, domainCatchDis) %>%
        dplyr::summarize(landings = sum(scientificWeight, na.rm = TRUE)) %>%
        dplyr::rename("domainCatch" = "domainCatchDis") %>% # make generic for different types.
        ## ...joined to the corresponding estimates:
        left_join(catch_estimates %>%
                  filter(CatchCategory %in% c("DIS"), # switch if BMS
                         variableType == "WeightLive")) # if BMS, should rbind possible census data!

    ## set.seed(123)
    ## data_ratio %>% sample_n(1) %>% as.data.frame()

    ## Estimate the ratio (hard-coded weighting factor for now, but could become a parameter)
    estimated_ratio <- weighted.mean(x = data_ratio$total / data_ratio$landings,
                                     w = data_ratio$landings)
    ## ## Alternatively, to account for discards with zero landings:
    ## estimated_ratio <- sum(data_ratio$total, na.rm = TRUE) /
    ##     sum(data_ratio$landings, na.rm = TRUE)

    land_catch_raised <- land_wo_est %>%
        dplyr::rename(LAN = scientificWeight) %>%
        dplyr::mutate(DIS = LAN * estimated_ratio, #Make generic for BMS.
                      CatchCategory = NULL) %>%
        tidyr::pivot_longer(any_of(c("LAN", "DIS", "BMS")),
                            names_to = "CatchCategory",
                            values_to = "scientificWeight") %>%
        mutate(dataType = if_else(CatchCategory %in% c("DIS", "BMS"),
                                  "raised", "census")) %>%
        ## Add landings with estimation... not the estimated DIS/BMS,
        ##   as might be used several times or none (possible
        ##   different aggregation levels) =>
        ##   needs to be handled seperately after raising all groups:
        bind_rows(land_w_est %>%
                  mutate(dataType = "census"))

    ## Trick to re-order the fields as original:
    land_catch_raised <- head(land_w_est, 0) %>%
        bind_rows(land_catch_raised)

    return(land_catch_raised)
}

debugonce(grp_catch_raising)

grp_raised_census <- grp_catch_raising(raising_census = raising_grp_cdf,
                                       data_census = data_grp_cdf,
                                       catch_estimates = catch_estimates)


grp_raised_census %>%
    head(4) %>% as.data.frame()

grp_raised_census %>%
    tail(4) %>% as.data.frame()


### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
