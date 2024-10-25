#-*- coding: utf-8 -*-

### File: 1_test_discard_raising.R
### Time-stamp: <2024-10-24 15:55:57 a23579>
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
library(rlang)
## library(lubridate)

scriptDir <- "./1_Scripts"
dataDir <- "./2_Data"
resDir <- "./3_Results"

census <- read_csv(file.path(dataDir, "census_data_v10.csv"))

catch_estimates <- read_csv(file.path(dataDir, "estimated_data_v10.csv"))

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
              ## Should additionnaly match on stock and year as can be duplicates otherwise.
              filter(domainCatchDis %in% na.omit(data_grp_cdf$domainCatchDis))) %>%
    ## Filter out data without discard estimates:
    filter(! is.na(domainCatchDis))


## ###########################################################################
## Step 2: raising...

##' Discards and BMS raising functions
##'
##' Catch raising for one raising group
##' @title Discards and BMS raising functions
##' @name dis_raising
##' @param raising_st_census Census data frame for the raised group, including data with and without DIS/BMS estimates.
##' @param matched_data_census Census data used to estimate the discard/BMS ratio. Commonly the same as raising_st_census (in which
##'     case it can be ignored), but possibility to borrow information from a wider stratum.
##' @param catch_estimates The complete catch estimate data.frame (no subset needed).
##' @param type The type of catch data to estimate. One of "discards" or "BMS".
##' @return A tibble with `raising_st_census`, and appended estimated catches + a dataType field indicating "census" or
##'     "raised".
##' @author Yves Reecht
grp_catch_raising <- function(raising_st_census,
                              matched_data_census,
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
    if (nrow(unique(raising_st_census %>% select(year, stock))) > 1)
    {
        stop("Several years and/or stocks in the raising group")
    }

    if (missing(matched_data_census))
    {
        if (isTRUE(verbose))
        {
            message("matching to \"same\" (matched_data_census = raising_st_census)")
        }
        matched_data_census <- raising_st_census
    }

    ## Separating landings with and without provided discards (or BMS when generic):
    land_w_est <- raising_st_census %>%
        filter(! is.na(domainCatchDis),
               catchCategory %in% c("LAN"))

    land_wo_est <- raising_st_census %>%
        filter(is.na(domainCatchDis),
               catchCategory %in% c("LAN"))

    ## set.seed(123)
    ## land_wo_est %>% sample_n(2) %>% as.data.frame()

    ## Data used for estimating the ratio (possibly borrowing info from a wider group):
    data_ratio <- matched_data_census %>%
        filter(catchCategory %in% c("LAN")) %>% # landings on one side...
        dplyr::group_by(vesselFlagCountry, year, workingGroup, stock, speciesCode, domainCatchDis) %>%
        dplyr::summarize(landings = sum(scientificWeight_kg, na.rm = TRUE)) %>%
        dplyr::rename("domainCatch" = "domainCatchDis") %>% # make generic for different types.
        ## ...joined to the corresponding estimates:
        left_join(catch_estimates %>%
                  filter(catchCategory %in% c("DIS"), # switch if BMS
                         variableType == "scientificWeight_kg")) # if BMS, should rbind possible census data!

    ## set.seed(123)
    ## data_ratio %>% sample_n(1) %>% as.data.frame()

    ## Estimate the ratio (hard-coded weighting factor for now, but could become a parameter)
    estimated_ratio <- weighted.mean(x = data_ratio$total / data_ratio$landings,
                                     w = data_ratio$landings, na.rm = TRUE)
    ## ## Alternatively, to account for discards with zero landings:
    ## estimated_ratio <- sum(data_ratio$total, na.rm = TRUE) /
    ##     sum(data_ratio$landings, na.rm = TRUE)

    land_catch_raised <- land_wo_est %>%
        dplyr::rename(LAN = scientificWeight_kg) %>%
        dplyr::mutate(DIS = LAN * estimated_ratio, #Make generic for BMS.
                      catchCategory = NULL) %>%
        tidyr::pivot_longer(any_of(c("LAN", "DIS", "BMS")),
                            names_to = "catchCategory",
                            values_to = "scientificWeight_kg") %>%
        mutate(dataType = if_else(catchCategory %in% c("DIS", "BMS"),
                                  "raised", "census")) ## %>%
        ## ## Add landings with estimation... not the estimated DIS/BMS,
        ## ##   as might be used several times or none (possible
        ## ##   different aggregation levels) =>
        ## ##   needs to be handled seperately after raising all groups:
        ## bind_rows(land_w_est %>%
        ##           mutate(dataType = "census"))

    ## Trick to re-order the fields as original:
    land_catch_raised <- head(land_w_est, 0) %>%
        bind_rows(land_catch_raised) %>%
        filter(dataType == "raised") # Only return raised data, to avoid duplicates if several raisings.

    return(land_catch_raised)
}

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

##'
##' @rdname dis_raising
##' @param census_data The census data table.
##' @param estimated_data The estimated data table.
##' @param condition_raising_st Condition for the selection of a raising startum, provided as a
##'     quosure (`quo(...)`) or text (later converted to a quosure), as usable by the
##'     `dplyr::filter()` function. The function also accepts a logical index vector that matches
##'     the number of rows in `census_data`.
##' @param condition_matched_data Condition for the selection of matching estimates. Similar format
##'     specification as for `condition_raising_st`.
##' @author Yves Reecht
grp_catch_raising_condition <- function(census_data, estimated_data,
                                        condition_raising_st,
                                        condition_matched_data = condition_raising_st,
                                        type = c("discards", "BMS"), verbose = TRUE)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 24 Oct 2024, 11:04
    library(rlang)

    type <- match.arg(tolower(type), c("discards", "bms"))

    if (is.character(condition_raising_st))
    {
        condition_raising_st <- eval(parse_expr(paste0("quo(", condition_raising_st, ")")))
    }

    if (is.character(condition_matched_data))
    {
        condition_matched_data <- eval(parse_expr(paste0("quo(", condition_matched_data, ")")))
    }

    ## Census DF for one group in which the same discard ratio will be applied.
    ##   contains bot landings with discards estimates and landings without (see domainCatchDis key).
    ##
    ## Based on condition 1:
    raising_st_cdf <- census_data %>%
        filter(!!condition_raising_st)

    ## raising_st_cdf %>% group_by(is.na(domainCatchDis)) %>%
    ##     sample_n(1) %>% as.data.frame()

    ## Census DF with data used to estimate the discard ratio:
    ##  * most commonly the same as `estimation_grp_cdf`, but might include extra data.
    ##  * must make sure the all data corresponding to a given domainCatchDis key are included
    ##    (otherwise the estimatedcorresponding ratio estimate is wrong).
    ##
    ## Based on condition 2:

    ## To be able to check whether all data included, we store a logical group index:
    gidx <- seq_len(nrow(census_data)) %in%
        (census_data %>%
         mutate(idxTmp = 1:n()) %>%
         filter(!!condition_matched_data) %>%
         pull(idxTmp))

    matched_data_cdf <- census_data[gidx, ]

    matched_data_cdf <- matched_data_cdf %>%
        ## Add any missing data with the same domainCatchDis key:
        bind_rows(census_data[! gidx, ] %>%
                  ## Should additionnaly match on stock and year as can be duplicates otherwise(?)
                  filter(domainCatchDis %in%
                         na.omit(matched_data_cdf$domainCatchDis))) %>% # Make generic for BMS.
        ## Filter out data without discard/BMS/... estimates:
        filter(! is.na(domainCatchDis)) # Make generic for BMS.

    ## Make get raised data (census format + dataType):
    grp_catch_raising(raising_st_census = raising_st_cdf,
                      matched_data_census = matched_data_cdf,
                      catch_estimates = catch_estimates,
                      type = type, verbose = verbose)

}

grp_catch_raising_condition(census_data = census,
                            estimated_data = catch_estimates,
                            condition_raising_st = quo(stock == "cod.27.21" &
                                                       fleet == "Passive")) %>%
    head() %>% as.data.frame()


## ###########################################################################
## Step 4: wrapper function to loop through a list of conditions


##'
##' @rdname dis_raising
##' @param condition_raising_st_list List of conditions, as described in `condition_raising_st`. See
##'     details for further explanations.
##' @param condition_matched_data_list List of conditions, as described in
##'     `condition_matched_data`. Must have the same length as `condition_raising_st_list`
##' @param assembled_output Logical; whether to assemble raised, census and estimated data. Only
##'     raised data are returned otherwise.
##'
raising_cond_loop <- function(census_data, estimated_data,
                              condition_raising_st_list,
                              condition_matched_data_list = condition_raising_st_list,
                              type = c("discards", "BMS"), verbose = TRUE,
                              assembled_output = TRUE)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 24 Oct 2024, 13:55
    library(rlang)
    library(dplyr)

    type <- match.arg(tolower(type), c("discards", "bms"))


    ## browser()

    ## ##################################################
    ## Check consistency:

    ## between condition list length:
    stopifnot(length(condition_raising_st_list) == length(condition_matched_data_list))

    ##

    ## Loops through conditions
    res <- mapply(grp_catch_raising_condition,
                  condition_raising_st = condition_raising_st_list,
                  condition_matched_data = condition_matched_data_list,
                  MoreArgs = list(census_data = census_data,
                                  estimated_data = estimated_data,
                                  type = type,
                                  verbose = verbose),
                  SIMPLIFY = FALSE)

    res <- bind_rows(res)

    ##
    if (isTRUE(assembled_output))
    {
        estim <- census_data %>%
            dplyr::filter(! is.na(domainCatchDis),
                   is.na(scientificWeight_kg)) %>% ## as.data.frame()
            dplyr::rename(domainCatch = "domainCatchDis") %>%
            dplyr::left_join(estimated_data,
                             by = c("vesselFlagCountry", "year", "workingGroup",
                                    "stock", "speciesCode", "catchCategory",
                                    "domainCatch")) %>% ## + variableType
            mutate(scientificWeight_kg = total,   # Change total = total.y with new format
                   dataType = "estimated") %>%
            select(-any_of(c("total.x", "total.y", "total", # remove total with new format
                             "mean", "varianceMean")))

        res <- res %>%
            bind_rows(census_data %>%
                      dplyr::filter(! is.na(scientificWeight_kg)) %>% # total for new format
                      mutate(dataType = "census")) %>%
            bind_rows(estim)
    }

    return(res)
}

strataCond <- list(G1 = quo(stock == "cod.27.21" &
                            fleet == "Passive"),
                   G2 = quo(stock == "cod.27.21" &
                            fleet == "Active"))

test <- raising_cond_loop(census_data = census,
                          estimated_data = catch_estimates,
                          condition_raising_st_list = strataCond)


test %>%
    group_by(dataType) %>%
    sample_n(2) %>%
    as.data.frame()

### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
