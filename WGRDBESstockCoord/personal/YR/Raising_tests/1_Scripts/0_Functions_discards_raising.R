#-*- coding: utf-8 -*-

### File: 0_Functions_discards_raising.R
### Time-stamp: <2025-06-13 15:18:11 a23579>
###
### Created: 13/06/2025	15:14:36
### Author: Yves Reecht
###
####################################################################################################
### Description:
### 
### Definition of functions specific to discards raising.
####################################################################################################


##' Discards and BMS raising functions
##'
##' Catch raising for one raising group
##' @title Discards and BMS raising functions
##' @name dis_raising
##' @param raising_st_census Census data frame for the raised group, including data with and without
##'     DIS/BMS estimates.
##' @param matched_data_census Census data used to estimate the discard/BMS ratio. Commonly the same
##'     as raising_st_census (in which case it can be ignored), but possibility to borrow
##'     information from a wider stratum.
##' @param catch_estimates The complete catch estimate data.frame (no subset needed).
##' @param type The type of catch data to estimate. One of "discards" or "BMS".
##' @return A tibble with `raising_st_census`, and appended estimated catches + a dataType field
##'     indicating "census", "estimated" or "raised".
##' @author Yves Reecht
##'
NULL

##' @rdname dis_raising
##'
grp_catch_raising <- function(raising_st_census,
                              matched_data_census,
                              catch_estimates,
                              variableType = "scientificWeight_kg",
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

    estField <- case_when(type == "discards" ~ "domainCatchDis",
                          type == "bms" ~ "domainCatchBMS",
                          TRUE ~ NA)

    estCateg <- case_when(type == "discards" ~ "DIS",
                          type == "bms" ~ "BMS",
                          TRUE ~ NA)

    ## Separating landings with and without provided discards (or BMS):
    land_w_est <- raising_st_census %>%
        filter(! is.na(!!sym(estField)),
               toupper(catchCategory) %in% c("LAN"),
               variableType %in% variableType)

    land_wo_est <- raising_st_census %>%
        filter(is.na(!!sym(estField)),
               toupper(catchCategory) %in% c("LAN"),
               variableType %in% variableType)

    ## set.seed(123)
    ## land_wo_est %>% sample_n(2) %>% as.data.frame()

    ## Data used for estimating the ratio (possibly borrowing info from a wider group):
    data_ratio <- matched_data_census %>%
        filter(toupper(catchCategory) %in% c("LAN")) %>% # landings on one side...
        dplyr::group_by(vesselFlagCountry, year, workingGroup, stock,
                        speciesCode, variableType, !!sym(estField)) %>%
        dplyr::summarize(landings = sum(total, na.rm = TRUE)) %>%
        dplyr::rename("domainCatch" = estField) %>% # make generic for different types.
        ## ...joined to the corresponding estimates:
        left_join(catch_estimates %>%
                  filter(toupper(catchCategory) %in% c(estCateg), # switch if BMS
                         variableType %in% variableType)) # if BMS, should rbind possible census data!

    ## set.seed(123)
    ## data_ratio %>% sample_n(1) %>% as.data.frame()

    ## Estimate the ratio (hard-coded weighting factor for now, but could become a parameter)
    estimated_ratio <- weighted.mean(x = data_ratio$total / data_ratio$landings,
                                     w = data_ratio$landings, na.rm = TRUE)
    ## ## Alternatively, to account for discards with zero landings:
    ## estimated_ratio <- sum(data_ratio$total, na.rm = TRUE) /
    ##     sum(data_ratio$landings, na.rm = TRUE)

    land_catch_raised <- land_wo_est %>%
        dplyr::rename(LAN = total) %>%
        dplyr::mutate(DIS = LAN * estimated_ratio, #Make generic for BMS.
                      catchCategory = NULL) %>%
        tidyr::pivot_longer(any_of(c("LAN", "DIS", "BMS")),
                            names_to = "catchCategory",
                            values_to = "total") %>%
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
##'
grp_catch_raising_condition <- function(census_data, estimated_data,
                                        condition_raising_st,
                                        condition_matched_data = condition_raising_st,
                                        variableType = "scientificWeight_kg",
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

    estField <- case_when(type == "discards" ~ "domainCatchDis",
                          type == "bms" ~ "domainCatchBMS",
                          TRUE ~ NA)

    estCateg <- case_when(type == "discards" ~ "DIS",
                          type == "bms" ~ "BMS",
                          TRUE ~ NA)

    matched_data_cdf <- matched_data_cdf %>%
        ## Add any missing data with the same domainCatchDis key:
        bind_rows(census_data[! gidx, ] %>%
                  ## Should additionnaly match on stock and year as can be duplicates otherwise(?)
                  filter(!!sym(estField) %in%
                         na.omit(dplyr::pull(matched_data_cdf, estField)))) %>% # Make generic for BMS.
        ## Filter out data without discard/BMS/... estimates:
        filter(! is.na(!!sym(estField))) # Make generic for BMS.

    ## Make get raised data (census format + dataType):
    grp_catch_raising(raising_st_census = raising_st_cdf,
                      matched_data_census = matched_data_cdf,
                      catch_estimates = catch_estimates,
                      variableType = variableType,
                      type = type, verbose = verbose)

}



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
                              variableType = "scientificWeight_kg",
                              type = c("discards", "BMS"),
                              verbose = TRUE,
                              assembled_output = TRUE,
                              append = FALSE,
                              ...)
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
    condition_raising_st_list <-
        check_group_conditions(census_data = census_data,
                               condition_list = condition_raising_st_list,
                               conditionType = "strata",
                               dataType = type, variableType = variableType,
                               append = append,
                               ...)

    condition_matched_data_list <-
        check_group_conditions(census_data = census_data,
                               condition_list = condition_matched_data_list,
                               conditionType = "matched_data",
                               dataType = type, variableType = variableType,
                               append = TRUE,
                               ...)

    ## ##################################################
    ## Raising:

    ## Loops through conditions
    res <- mapply(grp_catch_raising_condition,
                  condition_raising_st = condition_raising_st_list,
                  condition_matched_data = condition_matched_data_list,
                  MoreArgs = list(census_data = census_data,
                                  estimated_data = estimated_data,
                                  type = type, variableType = variableType,
                                  verbose = verbose),
                  SIMPLIFY = FALSE)

    res <- bind_rows(res)

    ## ##################################################
    ## Post-processing:

    ## Add census and estimated data, if requested:
    if (isTRUE(assembled_output))
    {
        estim <- census_data %>%
            dplyr::filter(! is.na(domainCatchDis), # Make generic
                          variableType %in% variableType,
                          is.na(total)) %>% ## as.data.frame()
            dplyr::rename(domainCatch = "domainCatchDis") %>%
            dplyr::left_join(estimated_data,
                             by = c("vesselFlagCountry", "year", "workingGroup",
                                    "stock", "speciesCode", "catchCategory",
                                    "variableType",
                                    "domainCatch")) %>% ## + variableType
            mutate(total = total.y,   # Change total = total.y with new format
                   dataType = "estimated") %>%
            dplyr::rename(domainCatchDis = "domainCatch") %>% # Make generic
            select(-any_of(c("total.x", "total.y", ## "total", # remove total with new format
                             "mean", "varianceMean"))) ## %>%
            ## dplyr::rename(domainCatchDis = "domainCatch") # Make generic

        res <- res %>%
            bind_rows(census_data %>%
                      dplyr::filter(! is.na(total),
                                    variableType %in% variableType) %>% # total for new format
                      mutate(dataType = "census")) %>%
            bind_rows(estim)
    }

    return(res)
}






### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
