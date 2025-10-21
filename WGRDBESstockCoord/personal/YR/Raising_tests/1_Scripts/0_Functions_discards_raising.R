#-*- coding: utf-8 -*-

### File: 0_Functions_discards_raising.R
### Time-stamp: <2025-10-21 15:44:41 a23579>
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
##' @param raising_st_catch Catch data frame (unified census and estimates) for the raised group,
##'      including data with and without DIS/BMS estimates.
##' @param matched_data_catch Catch data (unified census and estimates) used to estimate the
##'     discard/BMS ratio. Commonly the same as raising_st_census (in which case it can be ignored),
##'     but possibility to borrow information from a wider stratum.
##' @param type The type of catch data to estimate. One of "discards" or "BMS".
##' @return A tibble with `raising_st_catch`, and appended estimated catches + a dataType field
##'     indicating "census", "estimated" or "raised".
##' @author Yves Reecht
##'
NULL

##' @rdname dis_raising
##'
grp_catch_raising <- function(raising_st_catch,
                              matched_data_catch,
                              variableType = "WGWeight",
                              type = c("discards", "BMS"),
                              verbose = TRUE)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 23 Oct 2024, 12:23

    ## if (groupName %in% c("G10")) browser()

    type <- match.arg(tolower(type), c("discards", "bms"))

    if (type == "bms") stop("BMS not implemented yet!")
    ## Needs to implement matching key switch +
    ##   figure out how to handle possible BMS census information.

    ## Basic check: must work within one year and stock
    if (nrow(unique(raising_st_catch %>% select(year, stock))) > 1)
    {
        stop("Several years and/or stocks in the raising group")
    }

    if (missing(matched_data_catch))
    {
        if (isTRUE(verbose))
        {
            message("matching to \"same\" (matched_data_catch = raising_st_catch)")
        }
        matched_data_catch <- raising_st_catch
    }

    estField <- case_when(type == "discards" ~ "domainCatchDis",
                          type == "bms" ~ "domainCatchBMS",
                          TRUE ~ NA)

    estCateg <- case_when(type == "discards" ~ "DIS",
                          type == "bms" ~ "BMS",
                          TRUE ~ NA)

    variableType2 <- variableType # Synonyms.
    
    ## Identify matched data with estimates:
    catch_estimates_cat <- matched_data_catch %>%
        filter(catchCategory %in% c(estCateg),
               ! is.na(total),
               variableType %in% variableType2,
               ! is.na(!!sym(estField))) %>% # Should there be a filter on variableType?
                                        # Nope, comes later!
        dplyr::rename("domainCatch" = estField) %>%
        dplyr::select(vesselFlagCountry:catchCategory, domainCatch, variableType, total)

    ## Identify categories with estimates:
    reported_catch_cat <- raising_st_catch %>%
        filter(catchCategory %in% c(estCateg),
               ! is.na(total),
               variableType %in% variableType2,
               ! is.na(!!sym(estField))) %>% # Should there be a filter on variableType?
                                        # Nope, comes later!
        dplyr::rename("domainCatch" = estField) %>%
        dplyr::select(vesselFlagCountry:catchCategory, domainCatch, variableType, total)

    ## Separating landings with and without provided discards (or BMS)
    ##   (NA in domain is not reliable => using semi and anti_join):
    land_w_est <- raising_st_catch %>%
        filter(toupper(catchCategory) %in% c("LAN"),
               variableType %in% variableType) %>%
        mutate(domainCatch = !!sym(estField)) %>%
        semi_join(reported_catch_cat, 
                  by = c("vesselFlagCountry", "year", "workingGroup", "stock", "speciesCode",
                         "domainCatch", "variableType")) %>%
        mutate(domainCatch = NULL)


    land_wo_est <- raising_st_catch %>%
        filter(toupper(catchCategory) %in% c("LAN"),
               variableType %in% variableType) %>%
        mutate(domainCatch = !!sym(estField)) %>%
        anti_join(reported_catch_cat, 
                  by = c("vesselFlagCountry", "year", "workingGroup", "stock", "speciesCode",
                         "domainCatch", "variableType"))%>%
        mutate(domainCatch = NULL)

    ## Need to add conversion (=> harmonized unit) + unit in the output. [!!!]

    ## Data used for estimating the ratio (possibly borrowing info from a wider group):
    data_ratio <- matched_data_catch %>%
        filter(toupper(catchCategory) %in% c("LAN")) %>% # landings on one side...
        dplyr::group_by(vesselFlagCountry, year, workingGroup, stock,
                        speciesCode, variableType, !!sym(estField)) %>%
        dplyr::summarize(landings = sum(total, na.rm = TRUE)) %>%
        dplyr::rename("domainCatch" = estField) %>% # make generic for different types.
        ## ...joined to the corresponding estimates:
        left_join(catch_estimates_cat %>% ## 
                  filter(toupper(catchCategory) %in% c(estCateg), # switch if BMS
                         variableType %in% variableType)) #

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
##' @param catch_data The catch data table (unified census and estimates).
##' @param condition_raising_st Condition for the selection of a raising startum, provided as a
##'     quosure (`quo(...)`) or text (later converted to a quosure), as usable by the
##'     `dplyr::filter()` function. The function also accepts a logical index vector that matches
##'     the number of rows in `census_data`.
##' @param condition_matched_data Condition for the selection of matching estimates. Similar format
##'     specification as for `condition_raising_st`.
##'
grp_catch_raising_condition <- function(catch_data, 
                                        condition_raising_st,
                                        condition_matched_data = condition_raising_st,
                                        groupName = NA_character_,
                                        variableType = "WGWeight",
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

    ## if (groupName %in% c("G5", "G6")) debugonce(grp_catch_raising)#browser()

    ## Catch DF for one group in which the same discard ratio will be applied.
    ##   contains bot landings with discards estimates and landings without (see domainCatchDis key).
    ##
    ## Based on condition 1:
    raising_st_cdf <- catch_data %>% 
        filter(!!condition_raising_st)

    ## raising_st_cdf %>% group_by(is.na(domainCatchDis)) %>%
    ##     sample_n(1) %>% as.data.frame()

    ## Catch DF with data used to estimate the discard ratio:
    ##  * most commonly the same as `raising_st_cdf`, but might include extra data.
    ##  * must make sure the all data corresponding to a given domainCatchDis key are included
    ##    (otherwise the corresponding ratio estimate is wrong).
    ##
    ## Based on condition 2:

    estField <- case_when(type == "discards" ~ "domainCatchDis",
                          type == "bms" ~ "domainCatchBMS",
                          TRUE ~ NA)

    estCateg <- case_when(type == "discards" ~ "DIS",
                          type == "bms" ~ "BMS",
                          TRUE ~ NA)

    ## To be able to check whether all data included, we store a logical group index:
    gidx <- seq_len(nrow(catch_data)) %in%
        (catch_data %>%
         mutate(idxTmp = 1:n()) %>%
         filter(!!condition_matched_data) %>%
         pull(idxTmp))

    catch_data <- catch_data %>%
        ## Add a unique key that does not include the catchCategory, but the appropriate domain:
        mutate(key = paste(vesselFlagCountry,
                           year,
                           workingGroup,
                           stock,
                           speciesCode,
                           seasonType,
                           seasonValue,
                           areaType,
                           areaValue,
                           fisheriesManagementUnit,
                           metier6,
                           fleetType,
                           fleetValue,
                           !!sym(estField),
                           variableType))

    matched_data_cdf <- catch_data[gidx, ]

    matched_data_cdf <- matched_data_cdf %>%
        ## Add any missing data with the same domainCatchDis key:
        bind_rows(catch_data[! gidx, ] %>%
                  ## Should additionnaly match on stock and year as can be duplicates otherwise(?)...
                  ##   ...now matching the unique key (incl. domain, excl. catchType):
                  filter(key %in%
                         na.omit(dplyr::pull(matched_data_cdf, "key")))) %>% #
        ## Filter out data without discard/BMS/... estimates:
        filter(! is.na(!!sym(estField))) #

    if (is.null(groupName)) groupName <- NA_character_

    groupName <<- groupName

    ## Make get raised data (catch format + dataType):
    grp_catch_raising(raising_st_catch = raising_st_cdf,
                      matched_data_catch = matched_data_cdf,
                      variableType = variableType,
                      type = type, verbose = verbose) %>%
        mutate(DrGroup = groupName)

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
raising_cond_loop <- function(catch_data, 
                              condition_raising_st_list,
                              condition_matched_data_list = condition_raising_st_list,
                              variableType = "WGWeight",
                              type = c("discards", "BMS"),
                              verbose = TRUE,
                              assembled_output = TRUE,
                              append = FALSE,
                              ...)
{
    ## Purpose: 
    ##   * Tests for validity of conditions.
    ##   * Loops over condition pairs (raised stratum, matched data) and
    ##     apply raising.
    ##   * Formats data (output with or without census and estimated data).
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 24 Oct 2024, 13:55
    library(rlang)
    library(dplyr)

    type <- match.arg(tolower(type), c("discards", "bms"))
    domainLookup <- c(discards = "domainCatchDis",
                      bms = "domainCatchBMS")

    ## browser()

    ## ##################################################
    ## Check consistency:

    ## between condition list length:
    stopifnot(length(condition_raising_st_list) == length(condition_matched_data_list))

    ##
    condition_raising_st_list <-
        check_group_conditions(catch_data = catch_data,
                               condition_list = condition_raising_st_list,
                               conditionType = "strata",
                               domain = domainLookup[type],
                               variableType = variableType,
                               append = append,
                               ...)

    condition_matched_data_list <-
        check_group_conditions(catch_data = catch_data,
                               condition_list = condition_matched_data_list,
                               conditionType = "matched_data",
                               domain = domainLookup[type],
                               variableType = variableType,
                               append = TRUE,
                               ...)

    ## ##################################################
    ## Raising:

    ## Loops through conditions
    res <- mapply(grp_catch_raising_condition,
                  condition_raising_st = condition_raising_st_list,
                  condition_matched_data = condition_matched_data_list,
                  groupName = names(condition_raising_st_list),
                  MoreArgs = list(catch_data, 
                                  type = type, variableType = variableType,
                                  verbose = verbose),
                  SIMPLIFY = FALSE)

    res <- bind_rows(res)
    ## browser()
    ## head(res, 3) %>% as.data.frame()
    ## table(res$DrGroup, res$dataType, useNA = "ifany")

    ## ##################################################
    ## Post-processing:

    ## catch_data %>%
    ##     group_by(catchCategory, is.na(domainCatchDis), is.na(total)) %>%
    ##     slice_sample(n = 1) %>% as.data.frame()

    ## Add (unified) census and estimated data, if requested:
    if (isTRUE(assembled_output))
    {
        res <- res %>%
            bind_rows(catch_data %>%
                      dplyr::filter(! is.na(total),
                                    variableType %in% variableType) %>% # total for new format
                      mutate(dataType = ifelse(toupper(catchCategory) %in% c("LAN", "BMS"),
                                               "reported", "estimated")))
    }

    return(res)
}






### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
