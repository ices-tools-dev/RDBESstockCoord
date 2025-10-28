#-*- coding: utf-8 -*-

### File: 0_Functions_discards_raising.R
### Time-stamp: <2025-10-28 16:35:45 a23579>
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
                              sourceType = c("WGValue", "Official"),
                              variableType = c("WeightLive", "Number"),
                              type = c("discards", "BMS"),
                              groupName = NA_character_,
                              verbose = TRUE)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 23 Oct 2024, 12:23

    ## if (groupName %in% c("G10")) browser()

    ## Only for one source and variable type at once:
    sourceType <- match.arg(sourceType,
                            c("WGValue", "Official"),
                            several.ok = FALSE)

    variableType <- match.arg(variableType,
                              c("WeightLive", "Number"),
                              several.ok = FALSE)

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

    ## Domains that need to be set to NA for raised data:
    otherDomains <- c("domainCatchDis", "domainCatchBMS", "domainBiology") %>%
        {.[! . %in% estField]}

    estCateg <- case_when(type == "discards" ~ "DIS",
                          type == "bms" ~ "BMS",
                          TRUE ~ NA)

    raising_st_catch <- raising_st_catch %>%
        bind_rows(tibble(importedOrRaised = character()))
    
    ## Identify matched data with estimates:
    catch_estimates_cat <- matched_data_catch %>%
        filter(catchCategory %in% c(estCateg),
               ! is.na(total),
               sourceType %in% {{sourceType}},
               variableType %in% {{variableType}}, # force evaluating the argument "outside",
                                        # instead of using the fields "variableType".
               ! is.na(!!sym(estField))) %>% 
        dplyr::rename(all_of(c("domainCatch" = estField))) %>%
        dplyr::select(vesselFlagCountry:catchCategory, domainCatch, sourceType, variableType, total)

    ## Identify categories with estimates:
    reported_catch_cat <- raising_st_catch %>%
        filter(catchCategory %in% c(estCateg),
               ! is.na(total),
               sourceType %in% {{sourceType}},
               variableType %in% {{variableType}},
               ! is.na(!!sym(estField))) %>% 
        dplyr::rename(all_of(c("domainCatch" = estField))) %>%
        dplyr::select(vesselFlagCountry:catchCategory, domainCatch, sourceType, variableType, total)

    ## Separating landings with and without provided discards (or BMS)
    ##   (NA in domain is not reliable => using semi and anti_join):
    land_w_est <- raising_st_catch %>%
        filter(toupper(catchCategory) %in% c("LAN"),
               sourceType %in% {{sourceType}},
               variableType %in% {{variableType}}) %>%
        mutate(domainCatch = !!sym(estField)) %>%
        semi_join(reported_catch_cat, 
                  by = c("vesselFlagCountry", "year", "workingGroup", "stock", "speciesCode",
                         "domainCatch", "sourceType", "variableType")) %>%
        mutate(domainCatch = NULL)

    ## Landings without estimates for the category considered: 
    land_wo_est <- raising_st_catch %>%
        filter(toupper(catchCategory) %in% c("LAN"),
               sourceType %in% {{sourceType}},
               variableType %in% {{variableType}}) %>%
        mutate(domainCatch = !!sym(estField)) %>%
        anti_join(reported_catch_cat, 
                  by = c("vesselFlagCountry", "year", "workingGroup", "stock", "speciesCode",
                         "domainCatch", "sourceType", "variableType"))%>%
        mutate(domainCatch = NULL)

    ## All remaining data:
    catch_other <- raising_st_catch %>%
        mutate(domainCatch = !!sym(estField)) %>%
        anti_join(land_wo_est %>%
                  mutate(domainCatch = !!sym(estField)), 
                  by = c("vesselFlagCountry", "year", "workingGroup", "stock", "speciesCode",
                         "domainCatch", "sourceType", "variableType")) %>%
        mutate(domainCatch = NULL,
               importedOrRaised = coalesce(importedOrRaised,
                                          "imported"))

    ## dim(raising_st_catch)
    ## dim(land_wo_est)
    ## dim(catch_other)

    ## Need to add conversion (=> harmonized unit) + unit in the output. [!!!]

    ## Data used for estimating the ratio (possibly borrowing info from a wider group):
    data_ratio <- matched_data_catch %>%
        filter(toupper(catchCategory) %in% c("LAN")) %>% # landings on one side...
        dplyr::group_by(vesselFlagCountry, year, workingGroup, stock,
                        speciesCode, sourceType, variableType, !!sym(estField)) %>%
        dplyr::summarize(landings = sum(total, na.rm = TRUE),
                         .groups = "drop") %>% # weighting based on landing CATON
                                        # (can be made more generic if needed).
        dplyr::rename(all_of(c("domainCatch" = estField))) %>% # make it generic for different types.
        ## ...joined to the corresponding estimates:
        left_join(catch_estimates_cat %>% ## 
                  filter(toupper(catchCategory) %in% c(estCateg), # switch if BMS
                         sourceType %in% {{sourceType}},
                         variableType %in% {{variableType}}),
                  by = join_by(vesselFlagCountry, year, workingGroup,
                               stock, speciesCode, sourceType, variableType, domainCatch)) #

    ## Estimate the ratio (hard-coded weighting factor for now, but could become a parameter)
    estimated_ratio <- weighted.mean(x = data_ratio$total / data_ratio$landings,
                                     w = data_ratio$landings, na.rm = TRUE)
    ## ## Alternatively, to account for discards with zero landings:
    ## estimated_ratio <- sum(data_ratio$total, na.rm = TRUE) /
    ##     sum(data_ratio$landings, na.rm = TRUE)

    raisGrpField <- paste0(sub("^(.).*$", "\\1", estCateg),
                           "rGroup")

    land_catch_raised <- land_wo_est %>%
        dplyr::rename(LAN = total) %>%
        dplyr::mutate(!!estCateg := LAN * estimated_ratio, # Generic for BMS.
                      catchCategory = NULL) %>%
        tidyr::pivot_longer(any_of(c("LAN", "DIS", "BMS")),
                            names_to = "catchCategory",
                            values_to = "total") %>%
        mutate(importedOrRaised = if_else(catchCategory %in% c("DIS", "BMS"),
                                          "raised", "imported"),
               across(all_of(otherDomains),
                      ~ if_else(importedOrRaised == "raised",
                                NA_character_, .x)),
               across(all_of(estField),
                      ~coalesce(.x,
                                paste("[raised]",
                                      seasonValue,
                                      areaValue,
                                      fisheriesManagementUnit,
                                      fleetValue, sep = "_"))),
               !!raisGrpField := groupName)
    
    ## Trick to re-order the fields as original + assembled output:
    land_catch_result <- head(land_w_est, 0) %>%
        bind_rows(land_catch_raised) %>%
        bind_rows(catch_other)

    ## land_catch_result %>% group_by(cc = catchCategory, importedOrRaised) %>% slice_head(n = 1) %>% as.data.frame()

    return(land_catch_result)
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
                                        sourceType = c("WGValue", "Official"),
                                        variableType = c("WeightLive", "Number"),
                                        type = c("discards", "BMS"), verbose = TRUE)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 24 Oct 2024, 11:04
    library(rlang)

    ## Only for one source and variable type at once:
    sourceType <- match.arg(sourceType,
                            c("WGValue", "Official"),
                            several.ok = FALSE)

    variableType <- match.arg(variableType,
                              c("WeightLive", "Number"),
                              several.ok = FALSE)
    
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
        ## Add a foreign key that does not include the catchCategory, but the appropriate domain:
        mutate(key = paste(vesselFlagCountry,
                           year,
                           workingGroup,
                           stock,
                           speciesCode,
                           ## seasonType,
                           ## seasonValue,
                           ## areaType,
                           ## areaValue,
                           ## fisheriesManagementUnit,
                           ## metier6,
                           ## fleetType,
                           ## fleetValue,
                           !!sym(estField),
                           sourceType,
                           variableType))

    matched_data_cdf <- catch_data[gidx, ]

    matched_data_cdf <- matched_data_cdf %>%
        ## Add any missing data with the same domainCatchDis key:
        bind_rows(catch_data[! gidx, ] %>%
                  filter(! is.na(!!sym(estField))) %>% ## Only data linked catch info is relevant.
                  ## Should additionnaly match on stock and year as can be duplicates otherwise(?)...
                  ##   ...now matching the unique key (incl. domain, excl. catchType):
                  filter(key %in%
                         na.omit(dplyr::pull(matched_data_cdf, "key")))) %>% #
        ## Filter out data without discard/BMS/... estimates:
        filter(! is.na(!!sym(estField))) #

    if (is.null(groupName)) groupName <- NA_character_

    ## groupName <<- groupName

    ## Make get raised data (catch format + importedOrRaised):
    grp_catch_raising(raising_st_catch = raising_st_cdf,
                      matched_data_catch = matched_data_cdf,
                      sourceType = sourceType,
                      variableType = variableType,
                      type = type,
                      groupName = groupName,
                      verbose = verbose)##  %>%
        ## mutate(DrGroup = groupName) # should be moved within the upper function [!!!]

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
                              sourceType = c("WGValue", "Official"),
                              variableType = c("WeightLive", "Number"),
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

    ## Only for one source and variable type at once:
    sourceType <- match.arg(sourceType,
                            c("WGValue", "Official"),
                            several.ok = FALSE)

    variableType <- match.arg(variableType,
                              c("WeightLive", "Number"),
                              several.ok = FALSE)

    type <- match.arg(tolower(type), c("discards", "bms"))
    domainLookup <- c(discards = "domainCatchDis",
                      bms = "domainCatchBMS")

    ## Harmonization of units:
    catch_data <- catch_data %>%
        convert_field(valueField = "total",
                      to = c("kg", "1000_pcs"))

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
                               sourceType = sourceType,
                               variableType = variableType,
                               append = append,
                               ...)

    condition_matched_data_list <-
        check_group_conditions(catch_data = catch_data,
                               condition_list = condition_matched_data_list,
                               conditionType = "matched_data",
                               domain = domainLookup[type],
                               sourceType = sourceType,
                               variableType = variableType,
                               append = TRUE,
                               ...)

    ## Fill in group names as needed:
    if (is.null(names(condition_raising_st_list)))
    {
        names(condition_raising_st_list) <-
            paste0("G", seq_len(length.out = length(condition_raising_st_list)))
    }

    if (is.null(names(condition_matched_data_list)))
    {
        names(condition_matched_data_list) <-
            names(condition_raising_st_list)
    }

    ## ##################################################
    ## Raising:

    ## Loops through conditions
    res <- mapply(grp_catch_raising_condition,
                  condition_raising_st = condition_raising_st_list,
                  condition_matched_data = condition_matched_data_list,
                  groupName = names(condition_raising_st_list),
                  MoreArgs = list(catch_data, 
                                  type = type, 
                                  sourceType = sourceType,
                                  variableType = variableType,
                                  verbose = verbose),
                  SIMPLIFY = FALSE)

    res <- bind_rows(res)

    ## head(res, 3) %>% as.data.frame()
    ## table(res$DrGroup, res$importedOrRaised, useNA = "ifany")
    ## table(res$catchCategory, res$variableType)

    ## ##################################################
    ## Post-processing:

    catch_data %>%
        group_by(catchCategory, is.na(domainCatchDis), is.na(total)) %>%
        slice_sample(n = 1) %>% as.data.frame()

    ## Already unified, remove imported and other categories/dataTypes if required:
    if (!isTRUE(assembled_output))
    {
        estCateg <- case_when(type == "discards" ~ "DIS",
                          type == "bms" ~ "BMS",
                          TRUE ~ NA)
        ## Practically not used... should it include landings if it ever is? 
        res <- res %>%
            filter(sourceType %in% {{sourceType}},
                   variableType %in% {{variableType}},
                   importedOrRaised %in% "raised", #  
                   catchCategory %in% c(estCateg))
    }

    return(res)
}






### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
