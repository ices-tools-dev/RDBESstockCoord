#-*- coding: utf-8 -*-

### File: fun_conversion_RCEF_v14_to_v14.5.R
### Time-stamp: <2025-09-23 10:44:28 a23579>
###
### Created: 09/09/2025	16:43:35
### Author: Yves Reecht
###
####################################################################################################
### Description:
### 
### So far only for catch table... biology table conversion yet to be developped.
####################################################################################################

##' Conversion of RCEF format from v14 to v14.5
##'
##' @title 
##' @param census_catches_file Path to the census catch file.
##' @param estimated_catches_file Path to the estimated catch file.
##' @param census_catches Data.frame, as an alternalive to census_catches_file.
##' @param estimated_catches Data.frame, as an alternalive to estimated_catches_file.
##' @param output_file Output file name without path (default elaborated from census_catches_file)
##' @param output_dir Path for the output file (default as census_catches_file, "." if missing).
##' @param ... Ignored optional parameters (for convenient use in do.call(...)).
##' @return An assembled table containing census and estimated data, following RCEF v14.5
##' specifications. 
##' @author Yves Reecht
RCEF_catch_convert_v14_to_v14.5 <- function(census_catches_file, estimated_catches_file,
                                            ## Alternatively pass tables as named parameters: 
                                            census_catches, estimated_catches,
                                            ## -------------------------------
                                            output_file = NULL,
                                            output_dir = dirname(census_catches_file),
                                            ...)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date:  9 Sep 2025, 13:57
    library(dplyr)
    library(tidyr)
    library(readr)
    library(rlang)
    
    if (missing(census_catches))
    {
        census_catches <- read_csv(census_catches_file)
    }else{
        census_catches <- as_tibble(census_catches) %>%
            mutate(domainCatchDis = ifelse(domainCatchDis %in% "", NA, domainCatchDis),
                   domainCatchBMS = ifelse(domainCatchBMS %in% "", NA, domainCatchBMS),
                   domainBiology = ifelse(domainBiology %in% "", NA, domainBiology))
    }

    if (missing(estimated_catches))
    {
        estimated_catches <- read_csv(estimated_catches_file)
    }else{
        estimated_catches <- as_tibble(estimated_catches) %>%
            mutate(domainCatch = ifelse(domainCatch %in% "", NA, domainCatch))
    }

    ## Guessing paths:
    if (is.null(output_file) &&
        ! missing(census_catches_file))
    {
        output_file <- sub(".csv", "_v14.5.csv",
            sub("census[_]", "", basename(census_catches_file)))
    }else{}

    if ((missing(output_dir) && missing(census_catches_file)) ||
        is.null(output_dir))
    {
        output_dir <- "."
    }

    if (! is.null(output_file))
    {        
        res_file <- file.path(normalizePath(output_dir), output_file)
    }else{}
    
    ## Conversion fields to type+value, +various data wrangling in census:
    censusExt <- census_catches %>%
        mutate(domainCatch = dplyr::coalesce(domainCatchDis, domainCatchBMS),
               domainCatch = if_else(is.na(domainCatch),
                                     NA_character_,
                                     paste(vesselFlagCountry, domainCatch, sep = "_")),
               seasonType = ifelse(is.na(quarter),
                                   "Year", "Quarter"),
               seasonValue = ifelse(is.na(quarter),
                                    year, quarter),
               quarter = NULL,
               areaType = "ICESArea",
               areaValue = area,
               area = NULL,
               fleetType = "WGFleet",
               fleetValue = fleet,
               metier6 = fleet,
               fleet = NULL,
               variableType = sub("Scientific", "", variableType),
               catchCategory = ifelse(catchCategory %in% "Logbook Registered Discard",
                                      "RegDIS", catchCategory),
               ## Moving domainCatchDis -> domainCatchBMS for BMS only
               ## ([YR]: Obsolete with new conversion patch but kept in case of persisting
               ##  inconsistency):  
               domainCatchBMS = ifelse(catchCategory %in% "BMS" &
                                       is.na(domainCatchBMS) &
                                       ! is.na(domainCatchDis),
                                       domainCatchDis, domainCatchBMS),
               domainCatchDis = ifelse(catchCategory %in% "BMS",
                                       NA, domainCatchDis)
               ) %>%
        select(vesselFlagCountry:catchCategory,
               seasonType, seasonValue,
               areaType, areaValue,
               fisheriesManagementUnit, metier6,
               fleetType, fleetValue,
               domainCatchDis:domainCatch)

    
    ## Catch estimate table processing:
    estimated_catches <- estimated_catches %>%
        mutate(catchCategory = ifelse(catchCategory %in% "Logbook Registered Discard",
                                      "RegDIS", catchCategory))

    ## Split discards and BMS catch estimates:
    estDIS <- estimated_catches %>%
        filter(catchCategory == "DIS") %>%
        mutate(domainCatchDis = domainCatch,
               domainCatch = ifelse(is.na(domainCatch),
                                    NA_character_,
                                    paste(vesselFlagCountry, domainCatchDis, sep = "_")),
               variableType = sub("Scientific", "", variableType))

    estRegDIS <- estimated_catches %>%
        filter(catchCategory == "RegDIS") %>%
        mutate(domainCatchDis = domainCatch,
               domainCatch = ifelse(is.na(domainCatch),
                                    NA_character_,
                                    paste(vesselFlagCountry, domainCatchDis, sep = "_")),
               variableType = sub("Scientific", "", variableType))

    estBMS <- estimated_catches %>%
        filter(catchCategory == "BMS") %>%
        mutate(domainCatchBMS = domainCatch,
               domainCatch = ifelse(is.na(domainCatch),
                                    NA_character_,
                                    paste(vesselFlagCountry, domainCatchBMS, sep = "_")),
               variableType = sub("Scientific", "", variableType))

    ## Valid domain(Dis|BMS) from estimated fraction:
    domainsDIS <- unique(na.omit(estDIS$domainCatch))
    domainsRegDIS <- unique(na.omit(estRegDIS$domainCatch))
    domainsBMS <- unique(na.omit(estBMS$domainCatch))

    ## Correction domain(Dis|BMS) for landings... if necessary:
    censusExt <- censusExt %>%
        mutate(domainCatchBMS = ifelse(catchCategory == "LAN" &
                                       domainCatch %in% domainsBMS,
                                       coalesce(domainCatchBMS, domainCatchDis), # Also works after
                                        # correction patch, when domainCatchBMS is already correctly
                                        # filled. 
                                       domainCatchBMS),
               domainCatchDis = ifelse(catchCategory == "LAN" &
                                       ! domainCatch %in% c(domainsDIS, domainsRegDIS),
                                       NA, domainCatchDis))

    ## Assembling census and estimates in one unique table:
    catches <- censusExt %>%
        filter(catchCategory == "LAN") %>%
        bind_rows(censusExt %>%
                  select(-total) %>%
                  right_join(estDIS)) %>%
        bind_rows(censusExt %>%
                  select(-total) %>%
                  right_join(estRegDIS)) %>%
        bind_rows(censusExt %>%
                  select(-total) %>%
                  right_join(estBMS)) %>%
        dplyr::select(-domainCatch)

    ## Write results, if a filename can be inferred:
    if (! is.null(output_file))
    {
        catches %>%
            write_csv(file = res_file,
                      na = "")
    }

    return(invisible(catches))
}








### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
