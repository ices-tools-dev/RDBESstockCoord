#-*- coding: utf-8 -*-

### File: 0_Functions_age-length_alloc.R
### Time-stamp: <2025-10-27 16:57:30 a23579>
###
### Created: 21/10/2025	16:54:17
### Author: Yves Reecht
###
####################################################################################################
### Description:
### 
### 
####################################################################################################

grp_AoL_alloc_N <- function(alloc_st_catch,
                            matched_data_catch,
                            distribution_data,
                            groupName = NA_character_,
                            ## variableType = "WGWeight", # Probably not necessary as already filtered
                            ## weighting = c("NumberAtAoL"), # weighting for mean weights at age|length.
                            bvType = c("Age", "Length"),
                            verbose = TRUE)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 22 Oct 2025, 14:58

    bvType <- match.arg(bvType, c("Age", "Length"), several.ok = FALSE)

    ## Basic check: must work within one year and stock
    if (nrow(unique(alloc_st_catch %>% select(year, stock))) > 1)
    {
        stop("Several years and/or stocks in the alloc group")
    }

    if (missing(matched_data_catch))
    {
        if (isTRUE(verbose))
        {
            message("matching to \"same\" (matched_data_catch = alloc_st_catch)")
        }
        matched_data_catch <- alloc_st_catch
    }

    ## dim(distribution_data)
    ## distribution_data %>% group_by(variableType) %>% slice(2) %>% as.data.frame()
    ## matched_data_catch %>% group_by() %>% slice(1) %>% as.data.frame()

    ## In case distribution_data is provided with sampledOrEstimated, this is kept; added otherwise:
    distribution_data <- distribution_data %>%
        bind_rows(tibble(sampledOrEstimated = character())) # Adds the column if missing (to
                                        # allow coalesce(...)).

    ## Merged catch data with sampled catch-at-age|length:
    catch_samp_N <- matched_data_catch %>%
        filter(! is.na(domainBiology)) %>%
        semi_join(distribution_data %>%
                  filter(bvType %in% {{bvType}},
                         variableType %in% "Number"),
                  by = c("vesselFlagCountry", "year", "workingGroup", "stock", "speciesCode",
                         "catchCategory", "domainBiology")) %>%
        mutate(CATON = sum(total, na.rm = TRUE)) %>% # After semi_join to ensure correct estimate.
        inner_join(distribution_data %>%
                   filter(bvType %in% {{bvType}},
                          variableType %in% "Number"),
                  by = c("vesselFlagCountry", "year", "workingGroup", "stock", "speciesCode",
                         "catchCategory", "domainBiology"),
                  suffix = c(".c", ""))

    ## ## In the unlikely even where there are NA-catch weights, the data should not be used for
    ## ## raising the catch numbers (but kept if provided numbers):
    ## catch_samp_N_unused <- catch_samp_N %>%
    ##     filter(is.na(total)) ## actually not necessary.

    catch_samp_N <- catch_samp_N %>%
        filter( ! is.na(total))

    if (nrow(catch_samp_N) == 0)
    {
        if (verbose)
            message("\n## No distribution info for raising N-at-", bvType,
                    " in group \"", groupName, "\"")
        
        return(NULL)
    }

    ## dim(catch_samp_N)
    ## catch_samp_N %>% group_by(variableType) %>% slice_head(n=2) %>% as.data.frame()
    
    
    ## table(distribution_data$bvType)
    ## bvType <- bvType2
    ## bvType <- "bla"

    ## Catch data with sampled catch-at-age|length:
    catch_w_est_N_samp <- alloc_st_catch %>%
        inner_join(distribution_data %>%
                   filter(bvType %in% {{bvType}},
                          variableType %in% "Number"),
                   by = c("vesselFlagCountry", "year", "workingGroup", "stock", "speciesCode",
                          "catchCategory", "domainBiology"),
                   suffix = c(".c", ""))
        ## semi_join(distribution_data %>%
        ##           filter(bvType %in% {{bvType}},
        ##                  variableType %in% "Number") %>%
        ##           select(all_of(c("vesselFlagCountry", "year",
        ##                           "workingGroup", "stock", "speciesCode",
        ##                           "catchCategory", "domainBiology"))),
        ##           by = c("vesselFlagCountry", "year",
        ##                  "workingGroup", "stock", "speciesCode",
        ##                  "catchCategory", "domainBiology"))

    ## Catch data without sampled catch-at-age|length:
    catch_wo_est_N <- alloc_st_catch %>%
        anti_join(distribution_data %>%
                  filter(bvType %in% {{bvType}},
                         variableType %in% "Number") %>%
                  select(all_of(c("vesselFlagCountry", "year",
                                  "workingGroup", "stock", "speciesCode",
                                  "catchCategory", "domainBiology"))),
                  by = c("vesselFlagCountry", "year",
                         "workingGroup", "stock", "speciesCode",
                         "catchCategory", "domainBiology"))

    any(!is.na(catch_wo_est_N$domainBiology)) # Tests
    any(is.na(catch_w_est_N_samp$domainBiology))
    any(is.na(catch_w_est_N_samp$total))
    
    ## dim(alloc_st_catch)
    ## dim(catch_wo_est_N)
    ## dim(catch_w_est_N_samp)

    ## Aggregated sampled-numbers at age|length:
    aggr_N_samp <- catch_samp_N %>%
        dplyr::group_by(bvType, bvUnit, bvValue,
                        variableType, variableUnit,
                        attributeType, attibuteValue,
                        valueType) %>%
        dplyr::summarize(CATON.samp = mean(CATON, na.rm = TRUE), # Already aggregated for sampled catch
                         value.samp = sum(value, na.rm = TRUE),
                         across(all_of(c("PSUtype")), # Not sure how those should be handled.
                                ~paste(unique(.x, collapse = "+"))),
                         ageGroupPlus = any(ageGroupPlus), # Not sure how those should be handled.
                         across(all_of(c("numPSUs", "numTrips",
                                         "numMeasurements")),
                                ~ NA),
                         .groups = "drop") 

    ## aggr_N_samp %>% as.data.frame() %>% head()

    ## catch_wo_est_N %>% slice_head(n = 2) %>% as.data.frame()

    ## Estimated catch numbers at age|length, collated with unsampled catch:
    catch_bio_est_N <- catch_wo_est_N %>%
        mutate(rowk = 1:n()) %>%
        group_by(rowk) %>%
        group_modify(function(cdata, key, aggr_N_samp)
        {
            ## browser()
            cdata %>%
                rename_with(~paste0(.x, ".c"),
                            all_of(c("variableType", "variableUnit", "numTrips", "numPSUs", "PSUtype"))) %>%
                bind_cols(aggr_N_samp) %>%
                mutate(value = value.samp * total / CATON.samp,
                       sampledOrEstimated = "estimated") 
                ## head(2) %>% as.data.frame()
        }, aggr_N_samp = aggr_N_samp) %>%
        bind_rows() %>%
        ungroup()

    ## Combined N-distributions:
    distribution_N <- head(distribution_data, 0) %>%
        ## Sampled N-ditributions:
        bind_rows(catch_w_est_N_samp %>%
                  dplyr::select(all_of(colnames(distribution_data))) %>%                  
                  mutate(sampledOrEstimated =  coalesce(sampledOrEstimated,
                                                        "sampled"))) %>% # do not override if
                                                          # already provided.
        ## collated with estimated N-distributions
        bind_rows(catch_bio_est_N %>%
                  ## new domainBiology for estimated distributions (with a prefix):
                  mutate(domainBiology =
                             coalesce(domainBiology,
                                      paste("[alloc]",
                                            seasonValue,
                                            areaValue,
                                            fisheriesManagementUnit,
                                            fleetValue,
                                            catchCategory, sep = "_"))) %>%
                  dplyr::select(c(any_of(colnames(distribution_data)),
                                  "sampledOrEstimated")) %>%
                  mutate(allocGroup = groupName))

    ## distribution_N %>%
    ##     group_by(sampledOrEstimated) %>%
    ##     slice_head(n = 2) %>% as.data.frame()

    return(distribution_N)

    ## catch_bio_est_N %>% group_by(!is.na(domainBiology)) %>% slice_head(n = 2) %>% as.data.frame()
}

grp_AoL_alloc_WL <- function(alloc_st_catch,
                             matched_data_catch,
                             distribution_data,
                             distribution_data_N = distribution_data, # in case there may be
                                        # some estimated N with provided W or L. 
                             groupName = NA_character_,
                             variableType_mean = c("WeightLive"), # Here for the averaged
                                        # quantity. Only mean weight for now.
                             weighting = c("NumberAtAoL", "CATON"), # weighting for mean weights at age|length.
                             bvType = c("Age", "Length"),
                             verbose = TRUE)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 22 Oct 2025, 14:58
    ## ###########################################################################
    ## Weights-at-age or -length:

    bvType <- match.arg(bvType, c("Age", "Length"), several.ok = FALSE)
    weighting <- match.arg(weighting,
                           c("NumberAtAoL", "CATON"), several.ok = FALSE)

    ## table(distribution_data$variableType)

    ## In case distribution_data is provided with sampledOrEstimated, this is kept; added otherwise:
    distribution_data <- distribution_data %>%
        bind_rows(tibble(sampledOrEstimated = character())) # Adds the column if missing (to
                                        # allow coalesce(...)).
    distribution_data_N <- distribution_data_N %>%
        bind_rows(tibble(sampledOrEstimated = character())) # Adds the column if missing (to
                                        # allow coalesce(...)).

    ## 
    catch_samp_W <- matched_data_catch %>%
        filter(! is.na(domainBiology)) %>%
        ## Add sum of catch weight for the sampled fraction:
        semi_join(distribution_data %>%
                  filter(bvType %in% {{bvType}},
                         variableType %in% "WeightLive"),
                  by = c("vesselFlagCountry", "year", "workingGroup", "stock", "speciesCode",
                         "catchCategory", "domainBiology")) %>%
        mutate(CATON = sum(total, na.rm = TRUE)) %>% # After semi_join (~filtering) to ensure
                                        # correct estimate. 
        ## Linked to sample-based mean weights at age|length:
        inner_join(distribution_data %>%
                   filter(bvType %in% {{bvType}},
                          variableType %in% variableType_mean),
                   by = c("vesselFlagCountry", "year", "workingGroup", "stock", "speciesCode",
                          "catchCategory", "domainBiology"),
                   suffix = c(".c", "")) %>%
        left_join(distribution_data_N  %>%
                  filter(bvType %in% {{bvType}},
                         variableType %in% "Number") %>% 
                  select(all_of(c("vesselFlagCountry", "year", "workingGroup", "stock", "speciesCode",
                                  "catchCategory", "domainBiology",
                                  "bvType", "bvUnit", "bvValue", "ageGroupPlus",
                                  "attributeType", "attibuteValue",
                                  "value"))) %>%
                  rename(NaAoL = value),
                  by = c("vesselFlagCountry", "year", "workingGroup", "stock", "speciesCode",
                         "catchCategory", "domainBiology",
                         "bvType", "bvUnit", "bvValue", "ageGroupPlus",
                         "attributeType", "attibuteValue"))

    ## In case of CATON weighting... Nope, not needed because of na.rm = TRUE
    ## if (weighting == "CATON")
    ## {
    ##     catch_samp_W_unused <- catch_samp_W %>%
    ##         filter(is.na(total))

    ##     catch_samp_W <- catch_samp_W %>%
    ##         filter( ! is.na(total))
    ## }else{
    ##     catch_samp_W_unused <- catch_samp_W %>%
    ##         head(0)
    ## }

    ## catch_samp_W %>%
    ##     head(2) %>% as.data.frame()

    ## dim(catch_samp_W)
    ## catch_samp_W %>% group_by(variableType) %>% slice_head(n=2) %>% as.data.frame()

    catch_w_est_W_samp <- alloc_st_catch %>%
        ## Linked to sample-based mean weights at age|length:
        inner_join(distribution_data %>%
                   filter(bvType %in% {{bvType}},
                          variableType %in% variableType_mean),
                   by = c("vesselFlagCountry", "year", "workingGroup", "stock", "speciesCode",
                          "catchCategory", "domainBiology"),
                   suffix = c(".c", ""))
        ## semi_join(distribution_data %>%
        ##           filter(bvType %in% {{bvType}},
        ##                  variableType %in% "WeightLive") %>%
        ##           select(all_of(c("vesselFlagCountry", "year",
        ##                           "workingGroup", "stock", "speciesCode",
        ##                           "catchCategory", "domainBiology"))),
        ##           by = c("vesselFlagCountry", "year",
        ##                  "workingGroup", "stock", "speciesCode",
        ##                  "catchCategory", "domainBiology"))

    catch_wo_est_W <- alloc_st_catch %>%
        anti_join(distribution_data %>%
                  filter(bvType %in% {{bvType}},
                         variableType %in% "WeightLive") %>%
                  select(all_of(c("vesselFlagCountry", "year",
                                  "workingGroup", "stock", "speciesCode",
                                  "catchCategory", "domainBiology"))),
                  by = c("vesselFlagCountry", "year",
                         "workingGroup", "stock", "speciesCode",
                         "catchCategory", "domainBiology"))

    any(!is.na(catch_wo_est_W$domainBiology)) # Tests
    any(is.na(catch_w_est_W_samp$domainBiology))
    
    ## dim(alloc_st_catch)
    ## dim(catch_wo_est_W)
    ## dim(catch_w_est_W_samp)

    ## Field used for weighting, based on user choice:
    wgField <- case_when(weighting == "NumberAtAoL" ~ "NaAoL",
                         weighting == "CATON" ~ "total",
                         TRUE ~ NA)
    if (is.na(wgField))
        stop("mean weighting \"", weighting,
             "\" not implemented for weights-at-age|length")

    ## Aggregation of mean W or L over per Age/Length class + Attributes:
    aggr_W_samp <- catch_samp_W %>%
        dplyr::group_by(bvType, bvUnit, bvValue,
                        variableType, variableUnit,
                        attributeType, attibuteValue,
                        valueType) %>%
        dplyr::summarize(CATON.samp = mean(CATON, na.rm = TRUE), # Already aggregated for sampled
                                        # catch
                         N.samp = sum(NaAoL, na.rm = TRUE),
                         ## The main estimate... weighted mean weight or length at age/size class:
                         value.samp = weighted.mean(x = value,
                                                    w = !!sym(wgField),
                                                    na.rm = TRUE),
                         across(all_of(c("PSUtype")), # Not sure how those should be handled.
                                ~paste(unique(.x, collapse = "+"))),
                         ageGroupPlus = any(ageGroupPlus), # Not sure how those should be handled.
                         across(all_of(c("numPSUs", "numTrips",
                                         "numMeasurements")),
                                ~ NA),
                         .groups = "drop")

    ## aggr_W_samp %>% head(5) %>% as.data.frame()

    ## catch_wo_est_W %>% slice_head(n = 2) %>% as.data.frame()

    ## Estimated catch numbers at age|length, collated with unsampled catch:
    catch_bio_est_W <- catch_wo_est_W %>%
        mutate(rowk = 1:n()) %>%
        group_by(rowk) %>%
        group_modify(function(cdata, key, aggr_W_samp)
        {
            ## browser()
            cdata %>%
                rename_with(~paste0(.x, ".c"),
                            all_of(c("variableType", "variableUnit", "numTrips", "numPSUs", "PSUtype"))) %>%
                bind_cols(aggr_W_samp) %>%
                mutate(value = value.samp, # just repeating the estimated means for all catches
                                        # without samples.
                       sampledOrEstimated = "estimated") 
        }, aggr_W_samp = aggr_W_samp) %>%
        bind_rows() %>%
        ungroup()

    ## Combined mean W/L-at-age/length distributions:
    distribution_W <-  head(distribution_data, 0) %>%
        ## Sampled N-ditributions:
        bind_rows(catch_w_est_W_samp %>%
                  dplyr::select(all_of(colnames(distribution_data))) %>%
                  mutate(sampledOrEstimated =  coalesce(sampledOrEstimated,
                                                        "sampled"))) %>% # do not override if
                                        # already provided.
        ## collated with estimated W(or L)-distributions:
        bind_rows(catch_bio_est_W %>%
                  ## new domainBiology for estimated distributions (with a prefix):
                  mutate(domainBiology =
                             coalesce(domainBiology,
                                      paste("[alloc]",
                                            seasonValue,
                                            areaValue,
                                            fisheriesManagementUnit,
                                            fleetValue,
                                            catchCategory, sep = "_"))) %>%
                  dplyr::select(c(any_of(colnames(distribution_data)),
                                  "sampledOrEstimated")) %>%
                  mutate(allocGroup = groupName))

    ## distribution_W %>%
    ##     group_by(sampledOrEstimated) %>%
    ##     slice_head(n = 2) %>% as.data.frame()

    return(distribution_W)
}


grp_AoL_alloc_condition <- function(catch_data, 
                                    distribution_data,
                                    condition_alloc_st,
                                    condition_matched_data = condition_alloc_st,
                                    groupName = NA_character_,
                                    variableType = "WGWeight",
                                    variableType_mean = c("WeightLive"), # For the averaged
                                        # quantity. Only mean weight for now; complete for mean
                                        # length when defined.
                                    weighting = c("NumberAtAoL", "CATON"), # weighting for mean weights at age|length.
                                    bvType = c("Age", "Length"),
                                    verbose = TRUE)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 22 Oct 2025, 09:47
    library(rlang)
    
    variableType <- match.arg(variableType,
                              c("WGWeight", "OfficialWeight", "WGNumber", "OfficialNumber"),
                              several.ok = FALSE) # Or should it allow raising for WG and Offocial together?
    bvType <- match.arg(bvType, c("Age", "Length"), several.ok = TRUE)
    variableType_mean <- match.arg(variableType_mean,
                                   c("WeightLive"), several.ok = TRUE)

    if (is.character(condition_alloc_st))
    {
        condition_alloc_st <- eval(parse_expr(paste0("quo(", condition_alloc_st, ")")))
    }

    if (is.character(condition_matched_data))
    {
        condition_matched_data <- eval(parse_expr(paste0("quo(", condition_matched_data, ")")))
    }

    ## browser()

    ## Catch DF for one group in which the same age/length structure will be applied.
    ##   contains contains both catches with and without estimates (see domainBiology key).
    ##
    ## Based on condition 1:
    alloc_st_cdf <- catch_data %>% 
        filter(!!condition_alloc_st,
               variableType %in% {{variableType}}) # Limited to data of one variable type.
                                        # {{}} necessary to force evaluating from the argument.

    ## table(catch_data$variableType)
    ## table(alloc_st_cdf$variableType)

    ## Catch DF with data used to estimate the age/length structure:
    ##  * most commonly the same as `raising_st_cdf`, but might include extra data.
    ##  * must make sure that all data corresponding to a given domainBiology key are included
    ##    (otherwise the corresponding ratio estimates - N/CATON, in particular - are wrong).
    ##
    ## Based on condition 2:

    ## To be able to check whether all data included, we store a logical group index:
    gidx <- seq_len(nrow(catch_data)) %in%
        (catch_data %>%
         mutate(idxTmp = 1:n()) %>%
         filter(!!condition_matched_data) %>%
         pull(idxTmp))

    ## head(na.omit(catch_data$domainBiology))

    ## catch_data %>%
    ##     select(vesselFlagCountry, year, workingGroup, stock, speciesCode,
    ##            domainBiology, variableType, catchCategory) %>%
    ##     filter(catch_data %>%
    ##            filter(! is.na(domainBiology))
    ##            select(vesselFlagCountry, year, workingGroup, stock, speciesCode,
    ##                   domainBiology, variableType, catchCategory) %>%
    ##            duplicated()) %>%
    ##     slice(1) %>%
    ##     left_join(catch_data) %>%
    ##     as.data.frame()

    ## catch_data %>%
    ##     filter(! is.na(domainBiology)) %>%
    ##     select(vesselFlagCountry, year, workingGroup, stock, speciesCode,
    ##            domainBiology, variableType##, catchCategory
    ##            ) %>%
    ##     duplicated() %>% sum()

    catch_data <- catch_data %>%
        ## Add a foreign key that includes the appropriate domain
        ## + catch category and variable type: 
        mutate(key = paste(vesselFlagCountry,
                           year,
                           workingGroup,
                           stock,
                           speciesCode,
                           catchCategory, # important to avoid duplication.
                           domainBiology, # domaine
                           variableType))

    catch_data %>% group_by(cc = catchCategory, !is.na(domainBiology)) %>% slice(1) %>% as.data.frame()

    matched_data_cdf <- catch_data[gidx, ]

    matched_data_cdf <- matched_data_cdf %>%
        ## Add any missing data with the same domainCatchDis key:
        bind_rows(catch_data[! gidx, ] %>%
                  filter(! is.na(domainBiology)) %>% # Only data with biological info is relevant.
                  ## Should additionnaly match on stock and year as can be duplicates otherwise(?)...
                  ##   ...now matching the foreign key (incl. domain and catchCategory):
                  filter(key %in%
                         na.omit(dplyr::pull(matched_data_cdf, "key")))) %>% #
        ## Filter out data without discard/BMS/... estimates:
        filter(! is.na(domainBiology)) # Only data with biological info is relevant.

    if (is.null(groupName)) groupName <- NA_character_

    resN <- sapply(bvType,
                  function(bvType.i)
           {
               grp_AoL_alloc_N(alloc_st_catch = alloc_st_cdf,
                               matched_data_catch = matched_data_cdf,
                               distribution_data = distribution_data,
                               groupName = groupName,
                               bvType = bvType.i,
                               verbose = verbose)
           }, simplify = FALSE) %>%
        bind_rows()

    ## colnames(resN)

    ## resN %>% group_by(allocGroup) %>%
    ##     slice_sample(n = 1) %>% as.data.frame()

    resWL <- sapply(bvType,
                    function(bvType.i)
             {
                 sapply(variableType_mean,
                        function(variableType_mean.i)
                 {
                     grp_AoL_alloc_WL(alloc_st_catch = alloc_st_cdf,
                                      matched_data_catch = matched_data_cdf,
                                      distribution_data = distribution_data,
                                      distribution_data_N = resN,
                                      groupName = groupName,
                                      bvType = bvType.i,
                                      variableType_mean = variableType_mean.i,
                                      weighting = weighting,
                                      verbose = verbose)
                 }, simplify = FALSE)
             }, simplify = FALSE) %>%
        do.call(what = c) %>%
        bind_rows()

    ## colnames(resWL)
    ## resWL %>% group_by(variableType, sampledOrEstimated) %>% slice_tail(n = 2) %>% as.data.frame()

    res_distribution <- bind_rows(resN, resWL)

    res_catch <- alloc_st_cdf %>%
        mutate(domainBiology =
                   coalesce(domainBiology,
                            paste("[alloc]",
                                  seasonValue,
                                  areaValue,
                                  fisheriesManagementUnit,
                                  fleetValue,
                                  catchCategory, sep = "_"))) %>%
        left_join(res_distribution %>%
                  dplyr::select(vesselFlagCountry,
                                year,
                                workingGroup,
                                stock,
                                speciesCode,
                                catchCategory, # important to avoid duplication.
                                domainBiology,
                                allocGroup) %>%
                  dplyr::distinct(),
                  by = c("vesselFlagCountry", "year", "workingGroup", "stock",
                         "speciesCode", "catchCategory", # important to avoid duplication.
                         "domainBiology"))

    ## res_catch %>% group_by(dataType, allocGroup) %>% slice_head(n = 1) %>% as.data.frame()

    ## list(list(tibble(a = 1), tibble(a = 2)), list(tibble(c = 3))) %>% do.call(what = c) %>%
    ##     bind_rows()

    ## list(list(tibble(a = 1)), list(NULL)) %>% do.call(what = c) %>% bind_rows()

    ## mutate(domainBiology =
    ##            coalesce(domainBiology,
    ##                     paste("[alloc]",
    ##                           seasonValue,
    ##                           areaValue,
    ##                           fisheriesManagementUnit,
    ##                           fleetValue,
    ##                           catchCategory, sep = "_")))

    return(list(distribution = res_distribution,
                catch = res_catch))
}



catch_at_AoL_cond_loop <- function(catch_data,
                                   distribution_data,
                                   condition_alloc_st_list,
                                   condition_matched_data_list = condition_alloc_st_list,
                                   bvType = c("Age", "Length"),
                                   variableType = c("WGWeight", "OfficialWeight",
                                                    "WGNumber", "OfficialNumber"), # fraction to apply it to.
                                   variableType_mean = c("WeightLive"), # For the averaged
                                        # quantity. Only mean weight for now; complete for mean
                                        # length when defined.
                                   weighting = c("NumberAtAoL", "CATON"), # weighting for mean weights at age|length.
                                   verbose = TRUE,
                                   assembled_output = TRUE,
                                   append = FALSE,
                                   ...)
{
    ## Purpose: 
    ##   * Tests for validity of conditions.
    ##   * Loops over condition pairs (raised stratum, matched data) and
    ##     apply allocations.
    ##   * Formats data ().
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 24 Oct 2024, 13:55
    library(rlang)
    library(dplyr)

    variableType <- match.arg(variableType,
                              c("WGWeight", "OfficialWeight",
                                "WGNumber", "OfficialNumber"),
                              several.ok = FALSE) # Or should it allows to raise together WG and
                                        # Official weights or numbers? (not relevant to mix weights
                                        # and numbers for catch-number-at-age or length.)

    ## browser()

    ## ##################################################
    ## Check consistency:

    ## between condition list length:
    stopifnot(length(condition_alloc_st_list) == length(condition_matched_data_list))

    ##
    condition_alloc_st_list <-
        check_group_conditions(catch_data = catch_data,
                               condition_list =  condition_alloc_st_list,
                               conditionType = "strata",
                               domain = "domainBiology",
                               variableType = variableType,
                               append = append,
                               ...)

    condition_matched_data_list <-
        check_group_conditions(catch_data = catch_data,
                               condition_list = condition_matched_data_list,
                               conditionType = "matched_data",
                               domain = "domainBiology",
                               variableType = variableType,
                               append = TRUE,
                               ...)

    ## Fill in group names as needed:
    if (is.null(names(condition_alloc_st_list)))
    {
        names(condition_alloc_st_list) <-
            paste0("GA", seq_len(length.out = length(condition_alloc_st_list)))
    }

    if (is.null(names(condition_matched_data_list)))
    {
        names(condition_matched_data_list) <-
            names(condition_alloc_st_list)
    }

    ## ##################################################
    ## Allocations:

    ## Loops through conditions
    res <- mapply(grp_AoL_alloc_condition,
                  condition_alloc_st = condition_alloc_st_list,
                  condition_matched_data = condition_matched_data_list,
                  groupName = names(condition_alloc_st_list),
                  MoreArgs = list(catch_data = catch_data,
                                  distribution_data = distribution_data,
                                  bvType = bvType,
                                  variableType = variableType,
                                  variableType_mean = variableType_mean,
                                  weighting = weighting,
                                  verbose = verbose),
                  SIMPLIFY = FALSE)

    ## names(res)

    res_distribution <- sapply(res,
                               function(x){return(x$distribution)},
                               simplify = FALSE) %>%
        bind_rows()

    res_catch <- sapply(res,
                        function(x){return(x$catch)},
                        simplify = FALSE) %>%
        bind_rows()

    res_distribution %>% group_by(variableType, allocGroup) %>% slice_sample(n = 1) %>% as.data.frame()
    res_catch %>% group_by(variableType, allocGroup) %>% slice_sample(n = 1) %>% as.data.frame()

    if (isTRUE(assembled_output))
    {
        ## identifications of variable combinations actually allocated:
        varCombAlloc <- res_catch %>%
            select(vesselFlagCountry, year, workingGroup,
                   stock, speciesCode, catchCategory,
                   domainBiology,
                   variableType.c = variableType) %>%
            right_join(res_distribution,
                       by = join_by(vesselFlagCountry, year,
                                    workingGroup, stock, speciesCode,
                                    catchCategory, domainBiology)) %>%
            dplyr::select(c("variableType.c", "variableType", "bvType")) %>%
            distinct()

        ## Non allocated distributions based on variable:
        distribution_non_alloc <-
            catch_data %>%
            select(vesselFlagCountry, year, workingGroup,
                   stock, speciesCode, catchCategory,
                   domainBiology,
                   variableType.c = variableType) %>%
            right_join(distribution_data,
                       by = join_by(vesselFlagCountry, year,
                                    workingGroup, stock, speciesCode,
                                    catchCategory, domainBiology)) %>%
            anti_join(varCombAlloc,
                      by = join_by(variableType.c, bvType, variableType)) %>%
            select(-variableType.c)

        ## Catch data not selected (not matched to any group or other variableType):
        catch_data_non_used <-  catch_data %>%
            filter(! apply(sapply(condition_alloc_st_list, I),
                           1, any) |
                   ! variableType %in% {{variableType}})

        ## Non allocated distributions based on non selected matching catch_data:
        distribution_non_alloc2 <-
            catch_data_non_used %>%
            select(vesselFlagCountry, year, workingGroup,
                   stock, speciesCode, catchCategory,
                   domainBiology,
                   variableType.c = variableType) %>%
            right_join(distribution_data,
                       by = join_by(vesselFlagCountry, year,
                                    workingGroup, stock, speciesCode,
                                    catchCategory, domainBiology)) %>%
            semi_join(varCombAlloc,
                      by = join_by(variableType.c, bvType, variableType)) %>%
            select(-variableType.c)

        ## Assembling results and leftover data:
        res_distribution <- res_distribution %>%
            bind_rows(distribution_non_alloc,
                      distribution_non_alloc2)

        res_catch <- res_catch %>%
            bind_rows(catch_data_non_used)

        res_catch %>% group_by(variableType, allocGroup) %>% slice_sample(n = 1) %>% as.data.frame()
    }

    return(list(distribution = res_distribution,
                catch = res_catch))

}

## ###########################################################################
## Reporting functions:

catch_numbers_at_AoL_per_category <- function(distribution_data,
                                       catch_data,
                                       grouping = c("catchCategory", "attributeType", "attibuteValue"),
                                       variableType_catch = "WGWeight",
                                       bvType = "Age",
                                       minAoL = 0, maxAoL = NA,
                                       plusGroup = TRUE)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 27 Oct 2025, 13:36
    ## browser()
    tmp <- distribution_data %>%
        inner_join(catch_data %>%
                   filter(variableType %in% variableType_catch) %>%
                   select(vesselFlagCountry, year, workingGroup,
                          stock, speciesCode, catchCategory,
                          domainBiology,
                          any_of(grouping)),
                   by = join_by(vesselFlagCountry, year, workingGroup,
                                stock, speciesCode, catchCategory, domainBiology)) %>%
        filter(variableType %in% c("Number"),
               bvType %in% {{bvType}})

    if (is.na(maxAoL))
    {
        maxAoL2 <- max(tmp$bvValue, na.rm = TRUE)
    }else{
        maxAoL2 <- maxAoL
    }

    maxAoLdata <- ifelse(plusGroup || is.na(maxAoL),
                         max(tmp$bvValue, na.rm = TRUE),
                         maxAoL)

    res <- tmp %>%
        select(all_of(c(grouping, "bvValue", "variableUnit", "value"))) %>%
        filter(between(bvValue, minAoL, maxAoLdata)) %>% ## pull(bvValue) %>% unique() %>% sort()
        mutate(grp = if_else(bvValue < maxAoL2,
                             as.character(bvValue),
                             paste0(maxAoL2,
                                    ifelse(plusGroup & ! is.na(maxAoL),
                                           "+", ""))),
               grp = factor(grp,
                            levels = unique(grp)[order(as.numeric(sub("+",
                                                                      "",
                                                                      unique(grp),
                                                                      fixed = TRUE)))])) %>%
        dplyr::group_by(across(all_of(c(grouping, unit = "variableUnit", "grp")))) %>%
        summarize(N = sum(value, na.rm = TRUE), .groups = "drop") %>%
        pivot_wider(names_from = "grp", names_prefix = paste0("N_", bvType, "_"), values_from = "N") %>%
        mutate(across(where(is.numeric), ~replace_na(round(.x * 1e3, 2), 0))) ## %>% as.data.frame()

    return(res)
}


mean_WoL_at_AoL_per_category <- function(distribution_data,
                                         catch_data,
                                         grouping = c("catchCategory", "attributeType",
                                                      "attibuteValue"),
                                         weighting = c("NumberAtAoL", "CATON"), # weighting for mean
                                           # weights|length at age|length class. 
                                         variableType_catch = "WGWeight",
                                         variableType_dist = "WeightLive",
                                         bvType = "Age",
                                         minAoL = 0, maxAoL = NA,
                                         plusGroup = TRUE,
                                         samplesOnly = TRUE)
{
    ## Purpose:
    ## ----------------------------------------------------------------------
    ## Arguments:
    ## ----------------------------------------------------------------------
    ## Author: Yves Reecht, Date: 27 Oct 2025, 14:57
    ## browser()

    weighting <- match.arg(weighting, c("NumberAtAoL", "CATON"), several.ok = FALSE)

    variableType_dist <- match.arg(variableType_dist,
                                   c("WeightLive"), several.ok = FALSE)

    varPfx <- sub("^(.).*$", "\\1", variableType_dist)

    catch_data_sel <- catch_data %>%
        filter(variableType %in% variableType_catch) %>%
        mutate(CATON = total) %>%
        select(vesselFlagCountry, year, workingGroup,
               stock, speciesCode, catchCategory,
               domainBiology, CATON,
               any_of(grouping))

    tmp <- distribution_data %>%
        inner_join(catch_data_sel,
                   by = join_by(vesselFlagCountry, year, workingGroup,
                                stock, speciesCode, catchCategory, domainBiology)) %>%
        filter(variableType %in% variableType_dist,
               bvType %in% {{bvType}}) %>%
        ## Add sampled and eatimated catch numbers:
        left_join(distribution_data %>%
                  filter(variableType %in% "Number",
                         bvType %in% {{bvType}}) %>%
                  select(vesselFlagCountry, year, workingGroup,
                         stock, speciesCode, catchCategory, domainBiology,
                         attributeType, attibuteValue, bvType, bvUnit, bvValue,
                         NaAoL = value),
                  by = join_by(vesselFlagCountry, year, workingGroup,
                               stock, speciesCode, catchCategory, domainBiology,
                               attributeType, attibuteValue, bvType, bvUnit, bvValue))

    distribution_data %>% group_by(variableType) %>% slice_head(n = 1) %>% as.data.frame()
    tmp %>% group_by(variableType) %>% slice_head(n = 1) %>% as.data.frame()

    if (is.na(maxAoL))
    {
        maxAoL2 <- max(tmp$bvValue, na.rm = TRUE)
    }else{
        maxAoL2 <- maxAoL
    }

    maxAoLdata <- ifelse(plusGroup || is.na(maxAoL),
                         max(tmp$bvValue, na.rm = TRUE),
                         maxAoL)
    
    ## Field used for weighting, based on user choice:
    wgField <- case_when(weighting == "NumberAtAoL" ~ "NaAoL",
                         weighting == "CATON" ~ "total",
                         TRUE ~ NA)
    if (is.na(wgField))
        stop("mean weighting \"", weighting,
             "\" not implemented for weights-at-age|length")
    
    res <- tmp %>%
        select(all_of(c(grouping, "bvValue", "variableUnit", "value", "CATON", "NaAoL"))) %>%
        filter(between(bvValue, minAoL, maxAoLdata)) %>% ## pull(bvValue) %>% unique() %>% sort()
        mutate(grp = if_else(bvValue < maxAoL2,
                             as.character(bvValue),
                             paste0(maxAoL2,
                                    ifelse(plusGroup & ! is.na(maxAoL),
                                           "+", ""))),
               grp = factor(grp,
                            levels = unique(grp)[order(as.numeric(sub("+",
                                                                      "",
                                                                      unique(grp),
                                                                      fixed = TRUE)))])) %>%
        dplyr::group_by(across(all_of(c(grouping, unit = "variableUnit", "grp")))) %>%
        summarize(res = weighted.mean(x = value,
                                      w = !!sym(wgField),
                                      na.rm = TRUE),
                  .groups = "drop") %>%
        pivot_wider(names_from = "grp", names_prefix = paste0(varPfx, "_", bvType, "_"),
                    values_from = "res")
    
    return(res)
}




### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
