#-*- coding: utf-8 -*-

### File: 2_age_alloc_saithe_2022_test.R
### Time-stamp: <2025-10-24 17:31:46 a23579>
###
### Created: 21/10/2025	14:39:25
### Author: Yves Reecht
###
####################################################################################################
### Description:
### 
### 
####################################################################################################


discRaisedTest %>%
    mutate(cc = catchCategory) %>%
    group_by(catchCategory, !is.na(domainBiology), !is.na(DrGroup)) %>%
    slice_sample(n = 1) %>% as.data.frame()

## Make sure no raised data with domainBiology defined: (former issue, fixed now)
sum(!is.na(discRaisedTest$DrGroup) & ! is.na(discRaisedTest$domainBiology))
sum(!is.na(discRaisedTest$DrGroup) & ! is.na(discRaisedTest$domainCatchBMS))


source(file.path(scriptDir, "0_Functions_age-length_alloc.R"))

allocStrataCond <-
    list(GA1 = quo(catchCategory == "LAN" & Area1 == "4" & Season == 1),
         GA2 = quo(catchCategory == "LAN" & Area1 == "4" & Season == 2),
         GA3 = quo(catchCategory == "LAN" & Area1 == "4" & Season == 3),
         GA4 = quo(catchCategory == "LAN" & Area1 == "4" & Season == 4),
         GA5 = quo(catchCategory == "LAN" & Area1 == "4" & Season == 2022),
         ## Landings area 3, per season:
         GA5 = quo(catchCategory == "LAN" & Area1 == "3" & Season == 1),
         GA6 = quo(catchCategory == "LAN" & Area1 == "3" & Season == 2),
         GA7 = quo(catchCategory == "LAN" & Area1 == "3" & Season == 3),
         GA8 = quo(catchCategory == "LAN" & Area1 == "3" & Season == 4),
         ## Landings area 6, all seasons matched together:
         GA9 = quo(catchCategory == "LAN" & Area1 == "6"),
         ## Discards and BMS land., per area groups, all seasons:
         GA10 = quo(catchCategory %in% c("DIS", "BMS") & Area1 %in% c("3", "6")),
         GA11 = quo(catchCategory %in% c("DIS", "BMS") & Area1 == "4"))

bioMatchedCond <-
    list(## Landings area 4, per season:
        GA1 = quo(catchCategory == "LAN" & Area1 == "4" & Season == 1),
        GA2 = quo(catchCategory == "LAN" & Area1 == "4" & Season == 2),
        GA3 = quo(catchCategory == "LAN" & Area1 == "4" & Season == 3),
        GA4 = quo(catchCategory == "LAN" & Area1 == "4" & Season == 4),
        GA5 = quo(catchCategory == "LAN" & Area1 == "4"), # 2022 matched to all seasons
        ## Landings area 3, per season:
        GA6 = quo(catchCategory == "LAN" & Area1 == "3" & Season == 1),
        GA7 = quo(catchCategory == "LAN" & Area1 == "3" & Season == 2),
        GA8 = quo(catchCategory == "LAN" & Area1 == "3" & Season == 3),
        GA9 = quo(catchCategory == "LAN" & Area1 == "3" & Season == 4),
        ## Landings area 6, all seasons together (too few samples):
        GA10 = quo(catchCategory == "LAN" & Area1 == "6"), # All seasons matched to all seasons
        ## Discards, etc., per area groups:
        GA11 = quo(catchCategory %in% c("DIS", "BMS") & Area1 %in% c("3", "6")),
        GA12 = quo(catchCategory %in% c("DIS", "BMS") & Area1 == "4"))

## Check conditions (optional - automatically done during allocation):

cond_bio_test <- check_group_conditions(catch_data = discRaisedTest,
                                        condition_list = allocStrataCond,
                                        logFile = NULL, append = TRUE,
                                        domain = "domainBiology")

cond_bio_test2 <- check_group_conditions(catch_data = discRaisedTest,
                                         condition_list = bioMatchedCond,
                                         conditionType = "matched_data",
                                         logFile = NULL, append = TRUE,
                                         domain = "domainBiology")

## catch at age test:
CaAallocTest <-
    catch_at_AoL_cond_loop(catch_data = discRaisedTest,
                           distribution_data = distributions,
                           condition_alloc_st_list = allocStrataCond,
                           condition_matched_data_list = bioMatchedCond, # Optional if same as
                                        # allocation strata (condition_raising_st_list)!
                           bvType = "Age",
                           variableType = "WGWeight", # fraction to apply it to.
                           logFile = "Log.txt", ## append = TRUE,
                           assembled_output = TRUE)

distribution_alloc <- CaAallocTest$distribution
catch_alloc <- CaAallocTest$catch

distributions %>% group_by(variableType) %>% slice(1) %>% as.data.frame()

discRaisedTest %>% group_by(catchCategory, variableType) %>% slice(1) %>% as.data.frame()

table(discRaisedTest$importedOrRaised, discRaisedTest$catchCategory, useNA = "ifany")

distribution_alloc %>% group_by(variableType, sampledOrEstimated) %>% slice_head(n = 1) %>% as.data.frame()

catch_alloc %>% group_by(variableType, has_alloc = ! is.na(allocGroup)) %>% slice_sample(n = 1) %>%
    as.data.frame()

distribution_alloc %>%
    inner_join(catch_alloc %>%
               filter(variableType %in% "WGWeight") %>%
               select(vesselFlagCountry, year, workingGroup,
                      stock, speciesCode, catchCategory,
                      domainBiology)) %>%
    filter(variableType %in% c("Number"),
           bvType %in% "Age") %>%
    select(catchCategory, sampledOrEstimated, bvValue, value) %>%
    mutate(grp = if_else(bvValue < 10,
                         as.character(bvValue),
                         "10+"),
           grp = factor(grp,
                        levels = unique(grp)[order(as.numeric(sub("+",
                                                                  "",
                                                                  unique(grp))))])) %>%
    group_by(catchCategory, grp) %>%
    summarize(N = sum(value, na.rm = TRUE)) %>%
    pivot_wider(names_from = "grp", names_prefix = "N_", values_from = "N") %>%
    mutate(across(where(is.numeric), ~round(.x * 1e3, 2))) %>%
    as.data.frame()

distribution_alloc %>%
    inner_join(catch_alloc %>%
               filter(variableType %in% "WGWeight") %>%
               select(vesselFlagCountry, year, workingGroup,
                      stock, speciesCode, catchCategory,
                      domainBiology)) %>%
    filter(variableType %in% c("Number"),
           bvType %in% "Age",
           sampledOrEstimated %in% "sampled") %>%
    dim()

distributions %>%
    inner_join(catch_data %>%
               filter(variableType %in% "WGWeight") %>%
               select(vesselFlagCountry, year, workingGroup,
                      stock, speciesCode, catchCategory,
                      domainBiology)) %>%
    filter(variableType %in% c("Number"),
           bvType %in% "Age") %>%
    dim()

distributions %>%
    inner_join(catch_data %>%
               filter(variableType %in% "WGWeight") %>%
               select(vesselFlagCountry, year, workingGroup,
                      stock, speciesCode, catchCategory,
                      domainBiology)) %>%
    filter(variableType %in% c("Number"),
           bvType %in% "Age") %>%
    select(catchCategory, bvValue, value) %>%
    mutate(grp = if_else(bvValue < 10,
                         as.character(bvValue),
                         "10+"),
           grp = factor(grp,
                        levels = unique(grp)[order(as.numeric(sub("+",
                                                                  "",
                                                                  unique(grp))))])) %>%
    group_by(catchCategory, grp) %>%
    summarize(N = sum(value, na.rm = TRUE)) %>%
    pivot_wider(names_from = "grp", names_prefix = "N_", values_from = "N") %>%
    mutate(across(where(is.numeric), ~round(.x * 1e3, 2))) %>%
    as.data.frame()

## WeightLive:
distribution_alloc %>%
    inner_join(catch_alloc %>%
               filter(variableType %in% "WGWeight") %>%
               select(vesselFlagCountry, year, workingGroup,
                      stock, speciesCode, catchCategory,
                      domainBiology)) %>%
    filter(variableType %in% c("WeightLive"),
           bvType %in% "Age",
           sampledOrEstimated %in% "sampled") %>%
    dim()

distributions %>%
    inner_join(catch_data %>%
               filter(variableType %in% "WGWeight") %>%
               select(vesselFlagCountry, year, workingGroup,
                      stock, speciesCode, catchCategory,
                      domainBiology)) %>%
    filter(variableType %in% c("WeightLive"),
           bvType %in% "Age") %>%
    dim()

table(distributions$variableType)

### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
