#-*- coding: utf-8 -*-

### File: 2_age_alloc_saithe_2022_test.R
### Time-stamp: <2025-10-28 17:03:50 a23579>
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
sum(!is.na(discRaisedTest$DrGroup) & ! is.na(discRaisedTest$domainBiology) &
    discRaisedTest$catchCategory != "LAN")
sum(!is.na(discRaisedTest$DrGroup) & ! is.na(discRaisedTest$domainCatchBMS) &
    discRaisedTest$catchCategory != "LAN")


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
                           sourceType_catch = "WGValue", # fraction to apply it to.
                           variableType_catch = "WeightLive",
                           distributionType = "Age",
                           variableType_mean = "WeightLive",
                           logFile = "Log.txt", ## append = TRUE,
                           assembled_output = TRUE)

distribution_alloc <- CaAallocTest$distribution
catch_alloc <- CaAallocTest$catch

## ##################################################
## Aggregated catch numbers at age:

## Catch number at age 0 to 10 (not a plus group):
catch_numbers_at_AoL_per_category(distribution_alloc,
                                  catch_alloc,
                                  grouping = c("catchCategory", "importedOrRaised"),
                                  maxAoL = 10,
                                  plusGroup = FALSE) %>% as.data.frame()

## Catch number at age 3 to 10+ (as used in assessment)
##   +default grouping (catchCategory and attributes):
catch_numbers_at_AoL_per_category(distribution_alloc,
                                  catch_alloc,
                                  minAoL = 3,
                                  maxAoL = 10,
                                  plusGroup = TRUE,
                                  round = 0) %>% as.data.frame()


## ##################################################
## Aggregated catch weights at age:

as.data.frame(head(distribution_alloc, 2))

table(distribution_alloc$allocGroup, distribution_alloc$sampledOrEstimated, useNA = "ifany")
table(catch_alloc$allocGroup, catch_alloc$catchCategory, useNA = "ifany")

mean_WoL_at_AoL_per_category(distribution_alloc,
                             catch_alloc,
                             minAoL = 3,
                             maxAoL = 10,
                             plusGroup = TRUE,
                             unit = "g",
                             round = 0) %>% as.data.frame()

## All catch:
mean_WoL_at_AoL_per_category(distribution_alloc,
                             catch_alloc,
                             grouping = c(##"catchCategory",
                                          "attributeType",
                                          "attibuteValue"),
                             minAoL = 3,
                             maxAoL = 10,
                             plusGroup = TRUE,
                             unit = "g",
                             round = 0) %>% as.data.frame()

## All ages, in kg:
mean_WoL_at_AoL_per_category(distribution_alloc,
                             catch_alloc,
                             grouping = c("catchCategory",
                                          "attributeType",
                                          "attibuteValue"),
                             minAoL = 3,
                             maxAoL = NA,
                             plusGroup = FALSE,
                             unit = "kg",
                             round = 3) %>% as.data.frame()



## ####################################################################################################
## ####################################################################################################
distributions %>% group_by(variableType) %>% slice(1) %>% as.data.frame()

discRaisedTest %>% group_by(catchCategory, variableType) %>% slice(1) %>% as.data.frame()

table(discRaisedTest$importedOrRaised, discRaisedTest$catchCategory, useNA = "ifany")

distribution_alloc %>% group_by(variableType, sampledOrEstimated) %>% slice_head(n = 2) %>%
    as.data.frame()

distribution_alloc %>% group_by(variableType, sampledOrEstimated) %>% slice_head(n = 2) %>%
    convert_field("value") %>% as.data.frame()

catch_alloc %>% group_by(variableType, has_alloc = ! is.na(allocGroup)) %>% slice_sample(n = 1) %>%
    as.data.frame()

catch_numbers_at_AoL_per_category(distribution_alloc,
                                  catch_alloc)

## Discards without landings?
testDISnoLAN <- catch_alloc %>% filter(catchCategory %in% c("DIS", "BMS"),
                                       variableType %in% "WGWeight") %>%
    anti_join(catch_alloc %>%
              filter(catchCategory %in% c("LAN")) %>%
              select(all_of(c("vesselFlagCountry", "year",
                              "workingGroup", "stock",
                              "speciesCode", "domainCatchDis"))),
              by = c("vesselFlagCountry", "year",
                     "workingGroup", "stock",
                     "speciesCode", "domainCatchDis")) %>%
    mutate(noLan = TRUE)
    ## group_by(vesselFlagCountry) %>%
## slice_head(n = 1) %>% as.data.frame()

testDISnoLAN %>%
    group_by(vesselFlagCountry, catchCategory) %>%
    slice_head(n = 1) %>% as.data.frame()

## This one should have zero-landings declared!
catch_alloc %>%
    filter(vesselFlagCountry == "Sweden",
           seasonValue == "1",
           areaValue == "27.3.a.20",
           fleetValue == "FPO_CRU_0_0_0_all") %>% as.data.frame()

discRaisedTest %>% filter(catchCategory %in% "DIS") %>%
    anti_join(discRaisedTest %>%
              filter(catchCategory %in% "LAN") %>%
              select(all_of(c("vesselFlagCountry", "year",
                              "workingGroup", "stock",
                              "speciesCode", "domainCatchDis"))),
              by = c("vesselFlagCountry", "year",
                     "workingGroup", "stock",
                     "speciesCode", "domainCatchDis"))

catch_data %>% filter(catchCategory %in% "DIS",
                      variableType %in% "WGWeight") %>%
    anti_join(catch_data %>%
              filter(catchCategory %in% "LAN") %>%
              select(all_of(c("vesselFlagCountry", "year",
                              "workingGroup", "stock",
                              "speciesCode", "domainCatchDis"))),
              by = c("vesselFlagCountry", "year",
                     "workingGroup", "stock",
                     "speciesCode", "domainCatchDis")) %>%
    group_by(vesselFlagCountry) %>%
    slice_head(n = 1) %>% as.data.frame()

distribution_alloc %>%
    left_join(testDISnoLAN %>% 
              select(vesselFlagCountry, year, workingGroup,
                     stock, speciesCode, catchCategory,
                     domainBiology, noLan)) %>%
    inner_join(catch_alloc %>%
               filter(variableType %in% "WGWeight") %>%
               select(vesselFlagCountry, year, workingGroup,
                      stock, speciesCode, catchCategory,
                      domainBiology)) %>%
    filter(variableType %in% c("Number"),
           distributionType %in% "Age") %>%
    select(catchCategory, sampledOrEstimated, distributionValue, value, noLan) %>%
    mutate(grp = if_else(distributionValue < 10,
                         as.character(distributionValue),
                         "10+"),
           grp = factor(grp,
                        levels = unique(grp)[order(as.numeric(sub("+",
                                                                  "",
                                                                  unique(grp),
                                                                  fixed = TRUE)))]),
           noLan = coalesce(noLan, FALSE)) %>%
    group_by(catchCategory, grp, noLan) %>%
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
           distributionType %in% "Age",
           sampledOrEstimated %in% "sampled") %>%
    dim()

distributions %>%
    inner_join(catch_data %>%
               filter(variableType %in% "WGWeight") %>%
               select(vesselFlagCountry, year, workingGroup,
                      stock, speciesCode, catchCategory,
                      domainBiology)) %>%
    filter(variableType %in% c("Number"),
           distributionType %in% "Age") %>%
    dim()

distributions %>%
    inner_join(catch_data %>%
               filter(variableType %in% "WGWeight") %>%
               select(vesselFlagCountry, year, workingGroup,
                      stock, speciesCode, catchCategory,
                      domainBiology)) %>%
    filter(variableType %in% c("Number"),
           distributionType %in% "Age") %>%
    select(catchCategory, distributionValue, value) %>%
    mutate(grp = if_else(distributionValue < 10,
                         as.character(distributionValue),
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
           distributionType %in% "Age",
           sampledOrEstimated %in% "sampled") %>%
    dim()

distributions %>%
    inner_join(catch_data %>%
               filter(variableType %in% "WGWeight") %>%
               select(vesselFlagCountry, year, workingGroup,
                      stock, speciesCode, catchCategory,
                      domainBiology)) %>%
    filter(variableType %in% c("WeightLive"),
           distributionType %in% "Age") %>%
    dim()

table(distributions$variableType)

### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
