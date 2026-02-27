#-*- coding: utf-8 -*-

### File: 2_age_alloc_saithe_2022_test.R
### Time-stamp: <2026-02-27 15:34:18 a23579>
###
### Created: 21/10/2025	14:39:25
### Author: Yves Reecht
###
####################################################################################################
### Description:
### 
### 
####################################################################################################


catch_data_raised %>%
    mutate(cc = catchCategory) %>%
    group_by(catchCategory, !is.na(domainBiology), !is.na(DrGroup)) %>%
    slice_sample(n = 1) %>% as.data.frame()

## Make sure no raised data with domainBiology defined: (former issue, fixed now)
sum(!is.na(catch_data_raised$DrGroup) & ! is.na(catch_data_raised$domainBiology) &
    catch_data_raised$catchCategory != "LAN")
sum(!is.na(catch_data_raised$DrGroup) & ! is.na(catch_data_raised$domainCatchBMS) &
    catch_data_raised$catchCategory != "LAN")


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
        GA11 = quo(catchCategory %in% c("DIS", "BMS") & Area1 %in% c("3", "6")##  &
                   ## (! Country %in% "UK(Scotland)")
                   ),
        GA12 = quo(catchCategory %in% c("DIS", "BMS") & Area1 == "4"))

## Check conditions (optional - automatically done during allocation):

cond_bio_test <- check_group_conditions(catch_data = catch_data_raised,
                                        condition_list = allocStrataCond,
                                        logFile = NULL, append = TRUE,
                                        domain = "domainBiology")

cond_bio_test2 <- check_group_conditions(catch_data = catch_data_raised,
                                         condition_list = bioMatchedCond,
                                         conditionType = "matched_data",
                                         logFile = NULL, append = TRUE,
                                         domain = "domainBiology")

## Allocation of catch numbers and mean weights at age:
CaAallocTest <-
    catch_at_AoL_cond_loop(catch_data = catch_data_raised,
                           distribution_data = distributions,
                           condition_alloc_st_list = allocStrataCond,
                           condition_matched_data_list = bioMatchedCond, # Optional if same as
                                        # allocation strata (condition_raising_st_list)!
                           originType_catch = "WGValue", # fraction to apply it to.
                           variableType_catch = "WeightLive",
                           distributionType = "Age",
                           variableType_mean = "WeightLive",
                           logFile = "Log.txt", ## append = TRUE,
                           assembled_output = TRUE)

## Returns a list of table -> extraction of those:
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


## ###########################################################################
## Comparisons:

stockOverviewRaised <- read_csv(file.path("../2023_pok.27.3a46_RDBES_combined/output",
                                          "StockOverview_raised.txt"))

## stockOverviewRaised %>% head(1) %>% as.data.frame()

stockOverviewRaised %>%
    filter(CatchCat %in% "Discards",
           is.na(Caton)) %>%
    group_by(AGroup, estimated = is.na(Caton)) %>%
    dplyr::summarize(across(starts_with("N.UndeterminedAge"),
                            ~ round(sum(.x, na.rm = TRUE)))) %>%
    rename_with(~sub("N.UndeterminedAge", "N_Age_", .x)) %>%
    select(1:9) %>% as.data.frame()

catch_numbers_at_AoL_per_category(distribution_alloc,
                                  catch_alloc,
                                  grouping = c("catchCategory", "allocGroup", "sampledOrEstimated"),
                                  maxAoL = 6,
                                  plusGroup = FALSE,
                                  round = 0) %>%
    filter(catchCategory %in% "DIS",
           sampledOrEstimated %in% "estimated") %>% as.data.frame()

## CANUM data:
Canum_ICTAF <- read_csv(file.path("../2023_pok.27.3a46_RDBES_combined/output",
                                  "canum_table.csv"))

Canum_New <- catch_numbers_at_AoL_per_category(distribution_alloc,
                                               catch_alloc,
                                               grouping = c("catchCategory"),
                                               maxAoL = 10,
                                               plusGroup = TRUE,
                                               round = NULL) %>%
    bind_rows(catch_numbers_at_AoL_per_category(distribution_alloc,
                                                catch_alloc,
                                                grouping = c("attributeType", "attibuteValue"),
                                                maxAoL = 10,
                                                plusGroup = TRUE,
                                                round = NULL) %>%
              mutate(catchCategory = "CAT")) %>%
    mutate(Catch.Cat. = sub("^(.).*$", "\\1", catchCategory)) %>%
    rename_with(~sub("N_A", "a", .x))

## WECA data:
Weca_ICTAF <- read_csv(file.path("../2023_pok.27.3a46_RDBES_combined/output",
                                  "weca_table.csv"))

Weca_New <- mean_WoL_at_AoL_per_category(distribution_alloc,
                                         catch_alloc,
                                         grouping = c("catchCategory",
                                                      "attributeType",
                                                      "attibuteValue"),
                                         maxAoL = 10,
                                         plusGroup = TRUE,
                                         round = NULL) %>%
    bind_rows(mean_WoL_at_AoL_per_category(distribution_alloc,
                                           catch_alloc,
                                           grouping = c("attributeType", "attibuteValue"),
                                           maxAoL = 10,
                                           plusGroup = TRUE,
                                           round = NULL) %>%
              mutate(catchCategory = "CAT")) %>%
    mutate(Catch.Cat. = sub("^(.).*$", "\\1", catchCategory)) %>%
    rename_with(~sub("W_A", "a", .x))

Weca_New

library(scales)

## Deviations:
Canum_ICTAF %>%
    select(Catch.Cat.) %>%
    left_join(Canum_New %>%
              select(c(Catch.Cat., catchCategory))) %>%
    bind_cols(Canum_ICTAF %>% select(Catch.Cat.) %>%
              left_join(Canum_New %>%
                        select(all_of(colnames(Canum_ICTAF)))) %>%
              select(-Catch.Cat.) /
              Canum_ICTAF %>% select(-Catch.Cat.)) %>%
    mutate(across(where(is.numeric),
                  ~percent(.x - 1, accuracy = 2)))


Weca_ICTAF %>%
    select(Catch.Cat.) %>%
    left_join(Weca_New %>%
              select(c(Catch.Cat., catchCategory))) %>%
    bind_cols(Weca_ICTAF %>% select(Catch.Cat.) %>%
              left_join(Weca_New %>%
                        select(all_of(colnames(Weca_ICTAF)))) %>%
              select(-Catch.Cat.) /
              Weca_ICTAF %>% select(-Catch.Cat.)) %>%
    mutate(across(where(is.numeric),
                  ~percent(.x - 1, accuracy = 2))) %>%
    as.data.frame()


## ####################################################################################################
## ####################################################################################################
## Examples other functions:

## ##################################################
## Unit conversion:
distribution_alloc %>%
    group_by(variableType, sampledOrEstimated) %>%
    slice_head(n = 2) %>%
    ## convert_field("value") %>%
    as.data.frame()

##  Conversion to kg and 1000_pcs:
distribution_alloc %>%
    group_by(variableType, sampledOrEstimated) %>%
    slice_head(n = 2) %>%
    convert_field(valueField = "value",
                  to = c("kg", "1000_pcs")) %>% # ignores silently invalid conversions.
    as.data.frame()

## ###########################################################################
## Example of groups based on a simple strata (one or several fields),
## matched to same:
colnames(catch_data)

testCondGear <- fieldsToStrata(catch_data, # here for discard raising conditions;
                                        # should use catch_data_raised if groups for allocations. 
                               "gear")

## First six elements of each group:
sapply(testCondGear, head)

## Same with a combination of 2 fields
##   (use any tidy synthaxe compatible with dplyr::select()):
testCondGearA <- fieldsToStrata(catch_data,
                                c(gear, Area1))

sapply(testCondGearA, head)


## ##################################################
## Some exploration of results:

table(distribution_alloc$allocGroup,
      distribution_alloc$sampledOrEstimated,
      useNA = "ifany")

table(catch_alloc$allocGroup,
      catch_alloc$catchCategory,
      useNA = "ifany")

## Discards without landings?
testDISnoLAN <- catch_alloc %>%
    filter(catchCategory %in% c("DIS"),
           originType %in% "WGValue",
           variableType %in% "WeightLive") %>%
    anti_join(catch_alloc %>%
              filter(catchCategory %in% c("LAN")) %>%
              select(all_of(c("vesselFlagCountry", "year",
                              "workingGroup", "stock",
                              "speciesCode", "domainCatchDis"))),
              by = c("vesselFlagCountry", "year",
                     "workingGroup", "stock",
                     "speciesCode", "domainCatchDis")) %>%
    mutate(noLan = TRUE)

testDISnoLAN %>%
    ## group_by(vesselFlagCountry) %>%
    ## slice_head(n = 1) %>%
    as.data.frame()

## Sampled fraction of distribution should be unchanged (but has two extra fields):
distribution_alloc %>%
    inner_join(catch_alloc %>%
               filter(originType %in% "WGValue",
                      variableType %in% "WeightLive") %>%
               select(vesselFlagCountry, year, workingGroup,
                      stock, speciesCode, catchCategory,
                      domainBiology)) %>%
    filter(variableType %in% c("Number"),
           distributionType %in% "Age",
           sampledOrEstimated %in% "sampled") %>%
    dim()

distributions %>%
    inner_join(catch_data %>%
               filter(originType %in% "WGValue",
                      variableType %in% "WeightLive") %>%
               select(vesselFlagCountry, year, workingGroup,
                      stock, speciesCode, catchCategory,
                      domainBiology)) %>%
    filter(variableType %in% c("Number"),
           distributionType %in% "Age") %>%
    dim()

## Allocated distributions (mean weight), corresponding to WG catch estimates:
distribution_alloc %>%
    inner_join(catch_alloc %>%
               filter(originType %in% "WGValue",
                      variableType %in% "WeightLive") %>%
               select(vesselFlagCountry, year, workingGroup,
                      stock, speciesCode, catchCategory,
                      domainBiology)) %>%
    filter(variableType %in% c("WeightLive"),
           distributionType %in% "Age") %>%
    group_by(sampledOrEstimated) %>%
    slice_head(n = 2) %>% as.data.frame()


table(distributions$variableType)

### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
