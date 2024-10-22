#-*- coding: utf-8 -*-

### File: method_3_group_definitions_Age.R
### Time-stamp: <2023-10-04 13:19:06 a23579>
###
### Created: 03/10/2023	14:45:38
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
###
####################################################################################################

## ###########################################################################
## Allocation catch and weight at age:

## Defining groups in the catch data (eq. to making strata in Sofie's script):
catonR2 <- catonR %>%
    mutate(CatchCat = Catch.Cat.,
           Catch.Cat. = substr(x = sub("Logbook ", "",
                                       Catch.Cat.),
                               1, 1)) %>% # Same coding as number_at_age, for easier join.
    mutate(AGroup = case_when(## Landings area 4, per season:
                        Catch.Cat. == "L" & Area1 == "4" & Season == 1 ~ "GA1",
                        Catch.Cat. == "L" & Area1 == "4" & Season == 2 ~ "GA2",
                        Catch.Cat. == "L" & Area1 == "4" & Season == 3 ~ "GA3",
                        Catch.Cat. == "L" & Area1 == "4" & Season == 4 ~ "GA4",
                        Catch.Cat. == "L" & Area1 == "4" & Season == 2022 ~ "GA5",
                        ## Landings area 3, per season:
                        Catch.Cat. == "L" & Area1 == "3" & Season == 1 ~ "GA6",
                        Catch.Cat. == "L" & Area1 == "3" & Season == 2 ~ "GA7",
                        Catch.Cat. == "L" & Area1 == "3" & Season == 3 ~ "GA8",
                        Catch.Cat. == "L" & Area1 == "3" & Season == 4 ~ "GA9",
                        ## Landings area 6, all seasons matched together:
                        Catch.Cat. == "L" & Area1 == "6" ~ "GA10",
                        ## Discards and BMS land., per area groups, all seasons:
                        Catch.Cat. != "L" & Area1 %in% c("3", "6") ~ "GA11",
                        Catch.Cat. != "L" & Area1 == "4" ~ "GA12",
                        ## Just in case we've forgotten groups!:
                        TRUE ~ "Missing"))

## Test:
if (nrow(catonR2 %>%
         filter(AGroup %in% "Missing")) > 0)
{
    stop("Missing age allocation groups!")
}

## Matched samples:
##  * uses a list of conditions as one record can be used in several groups =>
##    need for loops.
##  * conditions in quosures for ease of programming.
AGroupCond <-
    list(## Landings area 4, per season:
        GA1 = quo(Catch.Cat. == "L" & Area1 == "4" & Season == 1),
        GA2 = quo(Catch.Cat. == "L" & Area1 == "4" & Season == 2),
        GA3 = quo(Catch.Cat. == "L" & Area1 == "4" & Season == 3),
        GA4 = quo(Catch.Cat. == "L" & Area1 == "4" & Season == 4),
        GA5 = quo(Catch.Cat. == "L" & Area1 == "4"), # 2022 matched to all seasons
        ## Landings area 3, per season:
        GA6 = quo(Catch.Cat. == "L" & Area1 == "3" & Season == 1),
        GA7 = quo(Catch.Cat. == "L" & Area1 == "3" & Season == 2),
        GA8 = quo(Catch.Cat. == "L" & Area1 == "3" & Season == 3),
        GA9 = quo(Catch.Cat. == "L" & Area1 == "3" & Season == 4),
        ## Landings area 6, all seasons together (too few samples):
        GA10 = quo(Catch.Cat. == "L" & Area1 == "6"), # All seasons matched to all seasons
        ## Discards, etc., per area groups:
        GA11 = quo(Catch.Cat. != "L" & Area1 %in% c("3", "6")),
        GA12 = quo(Catch.Cat. != "L" & Area1 == "4"))









### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
