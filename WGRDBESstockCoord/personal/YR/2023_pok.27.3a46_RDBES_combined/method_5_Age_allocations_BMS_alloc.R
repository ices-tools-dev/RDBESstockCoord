#-*- coding: utf-8 -*-

### File: method_5_Age_allocations.R
### Time-stamp: <2023-10-06 10:58:06 a23579>
###
### Created: 03/10/2023	16:25:45
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
###
####################################################################################################


## ######################################################################
## During the 2016 benchmark, it was decided to include BMS landings from
##   Norway within landings, and BMS landings from other countries within
##   discards.
##   Updated caton, canum and weca outputs were post-processed based on
##   IC outputs. These scripts offer the possibility to incorporate it
##   in the raising/allocation process.
catonR.totB <- catonR.tot %>%
    mutate(Catch.Cat. = case_when(Catch.Cat. == "B" & Country == "Norway" ~ "L",
                                  Catch.Cat. == "B" & Country != "Norway" ~ "D",
                                  Catch.Cat. == "R" ~ "D", # Also includes logbook rep. disc. in D
                        (! Catch.Cat. %in% c("B", "R")) ~ Catch.Cat.)) ## %>%
## select(Country, Catch.Cat., CatchCat) %>% sample_n(10)
## filter()
## as.data.frame() %>% head(2)

table(catonR.totB$Catch.Cat., catonR.totB$CatchCat, useNA = "always")


sumN.pgrp.catB <- catonR.totB %>%
    group_by(Catch.Cat., AGroup) %>%
    summarise(across(starts_with("N.UndeterminedAge"), ~ sum(.x)))


## Aggregation per catch category:
sumN.catchCatB <- sumN.pgrp.catB %>%
    summarise(across(starts_with("N.UndeterminedAge"), ~ sum(.x)))



meanW.catchCatB <- sumN.pgrp.catB %>%
    group_modify(function(.x, .y, meanW) {
        ## Match mean weight at age per group to estimated numbers at age per group:
        NW <- .x %>%
            left_join(meanW)
        ## .y
        ## Mean weight at age for the group, based on allocation group estimates:
        meanWgrp <- apply(sweep(x = (NW %>%
                                     select(starts_with("W.UndeterminedAge"))) *
                                    (NW %>%
                                     select(starts_with("N.UndeterminedAge"))),
                                MARGIN = 2,
                                STATS = apply(NW %>%
                                              select(starts_with("N.UndeterminedAge")),
                                              2, sum, na.rm = TRUE),
                                FUN = "/"),
                          2, sum, na.rm = TRUE)
        ## return a tibble:
        enframe(meanWgrp) %>%
            pivot_wider() %>%
            return()
    }, meanW = meanW.pgrp) %>%
    {replace(., . == 0, NA)}

catchCat.longB <- sumN.catchCatB %>%
    pivot_longer(cols = starts_with("N.UndeterminedAge"),
                 names_to = "Sex.Age", values_to = "N") %>%
    mutate(age = as.numeric(sub("^N\\.([[:alpha:]]+)Age([[:digit:]]+)$",
                                "\\2", Sex.Age)),
           sex =  sub("^N\\.([[:alpha:]]+)Age([[:digit:]]+)$",
                      "\\1", Sex.Age)) %>%
    select(-Sex.Age) %>%
    full_join(meanW.catchCatB %>%
              pivot_longer(cols = starts_with("W.UndeterminedAge"),
                           names_to = "Sex.Age", values_to = "meanW") %>%
              mutate(age = as.numeric(sub("^W\\.([[:alpha:]]+)Age([[:digit:]]+)$",
                                          "\\2", Sex.Age)),
                     sex =  sub("^W\\.([[:alpha:]]+)Age([[:digit:]]+)$",
                                "\\1", Sex.Age)) %>%
              select(-Sex.Age))

## Aggregation of plus-group:
catchCat.long.plusB <- catchCat.longB %>%
    group_by(Catch.Cat., sex) %>%
    group_modify(function(.x, .y, plusGroup = 10) {
        .x %>%
            mutate(ageG = if_else(age < plusGroup,
                                  as.character(age),
                                  paste0(plusGroup, "+"))) %>%
            group_by(ageG) %>%
            summarise(meanW = weighted.mean(x = meanW, w = N, na.rm = TRUE),
                      N = sum(N, na.rm = TRUE)) %>%
            rename(age = ageG) %>%
            arrange(as.numeric(sub("\\+", "", age))) %>%
            relocate(meanW, .after = last_col())
    }) %>%
    ## Add aggregations for the whole catch (needed for the SA inputs):
    bind_rows(catchCat.longB %>%
              mutate(ageG = if_else(age < plusGroup,
                                    as.character(age),
                                    paste0(plusGroup, "+"))) %>%
              group_by(ageG, sex) %>% # summary over all catch categories
              summarise(meanW = weighted.mean(x = meanW, w = N, na.rm = TRUE),
                        N = sum(N, na.rm = TRUE)) %>%
              mutate(Catch.Cat. = "C") %>%
              rename(age = ageG) %>%
              arrange(as.numeric(sub("\\+", "", age))) %>%
              relocate(meanW, .after = last_col()))








### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
