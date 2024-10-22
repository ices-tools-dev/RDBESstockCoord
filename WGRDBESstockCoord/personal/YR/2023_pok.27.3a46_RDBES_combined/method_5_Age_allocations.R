#-*- coding: utf-8 -*-

### File: method_5_Age_allocations.R
### Time-stamp: <2023-10-06 11:43:23 a23579>
###
### Created: 03/10/2023	16:25:45
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
###
####################################################################################################

## Extraction of sampled and unsampled fractions:
catonR.samp <- catonR2 %>%
    semi_join(numbers)

catonR.unsamp <- catonR2 %>%
    anti_join(numbers)

catonR.samp.W <- catonR2 %>%
    semi_join(weights)

catonR.unsamp.W <- catonR2 %>%
    anti_join(weights)

## Result objects:
meanW.l <- list()
sumN.l <- list()
catonR.l <- list()

for (grp in names(AGroupCond))
{
    ## Loop starts here:
    ## grp <- "GA1"
    allocCond <- AGroupCond[[grp]]

    ## ## Some basic checks:
    ## nrow(catonR.samp)
    ## nrow(catonR.unsamp)

    ## nrow(catonR.samp.W)
    ## nrow(catonR.unsamp.W)

    catonR.unsamp.grp <- catonR.unsamp %>%
        filter(AGroup == grp)

    catonR.samp.grp <- catonR.samp %>%
        filter(AGroup == grp)

    catonR.unsamp.W.grp <- catonR.unsamp.W %>%
        filter(AGroup == grp)

    if (nrow(catonR.unsamp.W.grp %>%
             anti_join(catonR.unsamp.grp)) ||
        nrow(catonR.unsamp.grp %>%
             anti_join(catonR.unsamp.W.grp)))
    {
        warning("Group ", grp, ": the weight and age samplings do not match.")
    }

    ## Filtering samples for allocation group:
    numbers.grp <- numbers %>%
        filter(!!enquo(allocCond))
    weights.grp <- weights %>%
        filter(!!enquo(allocCond))

    ## Sums over samples within the stratum:
    totNumbers.grp  <-  numbers.grp %>%
        ## group_by(AGroup) %>%
        dplyr::summarise(across(c("Caton"), ~ sum(.x, na.rm = TRUE)),
                         across(starts_with("UndeterminedAge"), ~ sum(.x, na.rm = TRUE)))

    ## Estimated number at age for unsampled fraction:
    estN.grp <-
        {as.matrix(catonR.unsamp.grp %>%
                   select(Catch..kg)) %*%
             as.matrix(totNumbers.grp %>%
                       select(starts_with("UndeterminedAge"))) /
             totNumbers.grp$Caton
        } %>%
        as_tibble() %>%
        rename_with(~ paste0("N.", .x))

    ## Assemble sampled and unsampled data:
    catonR.grp <- catonR.samp.grp %>%
        inner_join(numbers.grp) %>% # some numbers used several times,
                                    # the inner join on sampled data for the group
                                    # ensures we don't duplicate entries.
        rename_with(~paste0("N.", .x), starts_with("UndeterminedAge")) %>%
        ## Unsampled with estimated catch at age numbers:
        bind_rows(catonR.unsamp.grp %>%
                  bind_cols(estN.grp))

    ## Match sampled weights and numbers (order may differ):
    NW <- numbers.grp %>%
        rename_with(~paste0("N.", .x), starts_with("UndeterminedAge")) %>%
        full_join(weights.grp %>%
                  rename_with(~paste0("W.", .x), starts_with("UndeterminedAge")))

    ## Mean weight at age for the group, based on sampled strata:
    meanWgrp <- apply(sweep(x = (NW %>%
                                 select(starts_with("W.UndeterminedAge"))) *
                                (NW %>%
                                 select(starts_with("N.UndeterminedAge"))),
                            MARGIN = 2,
                            STATS = apply(NW %>%
                                          select(starts_with("N.UndeterminedAge")),
                                          2, sum, na.rm = TRUE),
                            FUN = "/"),
                      2, sum)

    ## Raised catch at age for the group:
    sumNgrp <- apply(catonR.grp %>%
                     select(starts_with("N.UndeterminedAge")),
                     2, sum, na.rm = TRUE)

    ## Save group results:
    meanW.l[[grp]] <- meanWgrp
    sumN.l[[grp]] <- sumNgrp

    catonR.l[[grp]] <- catonR.grp
}
## Loop ends here!

## Assemble result lists:
meanW.pgrp <- bind_cols(AGroup = names(meanW.l),
                        bind_rows(meanW.l))

sumN.pgrp <- bind_cols(AGroup = names(sumN.l),
                       bind_rows(sumN.l))

catonR.tot <- bind_rows(catonR.l)

sumN.pgrp.cat <- catonR.tot %>%
    group_by(Catch.Cat., AGroup) %>%
    summarise(across(starts_with("N.UndeterminedAge"), ~ sum(.x)))


## Aggregation per catch category:
sumN.catchCat <- sumN.pgrp.cat %>%
    summarise(across(starts_with("N.UndeterminedAge"), ~ sum(.x)))



meanW.catchCat <- sumN.pgrp.cat %>%
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

catchCat.long <- sumN.catchCat %>%
    pivot_longer(cols = starts_with("N.UndeterminedAge"),
                 names_to = "Sex.Age", values_to = "N") %>%
    mutate(age = as.numeric(sub("^N\\.([[:alpha:]]+)Age([[:digit:]]+)$",
                                "\\2", Sex.Age)),
           sex =  sub("^N\\.([[:alpha:]]+)Age([[:digit:]]+)$",
                      "\\1", Sex.Age)) %>%
    select(-Sex.Age) %>%
    full_join(meanW.catchCat %>%
              pivot_longer(cols = starts_with("W.UndeterminedAge"),
                           names_to = "Sex.Age", values_to = "meanW") %>%
              mutate(age = as.numeric(sub("^W\\.([[:alpha:]]+)Age([[:digit:]]+)$",
                                          "\\2", Sex.Age)),
                     sex =  sub("^W\\.([[:alpha:]]+)Age([[:digit:]]+)$",
                                "\\1", Sex.Age)) %>%
              select(-Sex.Age))

## Aggregation of plus-group:
catchCat.long.plus <- catchCat.long %>%
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
    bind_rows(catchCat.long %>%
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
