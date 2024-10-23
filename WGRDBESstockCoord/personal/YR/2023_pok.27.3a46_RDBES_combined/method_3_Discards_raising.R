#-*- coding: utf-8 -*-

### File: method_3_Discards_raising.R
### Time-stamp: <2023-10-04 13:19:06 a23579>
###
### Created: 03/10/2023	15:44:59
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
###
####################################################################################################


## Test:
if (nrow(caton %>%
         filter(DrGroup %in% "Missing")) > 0)
{
    stop("Missing discard raising groups!")
}

## Columns for merging landings and discards per startum:
mergeCol <- c("Country", "Season", "Season.type", "Year", "Stock", "Area", "Fleets", # Original
              "gear", "target", "mesh", "Area1", "gear_target", "FleetType", "DrGroup") # inferred

## Merged landing/discards per startum (to estimate ratios and fill in gaps):
catonLD <- caton %>%
    filter(Catch.Cat. %in% c("Landings")) %>%
    ## Join reported discards to reported landings of the same stratum:
    full_join(caton %>%
              filter(Catch.Cat. %in% c("Discards")) %>%
              select(one_of(c(mergeCol, "Catch..kg"))) %>%
              rename(Catch..kgD = Catch..kg)) %>% # specific col for discard weight.
    mutate(Dimported = ! is.na(Catch..kgD), # Keep tracks of imported/raised rows.
           ## Discards SCO G5-9 not used for raising.
           ## Can also add a threshold here:
           Catch..kgD = if_else(DrGroup %in% c("G5", "G6", "G7", "G8", "G9"
                                               ) &
                                 Country %in% "UK(Scotland)",
                                NA_real_, Catch..kgD)) # (NA ratio disregarded in following w.mean)

if (nrow(catonLD %>% filter(is.na(Catch..kg))))
{
    warning("There are ", nrow(catonLD %>% filter(is.na(Catch..kg))),
             " discard records without corresponding landings")
}

## Raised discards:
catonLDr <- catonLD %>%
    group_by(DrGroup) %>% # mutate will use non-vectorized operations
                          # (sum, weighted.mean,...) group-wise.
    mutate(Catch..kgD = if_else(Dimported,
                                Catch..kgD,
                                ## ## summing more conservative when disc. but zero landing...
                                ## ## ...but not the IC way!
                                ## Catch..kg * sum(x = Catch..kgD, na.rm = TRUE) /
                                ##   sum(x = if_else(is.na(Catch..kgD), NA_real_, Catch..kg),
                                ##       na.rm = TRUE)),
                                Catch..kg * weighted.mean(x = Catch..kgD / Catch..kg,
                                                          w = Catch..kg, na.rm = TRUE)),
           ## Calculate back the ratio (to check consistency):
           ratio = Catch..kgD / Catch..kg)


## Re-assemble reported and raised data
catonR <- catonLDr %>%
    filter( ! Dimported) %>%
    ungroup() %>%
    mutate(Catch.Cat. = "Discards",
           Discards.Imported.Or.Raised = "Raised",
           Catch..kg = Catch..kgD) %>%
    select(all_of(colnames(caton))) %>%
    bind_rows(caton)







### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
