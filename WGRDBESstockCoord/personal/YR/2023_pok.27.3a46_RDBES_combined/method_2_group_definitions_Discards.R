#-*- coding: utf-8 -*-

### File: method_2_group_definitions.R
### Time-stamp: <2023-10-04 15:01:12 a23579>
###
### Created: 03/10/2023	13:54:27
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
###
####################################################################################################

## ###########################################################################
## Raising discards:

mainCo <- c("France", "Norway", "Germany") # for raising groups.

caton <- caton %>%
    ## Groups for raising discards:
    mutate(DrGroup =
               case_when(
                   ## TR1, main countries (all areas, per quarter):
                   Country %in% mainCo & FleetType == "TR1" & Season == 1 ~ "G1",
                   Country %in% mainCo & FleetType == "TR1" & Season == 2 ~ "G2",
                   Country %in% mainCo & FleetType == "TR1" & Season == 3 ~ "G3",
                   Country %in% mainCo & FleetType == "TR1" & Season == 4 ~ "G4",
                   ## ===========================================================
                   ## ! for groups 5 to 9, exclusion of discards from SCO
                   ##   for raising in the next script (method_3_...).
                   ##   Necessity to think to a more flexible way to match
                   ##   entries in the future.
                   ## ===========================================================
                   ## Other métiers, all seasons, areas 46:
                   FleetType == "Other" & Area1 %in% c("4", "6") ~ "G5",
                   ## TR1 other countries, area 46 (per quarter or year):
                   (! Country %in% mainCo) & FleetType == "TR1" & Area1 %in% c("4", "6") & Season == 1 ~ "G6",
                   (! Country %in% mainCo) & FleetType == "TR1" & Area1 %in% c("4", "6") & Season == 2 ~ "G7",
                   (! Country %in% mainCo) & FleetType == "TR1" & Area1 %in% c("4", "6") & Season == 3 ~ "G8",
                   (! Country %in% mainCo) & FleetType == "TR1" & Area1 %in% c("4", "6") & Season == 4 ~ "G9",
                   (! Country %in% mainCo) & FleetType == "TR1" & Area1 %in% c("4", "6") & Season == 2022 ~ "G10",
                   ## TR1 other countries, area 3 (per quarter):
                   (! Country %in% mainCo) & FleetType == "TR1" & Area1 %in% c("3") & Season == 1 ~ "G11",
                   (! Country %in% mainCo) & FleetType == "TR1" & Area1 %in% c("3") & Season == 2 ~ "G12",
                   (! Country %in% mainCo) & FleetType == "TR1" & Area1 %in% c("3") & Season == 3 ~ "G13",
                   (! Country %in% mainCo) & FleetType == "TR1" & Area1 %in% c("3") & Season == 4 ~ "G14",
                   ## Other métiers, areas 3 (per quarter):
                   FleetType == "Other" & Area1 %in% c("3") & Season == 1 ~ "G15",
                   FleetType == "Other" & Area1 %in% c("3") & Season == 2 ~ "G16",
                   FleetType == "Other" & Area1 %in% c("3") & Season == 3 ~ "G17",
                   FleetType == "Other" & Area1 %in% c("3") & Season == 4 ~ "G18",
                   ## If missing:
                   TRUE ~ "Missing"))

if (any("Missing" %in% caton$DrGroup))
{
    warning("There are ", sum(caton$DrGroup %in% "Missing"),
            " entries with missing group information.\n",
            "\tDiscards raising will not be carried out for those.")

    print(caton %>%
          select(DrGroup, Catch..kg, Catch.Cat., Country, FleetType, Area1, Season) %>%
          filter(DrGroup %in% "Missing"))
}







### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
