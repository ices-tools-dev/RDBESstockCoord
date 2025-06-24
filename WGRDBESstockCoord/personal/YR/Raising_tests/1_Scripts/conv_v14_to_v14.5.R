#-*- coding: utf-8 -*-

### File: conv_v14.5.R
### Time-stamp: <2025-06-24 15:44:46 a23579>
###
### Created: 23/06/2025	15:35:13
### Author: Yves Reecht
###
####################################################################################################
### Description:
### 
### Conversion from v14, with correction of misallocated domain for BMS
####################################################################################################



library(dplyr)
library(tidyr)
library(readr)
library(rlang)
## library(lubridate)

scriptDir <- "./1_Scripts"
dataDir <- "./2_Data"
resDir <- "./3_Results"

## Set appropriate WD, based on some usual EDI R-opening behaviours:
if (basename(getwd()) == "1_Scripts")
    setwd("..")

if (dir.exists("./WGRDBESstockCoord/personal/YR/Raising_tests"))
    setwd("./WGRDBESstockCoord/personal/YR/Raising_tests")

## The raising functions:
source(file.path(scriptDir, "0_Functions.R"))
source(file.path(scriptDir, "0_Functions_discards_raising.R"))

census <- read_csv(file.path(dataDir, "pok_2022_census_catches.csv"))

catch_estimates <- read_csv(file.path(dataDir, "pok_2022_estimated_catches.csv"))

distributions <- read_csv(file.path(dataDir, "pok_2022_distributions.csv"))

names(distributions)
names(catch_estimates)

names(census)

## Conversion fields to type+value, +various data wrangling in census:
censusExt <- census %>%
    mutate(domainCatch = ifelse(is.na(domainCatchDis),
                                NA_character_,
                                paste(vesselFlagCountry, domainCatchDis, sep = "_")),
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
           ## Moving domainCatchDis -> domainCatchBMS for BMS only:
           domainCatchBMS = ifelse(catchCategory %in% "BMS" &
                                   is.na(domainCatchBMS) &
                                   ! is.na(domainCatchDis),
                                   domainCatchDis, domainCatchBMS),
           domainCatchDis = ifelse(catchCategory %in% "BMS",
                                   NA, domainCatchDis)) %>%
    select(vesselFlagCountry:catchCategory,
           seasonType, seasonValue,
           areaType, areaValue,
           fisheriesManagementUnit, metier6,
           fleetType, fleetValue,
           domainCatchDis:domainCatch)


censusExt %>%
    ## group_by(seasonType) %>%
    group_by(catchCategory) %>%
    slice_sample(n = 1) %>% as.data.frame()

## Split discards and BMS catch estimates:
estDIS <- catch_estimates %>%
    filter(catchCategory == "DIS") %>%
    mutate(domainCatchDis = domainCatch,
           domainCatch = ifelse(is.na(domainCatch),
                                NA_character_,
                                paste(vesselFlagCountry, domainCatchDis, sep = "_")),
           variableType = sub("Scientific", "", variableType))

estBMS <- catch_estimates %>%
    filter(catchCategory == "BMS") %>%
    mutate(domainCatchBMS = domainCatch,
           domainCatch = ifelse(is.na(domainCatch),
                                NA_character_,
                                paste(vesselFlagCountry, domainCatchBMS, sep = "_")),
           variableType = sub("Scientific", "", variableType))
 

estDIS %>%
    slice_sample(n = 2) %>% as.data.frame()

## Valid domain(Dis|BMS) from estimated fraction:
domainsDIS <- unique(na.omit(estDIS$domainCatch))
domainsBMS <- unique(na.omit(estBMS$domainCatch))

## Correction domain(Dis|BMS) for landings:
censusExt <- censusExt %>%
    mutate(domainCatchBMS = ifelse(catchCategory == "LAN" &
                                   domainCatch %in% domainsBMS,
                                   domainCatchDis, domainCatchBMS),
           domainCatchDis = ifelse(catchCategory == "LAN" &
                                   ! domainCatch %in% domainsDIS,
                                   NA, domainCatchDis))


censusExt %>%
    ## group_by(seasonType) %>%
    group_by(catchCategory) %>%
    slice_sample(n = 1) %>% as.data.frame()

## Assembling census and estiamtes in one unique table:
catches <- censusExt %>%
    filter(catchCategory == "LAN") %>%
    bind_rows(censusExt %>%
              select(-total) %>%
              right_join(estDIS)) %>%
    bind_rows(censusExt %>%
              select(-total) %>%
              right_join(estBMS))

catches %>%
    filter(! is.na(domainCatch)) %>%
    group_by(catchCategory) %>%
    slice_sample(n = 2) %>%
    select(-comment, -domainCatch, comment) %>%
    as.data.frame()

catches %>%
    write_csv(file.path(dataDir, "pok_2022_catches_v14.5.csv"),
              na = "")


### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
