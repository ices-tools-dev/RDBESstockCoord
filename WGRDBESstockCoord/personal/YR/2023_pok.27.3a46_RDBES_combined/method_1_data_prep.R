#-*- coding: utf-8 -*-

### File: method_data_prep.R
### Time-stamp: <2023-10-03 13:47:53 a23579>
###
### Created: 03/10/2023	13:44:38
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
###
####################################################################################################

## Extraction of information from the métier:
caton <- caton %>%
    mutate(gear = gsub("^(([^_]+)_([^_]+))_([^_]+)_.*$", "\\2", Fleets),
           target = gsub("^(([^_]+)_([^_]+))_([^_]+)_.*$", "\\3", Fleets),
           gear_target = gsub("^(([^_]+)_([^_]+))_([^_]+)_.*$", "\\1", Fleets),
           mesh = gsub("^(([^_]+)_([^_]+))_([^_]+)_.*$", "\\4", Fleets),
           Area1 = gsub("27\\.([[:digit:]]+)(\\..*)?", "\\1", Area),
           ## TR1 def.:
           FleetType = case_when((gear_target %in% c("OTB_DEF", "OTT_DEF") |
                                  gear %in% c("SDN", "SSC", "PTB")) &
                                 mesh %in% c(">=120", ">=220", "100-119", "120-219") ~ "TR1",
                                 TRUE ~ "Other"))

numbers <- numbers %>%
    mutate(gear = gsub("^(([^_]+)_([^_]+))_([^_]+)_.*$", "\\2", Fleets),
           target = gsub("^(([^_]+)_([^_]+))_([^_]+)_.*$", "\\3", Fleets),
           gear_target = gsub("^(([^_]+)_([^_]+))_([^_]+)_.*$", "\\1", Fleets),
           mesh = gsub("^(([^_]+)_([^_]+))_([^_]+)_.*$", "\\4", Fleets),
           Area1 = gsub("27\\.([[:digit:]]+)(\\..*)?", "\\1", Area),
           ## TR1 def.:
           FleetType = case_when((gear_target %in% c("OTB_DEF", "OTT_DEF") |
                                  gear %in% c("SDN", "SSC", "PTB")) &
                                 mesh %in% c(">=120", ">=220", "100-119", "120-219") ~ "TR1",
                                 TRUE ~ "Other"))

weights <- weights %>%
    mutate(Catch.Cat. = substr(x = Catch.Cat., 1, 1)) %>%
    mutate(gear = gsub("^(([^_]+)_([^_]+))_([^_]+)_.*$", "\\2", Fleets),
           target = gsub("^(([^_]+)_([^_]+))_([^_]+)_.*$", "\\3", Fleets),
           gear_target = gsub("^(([^_]+)_([^_]+))_([^_]+)_.*$", "\\1", Fleets),
           mesh = gsub("^(([^_]+)_([^_]+))_([^_]+)_.*$", "\\4", Fleets),
           Area1 = gsub("27\\.([[:digit:]]+)(\\..*)?", "\\1", Area),
           ## TR1 def.:
           FleetType = case_when((gear_target %in% c("OTB_DEF", "OTT_DEF") |
                                  gear %in% c("SDN", "SSC", "PTB")) &
                                 mesh %in% c(">=120", ">=220", "100-119", "120-219") ~ "TR1",
                                 TRUE ~ "Other"))


### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
