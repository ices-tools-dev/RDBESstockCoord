dfw_stock_overview_raised_w_2024_IC <- read_rds(file =  here("WGRDBESstockCoord/personal/JB/data/df_stock_overview_raised_w_2024_IC.rds"))


dfw_stock_overview_raised_w_2024_IC <- dfw_stock_overview_raised_w_2024_IC %>%
  dplyr::select(-dplyr::starts_with("stratum_")) %>%
  rename(vesselFlagCountry = Country,
         seasonValue =  Season,
         seasonType = SeasonType,
         year =  Year,
         stock=  Stock,
         areaValue =   Area,
         fleetValue =  Fleets,
         importedOrRaised =  DiscardsImportedOrRaised,
         Lan = Landings_Catchkg,
         Dis = Discards_Catchkg) %>%
  mutate(year = as.character(year),
         raising_type = "IC")

df_stock_overview_raised_w_2024_IC <- pivot_longer(dfw_stock_overview_raised_w_2024_IC,
                                                   cols = c("Lan", "Dis"),
                                                   names_to = "catchCategory",
                                                   values_to = "Catch_kg")

