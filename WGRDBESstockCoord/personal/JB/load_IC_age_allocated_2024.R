df_canum_ic <- readRDS(
        file = glue("{path_to_data}/{data_year}/IC_age_allocation/df_canum.rds")) %>%
  rename(vesselFlagCountry = Country,
         seasonValue =  Season,
         seasonType = SeasonType,
         year =  Year,
         catchCategory = CatchCat,
         stock=  Stock,
         areaValue =   Area,
         fleetValue =  Fleets) %>%
    mutate(origin = "IC",
           year = as.character(year),
           seasonValue =  as.character(seasonValue),
           catchCategory = case_when(catchCategory == "Landings" ~ "Lan",
                                     catchCategory == "Discards" ~ "Dis") )

dfs_canum_allocated_ic <- df_canum_ic %>%
  filter(!Source == "Imported") %>%
  group_by(year, fleetValue, catchCategory, Source, origin) %>%
  summarise(numbers = sum(numbers))

dfs_canum_ic <- readRDS(
        file = glue("{path_to_data}/{data_year}/IC_age_allocation/dfs_canum.rds")) %>%
  rename(vesselFlagCountry = Country,
         seasonValue =  Season,
         year =  Year,
         fleetValue =  Fleets) %>%
  mutate(origin = "IC",
         year = as.character(year),
         seasonValue =  as.character(seasonValue))

dfs_canum_ageplus_ic <- readRDS(
        file = glue("{path_to_data}/{data_year}/IC_age_allocation/dfs_canum_ageplus.rds")) %>%
  rename(vesselFlagCountry = Country,
         seasonValue =  Season,
         year =  Year,
         fleetValue =  Fleets) %>%
  mutate(origin = "IC",
         year = as.character(year),
         seasonValue =  as.character(seasonValue))

dfs_canum_ageplus_allocation_ic <- readRDS(
        file = glue("{path_to_data}/{data_year}/IC_age_allocation/dfs_canum_ageplus_allocation.rds")) %>%
  rename(year =  Year) %>%
  mutate(origin = "IC",
         year = as.character(year))

### Merge IC and CEF age allocation

df_canum_comp <- df_canum %>%
  mutate(origin = "CEF",
         Age = as.character(Age)) %>%
  bind_rows(., df_canum_ic)

dfs_canum_comp <- dfs_canum %>%
  mutate(origin = "CEF",
         Age = as.character(Age)) %>%
  bind_rows(., dfs_canum_ic)

dfs_canum_ageplus_comp <- dfs_canum_ageplus %>%
  mutate(origin = "CEF")  %>%
  bind_rows(., dfs_canum_ageplus_ic)

dfs_canum_ageplus_allocation_comp <- dfs_canum_ageplus_allocation %>%
  mutate(origin = "CEF")  %>%
  bind_rows(., dfs_canum_ageplus_allocation_ic)

dfs_canum_allocated_comp <- dfs_canum_allocated %>%
  mutate(origin = "CEF") %>%
  bind_rows(.,dfs_canum_allocated_ic)

