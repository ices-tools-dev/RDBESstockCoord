setwd("D:/OneDrive - International Council for the Exploration of the Sea (ICES)/Profile/Desktop/Maria_ICES_dataflows/IC_RCEF_Fleets")

library(readxl)
library(dplyr)
library(stringr)

IC_fleet <- read_excel("2025_IC_Used_fleets_metiers.xlsx", sheet = "Fleets by stock and WG")
IC_fleet <- read.csv("ICfleetName2015to2025.csv")

library(icesVocab)

IC_FleetName <- icesVocab::getCodeList('IC_FleetName')

Metier3<- icesVocab::getCodeList('Gear')
Metier4<- icesVocab::getCodeList('GearType')
Metier5<- icesVocab::getCodeList('Metier5_FishingActivity')
Metier6<- icesVocab::getCodeList('Metier6_FishingActivity')
Metier7<- icesVocab::getCodeList('NationalFishingActivity')

Area<- icesVocab::getCodeList('ICES_Area')
ASFIS<- icesVocab::getCodeList('SpecASFIS')

WG<- icesVocab::getCodeList('ExpertGroups')

all_metier_codes <- c(
  Metier3$Key,
  Metier4$Key,
  Metier5$Key,
  Metier6$Key,
  Metier7$Key
)


remaining <- IC_fleet %>%
  filter(!(fleetname %in% all_metier_codes))

dis <- unique(remaining$fleetname)


#IC_fleet_clean <- remaining %>%
 # filter(!str_detect(Key, "$_all"))

agg_by_fleet <- remaining %>%
  group_by(fleetname) %>%
  summarise(
    fleetnamedesc = paste(
      sort(unique(str_trim(fleetnameDesc[!is.na(fleetnameDesc)]))),
      collapse = ", "
    ),
    Codes = paste(
      sort(unique(str_trim(Code[!is.na(Code)]))),
      collapse = ", "
    ),
    emails = paste(
      sort(unique(str_trim(Email[!is.na(Email)]))),
      collapse = ", "
    ),
    stockCoordinator = paste(
      sort(unique(str_trim(StockCoordinator[!is.na(StockCoordinator)]))),
      collapse = ", "
    ),
    Stock = paste(
      sort(unique(str_trim(Stock[!is.na(Stock)]))),
      collapse = ", "
    ),
    .groups = "drop"
  ) %>%
  
  arrange(fleetname)



agg_by_fleet_rest<-agg_by_fleet %>%
  mutate(
    agg_by_fleet1 =
      (str_detect(fleetname, "[A-Za-z]") & !str_detect(fleetname, "_")) |
      str_detect(fleetname, "[A-Za-z]+[ -][A-Za-z]+") |
      str_detect(fleetname, "[a-z][A-Z]")
  ) %>%
  filter(agg_by_fleet1)

agg_by_fleet_metiers <- agg_by_fleet %>%
  anti_join(agg_by_fleet_rest, by = "fleetname")


write.csv(agg_by_fleet, 'agg_by_fleet.csv')
write.csv(agg_by_fleet_metiers, 'agg_by_fleet_metiers.csv')
write.csv(agg_by_fleet_rest, 'agg_by_fleet_rest.csv')

write.csv(all_metier_codes, 'all_metier_codes.csv')


