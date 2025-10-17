##############################################
rm(list = ls())
# Required Libraries
# Vector containing the desired packages
packs <- c(
  "dplyr", "data.table", "janitor", "readr", "readxl", "lubridate", 
  "stringr", "icesVocab", "icesSD", "tidyr", "FSA", "collapse", "icesSD"
)

# Install missing packages and load all of them
invisible(lapply(packs, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}))

# --- RDBES & ICES stock related tables setup ---

###********************************** Stocklist & Stock-Area-Code Assignment ****************************
###*


# --- 1. Download and filter 2024 Stock Data (from ICES Stock Database) ---
y2024 <- getSD(year = 2024) %>%
  select(
    ActiveYear,
    ExpertGroup,
    Stock = StockKeyLabel,
    SpeciesName = SpeciesScientificName
  ) %>%
  filter(Stock %in% c("pil.27.8c9a", "mac.27.nea")) %>%
  distinct()  # remove duplicates

head (y2024)
# --- 2. Load and clean Stock List by ICES Area ---
StockListbyArea <- read.csv(
  "boot/initial/data/StockAssessmentGraphs_2025.csv",
  header = TRUE, sep = ";", encoding = "UTF-8"
) %>%
  filter(Year == 2024) %>%
  transmute(  # transmute is faster than select() + rename()
    Stock = FishStock,
    SpeciesName,
    # Clean up ICES Area formatting (replacing '~' with ',' and removing whitespace)
    ICESArea = gsub("~", ",", trimws(ICES.Areas..splited.with.character.....))
  ) %>%
  distinct()

# --- 3. FAO Codes Auxiliary Table ---
codes_FAO <- getCodeList("SpecASFIS") %>%
  transmute(
    cod.FAO = Key,
    SpeciesName = Description,
    SpeciesCode = Id
  ) %>%
  distinct()

# --- 4. Final Stock Metadata Table (Union of all sources) ---
StockListbyEG <- y2024 %>%
  left_join(StockListbyArea, by = c("Stock", "SpeciesName")) %>%
  left_join(codes_FAO, by = "SpeciesName") %>%
  mutate(
    AphiaID = SpeciesCode
  ) %>%
  # Final filter to ensure only target stocks/species are kept
  filter(
    cod.FAO %in% c("MAC", "PIL"),
    Stock %in% c("mac.27.nea", "pil.27.8c9a")
  ) %>%
  distinct(Stock, .keep_all = TRUE) %>% # Keep one unique row per Stock
  select( Year=ActiveYear,ExpertGroup,
          Stock, SpeciesName, cod.FAO, AphiaID,ICESArea
  )

head(StockListbyEG)


# --- Reading RDBES Data Files (Matrices) ---

# SA (Sample) Table: Sampling activity details, total/sample weight
SA <- fread("boot/initial/data/Sample.csv", quote = "", encoding = "Latin-1") %>%
  fselect(SAid, SSid, SamplingScheme=SAcommSizeCatScale,  CatchCategory = SAcatchCategory, speciesCode = SAspeciesCode, SAspeciesCodeFAO, SAsex, SAcommSizeCat, 
          SAtotalWeightLive, SAsampleWeightLive) %>% 
  filter(!is.na( SAtotalWeightLive) &   SAsampleWeightLive>0 ) %>% 
  fmutate(speciesCode = as.character(speciesCode)); headtail (SA)

# FM (Frequency Measure) Table: Length/frequency counts
FM <- fread("boot/initial/data/FrequencyMeasure.csv", quote = "", encoding = "Latin-1") %>%
  fselect(FMid, SAid, FMclassMeasured, FMnumberAtUnit, FMtypeMeasured, FMtypeAssessment); head (FM)

# SS (Species Selection) Table: Links SA to LE (Sampling Event)
SS <- fread("boot/initial/data/SpeciesSelection.csv", quote = "", encoding = "Latin-1") %>%
  fselect(SSid, LEid, SLid, SSsampled,SSspeciesListName) %>%
  # filter(SLid %in% c(207,523))%>%
  arrange(SSspeciesListName); head (SS)#

# LE (Landing Event) Table: Trip characteristics, area, metier, vessel
LE <- fread("boot/initial/data/LandingEvent.csv", quote = "", encoding = "Latin-1") %>%
  fselect(Country= LEcountry,LEid, OSid,  LEmetier6, LEarea,LEunitName, LEdate,LElocode ,LEencryptedVesselCode, LElocationType,
          LEfisheriesManagementUnit) %>% arrange(desc(LEfisheriesManagementUnit)); head (LE)

# --- Reading Auxiliary Parameters Table ---
# Contains a & b parameters for length-weight conversion and FMU definitions
PARAMETERS<- fread("boot/initial/data/MAC_PIL_Parameters.csv", encoding = "Latin-1") %>%
  select(
    Year = ActiveYear, ExpertGroup, Stock = StockKeyLabel,  Q,cod.FAO,
    SpeciesName, speciesCode, FMU = Fisheries_management_unit, area, a, b
  ) %>%
  
  filter(!is.na(a) & a != "") %>%
  filter(cod.FAO %in% c('PIL', 'MAC')) %>% 
  # Conditional area assignment for PIL (using FMU for specific areas)
  mutate(area = ifelse(cod.FAO== "PIL" & area %in% c("27.8.c", "27.9.a"), FMU, area)  ,
         speciesCode = as.character(speciesCode)
  ) %>%
  
  mutate(Country="ES") %>% 
  select(-FMU) %>%
  unique()

headtail(PARAMETERS)


# --- 1. Initial Processing and Raising of Sampling Data (Samp) ---
# Calls a function to join SA, FM, SS, LE and calculate meanweight (using a/b parameters)
source("boot/initial/utilities/funciones/procesar_Samp.R")
# Note: muestreos_FMU is not defined here, assuming it's passed/loaded within the function or environment
Samp <- procesar_Samp(SA, FM, SS, LE, PARAMETERS, muestreos_FMU) %>% filter(unitName!="Trip")

headtail(Samp)

# --- 2. Calculate Length-Specific Counts (N & Npond) ---
Samp_Lengths <- Samp %>%
  filter(!is.na(length), !is.na(FMnumberAtUnit)) %>%
  left_join(
    Samp %>%
      select(SAid, totalWeightLive = SAtotalWeightLive, sampleWeightLive = SAsampleWeightLive) %>%
      distinct(),
    by = "SAid"
  ) %>%
  transmute(
    Country,
    ExpertGroup ,
    Stock ,
    SLid,
    SamplingScheme,
    unitName,
    date,
    SAid,
    metier6,
    Q,
    area,
    CatchCategory,
    cod.FAO,
    SpeciesName,
    speciesCode,
    SAcommSizeCat,
    totalWeightLive = totalWeightLive / 1000, # Convert to tonnes (assuming original unit is kg)
    sampleWeightLive = sampleWeightLive / 1000, # Convert to tonnes
    meanweight,
    length = length/ 10, # Convert length to cm (assuming original unit is mm)
    N = FMnumberAtUnit,
    # Npond: Number of individuals raised (weighted) to the total trip weight
    Npond = round((totalWeightLive * N) / sampleWeightLive, 2)
  ) %>%
  group_by(
    Country,SLid, SamplingScheme, ExpertGroup , Stock , date,unitName, SAid, metier6, Q, area, CatchCategory,cod.FAO, SpeciesName,
    speciesCode, SAcommSizeCat, totalWeightLive, sampleWeightLive, meanweight, length
  ) %>%
  summarise(
    N = sum(N),
    Npond = sum(Npond)
  )

# --- 3. Calculate Sampling Effort Metrics ---
Samp_Lengths <-Samp_Lengths %>% group_by(metier6, Q ,  area, CatchCategory ,cod.FAO) %>% 
  mutate(
    NumSamplesLngt= n_distinct(unitName),  # Number of sampled trips/units with length measurements
    num_PSU = n_distinct(date),            # Number of unique primary sampling units (dates)
    numLngtMeas =sum(N)                    # Total number of individuals measured
  )


# --- 4. Aggregate Sample Weights by MQD (Metier-Quarter-Division) ---
Samp_mix_MQD<- Samp_Lengths %>%
  distinct() %>%
  mutate(Weight_sample_length = N * meanweight) %>%
  group_by(metier6, Q, area, CatchCategory,cod.FAO, SpeciesName, speciesCode, NumSamplesLngt ,num_PSU,     numLngtMeas) %>%
  # Calculate the total sampled weight for the MQD stratum
  summarise(SampleWeightLive = sum(Weight_sample_length, na.rm = TRUE), .groups = "drop") %>%
  # Join back the length-specific counts (N_length, N_Pond_length)
  left_join(
    Samp_Lengths %>%
      distinct() %>%
      group_by(ExpertGroup, Stock, metier6, Q, area, CatchCategory,cod.FAO, speciesCode, meanweight,
               length,, NumSamplesLngt ,num_PSU,     numLngtMeas) %>%
      summarise(
        N_length = sum(N, na.rm = TRUE),
        N_Pond_length = sum(Npond, na.rm = TRUE)
      )
  ) %>% filter(!(cod.FAO == "PIL" & area == "27.8.b")) %>% 
  select(ExpertGroup, Stock, metier6, Q, area,CatchCategory, cod.FAO, SampleWeightLive,
         meanweight, length, N_length, N_Pond_length, NumSamplesLngt ,num_PSU,     numLngtMeas) %>%
  
  ungroup() %>%
  as.data.frame()
headtail(Samp_mix_MQD)


# --- Reading and Processing Commercial Landings (CL) ---

# CL_all: Commercial Landing data (official catches)
CL_all <-fread ("boot/initial/data/CommercialLanding_encrypted.csv") %>%
  fselect(
    Country= CLlandingCountry, CLid, year = CLyear, metier6 = CLmetier6, Q = CLquarter, area = CLarea,  
    CLspeciesCode, statisticalRectangle = CLstatisticalRectangle, landingLocation = CLlandingLocation,
    CLspeciesFaoCode, CatchCategory = CLcatchCategory, CLnationalFishingActivity, CLsamplingScheme,
    CLWeight = CLofficialWeight
  ) %>%  
  # Filter only for MAC/PIL species codes and ES country
  mutate(speciesCode = as.character(CLspeciesCode)) %>%
  filter(!is.na(CLid)  &  CLspeciesCode %in% c('127023', '126421') & Country== "ES")   %>%
  mutate(cod.FAO= ifelse(speciesCode==127023, 'MAC', 'PIL')) %>% 
  filter(!(cod.FAO == "PIL" & area == "27.8.b")) %>% 
  select(-CLspeciesCode,-CLspeciesFaoCode)
headtail(CL_all)
source("boot/initial/utilities/funciones/assign_fmu.R") # Function to assign FMU to landings
CL_all <- assign_fmu(CL_all, PARAMETERS)

tabyl(CL_all, CatchCategory)      
headtail(CL_all)
# AGGREGATE Landings by MQD (Metier-Quarter-Division)

CL_ES_MQD <- CL_all %>%
  group_by(Country,ExpertGroup, Stock,metier6, Q, area, CatchCategory, speciesCode,cod.FAO) %>%
  summarise(
    officialWeight = sum(CLWeight, na.rm = TRUE),
    .groups = "drop"
  ) %>%arrange(metier6) %>%  as.data.frame()


# --- JOIN Samples (MQD) with Landings (MQD) ---
# The join adds the 'officialWeight' to the sampling table
Samp_mix_CLMQD <- full_join(
  Samp_mix_MQD,
  CL_ES_MQD,relationship = "many-to-many") %>% 
  unique() %>%  as.data.frame() %>% arrange(Country)

headtail(Samp_mix_CLMQD)

# --- Calculate NumberCaught (Number of individuals raised to official catches) ---
# This is the core raising step: (N_Pond_length / SampleWeightLive) * officialWeight / meanweight
source("boot/initial/utilities/funciones/calc_numbercaught.R")
Samp_mix_CLMQD2 <- Samp_mix_CLMQD %>%
  calc_numbercaught() %>%
  select(Country, ExpertGroup, Stock, metier6, Q, area, CatchCategory, 
         speciesCode,cod.FAO,
         SampleWeightLive, meanweight, length, N_Pond_length,numLngtMeas,
         NumSamplesLngt, num_PSU,
         officialWeight, numbercaught,) %>%
  distinct() %>%
  # Re-join to count samples (the previous count was incorrect)
  left_join(
    Samp_Lengths %>%
      count(Country, metier6, Q, CatchCategory, area, cod.FAO, name = "NumSamplesLngt") %>%
      distinct()
  ) %>% mutate(
    Country = "ES",
    officialWeight = ifelse(is.na(officialWeight), 0, officialWeight),
    numbercaught = ifelse(is.na(numbercaught), 0, numbercaught),
    # Ensure speciesCode is present even if numbercaught was 0
    speciesCode = case_when(
      is.na(speciesCode) & cod.FAO == "MAC" ~ "127023",
      is.na(speciesCode) & cod.FAO == "PIL" ~ "126421",
      TRUE ~ speciesCode
    )
  ) %>%
  as.data.frame()


headtail(Samp_mix_CLMQD2)

# --- Reading and Processing Commercial Effort (CE) ---


CE <- fread("boot/initial/data/CommercialEffort.csv") %>%
  fselect(-CEencryptedVesselIds) %>%
  rename(
    statisticalRectangle = CEstatisticalRectangle,
    area = CEarea,
    landingLocation = CElandingLocation
  ) %>%
  filter(area %in% c("27.6.a", "27.6.b", "27.7.a", "27.7.b", "27.7.c", "27.7.g", 
                     "27.7.h", "27.7.j", "27.7.k", "27.8.a", "27.8.b", "27.8.c", 
                     "27.8.d", "27.9.a")) 
source("boot/initial/utilities/funciones/asignar_FMU.R")  # Function to assign FMU to effort (assuming this
# Apply Fishery Management Unit (FMU) assignment
CE <- CE %>%
  mutate(fisheriesManagementUnit = asignar_fisheriesManagementUnit(statisticalRectangle, area)) %>%   
  fselect(
    Country = CEvesselFlagCountry,
    CEid,
    year = CEyear,
    CErecordType,
    Q = CEquarter,
    area,
    FMU=fisheriesManagementUnit,
    metier6 = CEmetier6,
    landingLocation,
    statisticalRectangle,
    CEofficialkWFishingDays
  ) 



# --- Prepare Stock/Area Key for Conditional Effort Join ---
stockfmu<-Samp_mix_CLMQD[,c('area', 'Stock', 'ExpertGroup', "cod.FAO")] %>%
  filter(!is.na(ExpertGroup) &   Stock %in% c("pil.27.8c9a", "mac.27.nea"  )) %>% 
  arrange(ExpertGroup, Stock) %>% 
  unique()
headtail(stockfmu,2)

# Aggregate Effort by MQD (Metier-Quarter-Division/FMU)
CE_MQD<- CE %>%
  group_by(year,Country,Q , area, FMU,metier6) %>%
  summarise(Effort = sum(CEofficialkWFishingDays, na.rm = TRUE))


head (CE_MQD)
head (stockfmu)

# Conditional Join: MAC (join on area) vs PIL (join on FMU)
CE_MQD2 <- bind_rows(
  # MAC join: Uses 'area' from stockfmu to match 'area' in CE_MQD
  CE_MQD %>%
    left_join(stockfmu %>% filter(Stock == "mac.27.nea"), by = c("area" = "area")),
  # PIL join: Uses 'area' from stockfmu to match 'FMU' in CE_MQD
  CE_MQD %>%
    left_join(stockfmu %>% filter(Stock == "pil.27.8c9a"), by = c("FMU" = "area"))
) %>% unique() %>% 
  # Select the correct area/FMU for the final grouping
  mutate(FishingArea=  ifelse(Stock == "mac.27.nea", area, FMU)) %>%
  group_by(year, Country, Q, area=FishingArea, metier6, Stock, ExpertGroup,
           cod.FAO) %>% summarise(Effort= sum(Effort)) %>%
  
  
  fselect(
    year, Country, Q, area, metier6, Stock, ExpertGroup,
    cod.FAO, Effort
  ) %>%
  filter(!is.na(Stock)) %>% arrange(desc(Effort
  ))


headtail(CE_MQD2,2)


# --- FINAL JOIN: Samples/Landings + Effort ---
# Join aggregated samples/landings (Samp_mix_CLMQD2) with effort data (CE_MQD2)
Samp_mix_CL_CE_MQD <- Samp_mix_CLMQD2 %>%
  left_join(CE_MQD2) %>% unique()

# Assign fishManUnit (FMU) based on stock
Samp_mix_CL_CE_MQD <-Samp_mix_CL_CE_MQD %>%
  mutate(
    fishManUnit = case_when(
      Stock == "mac.27.nea" ~ NA_character_, # MAC stock uses ICES area, not FMU
      Stock == "pil.27.8c9a" ~ area,        # PIL stock uses FMU (which is in the 'area' column after the join)
      TRUE ~ NA_character_
    )
  ) %>%
  relocate(fishManUnit, .after = area) # Move FMU column after area for clarity

headtail(Samp_mix_CL_CE_MQD)


###********************************** Create Census Catches (SI) Table ********************************
###*
# SI: Final landings table structure required for Intercatch/RCEF format
head (Samp_mix_CL_CE_MQD)
colSums(is.na(Samp_mix_CL_CE_MQD))


SI<- Samp_mix_CL_CE_MQD%>% mutate(
  
  Year= year,
  Season= Q,
  SeasonType= "Quarter",
  Species=cod.FAO,
  AreaType ="Div", # Initial Area Type
  FishingArea= area,
  Fleet=metier6,
  CATON= officialWeight # Official Landings Weight
  
) %>% fselect(-SampleWeightLive, -meanweight, -length,-N_Pond_length,-numbercaught, -year) %>% 
  distinct()

headtail( SI)

# Create unique key for Landings (SI) table
SI$key = paste(SI$Country,
               SI$Year,
               SI$Season,
               SI$Species,
               SI$FishingArea,
               SI$Fleet,
               SI$CatchCategory,
               sep = "_")

# SD: Final distributions table structure (derived from aggregated sampling data)
SD<-   Samp_mix_CL_CE_MQD%>% mutate(
  
  Year= 2024,
  Season= Q,
  Species=cod.FAO,
  FishingArea= area,
  Fleet=metier6,
  CANUMtype = "Length",
  ageType ="Age", 
  
  
) %>% filter(!is.na(length) ) %>%
  
  distinct()

# Create unique key for Distributions (SD) table
SD$key = paste(SD$Country,
               SD$Year,
               SD$Season,
               SD$Species,
               SD$FishingArea,
               SD$Fleet,
               SD$CatchCategory,
               sep = "_")

# Clean up SI: remove zero landings and duplicates based on key
SI <- SI[! (SI$CATON == 0 & SI$CatchCategory == "Lan"), ]
SI <- SI[!duplicated(SI$key), ]

# Join Stock information (AphiaID, StockCode)
StockListbyEG$Year<-as.integer(StockListbyEG$Year)
StockListbyEG <- StockListbyEG %>%
  rename(Species = cod.FAO)
SI <- left_join(SI, StockListbyEG)

# Identify domains with both Landings and Discards (has_dis)
SI$key2 <- substr(SI$key, 1, nchar(SI$key)-4)
tbl <- unique(SI[, c("key2", "CatchCategory")])
has_dis <- tbl[duplicated(tbl$key2), "key2"]

# Function to define the estimation domain (by ICES Area or full Stock Area)
define_domain <- function(df, use_stock_area = TRUE) {
  df <- df %>%
    mutate(domain = if (use_stock_area) {
      paste(Season, 'all', Fleet, sep = "_")
    } else {
      # Use FishingArea for domain definition (use_stock_area = FALSE)
      paste(Season, FishingArea, Fleet, sep = "_")
    })
  
  return(df)
}

# Define domain based on FishingArea (per ICES area)
SI<- define_domain(SI, use_stock_area = FALSE)

# Create domain flags for final table
SI$domainCatchDis <- ifelse(SI$key2 %in% has_dis | SI$CatchCategory == "DIS",
                            SI$domain, "") # Domain for Discards
SI$domainBiology <- ifelse(SI$key %in% SD$key,
                           SI$domain, "") # Domain for Biological data (SD table)

SI$quarter <- ifelse(SI$SeasonType == "Quarter", 
                     SI$Season, NA)

CI<-SI[SI$CatchCategory == "Lan", ] # Filter for Landings (Census Catches)

# Construct final CENSUS_CATCHES table
census_catches <- data.frame(VesselFlagCountry = CI$Country,
                             year = CI$Year,
                             workingGroup = CI$ ExpertGroup,
                             stock = CI$Stock,
                             speciesCode = CI$AphiaID,
                             CatchCategory	= CI$CatchCategory,
                             quarter = CI$quarter,
                             AreaType =CI$AreaType,
                             area = CI$FishingArea,
                             fisheriesManagementUnit = NA, # FMU is handled via area/stock logic
                             metier6 =  CI$Fleet,
                             # Fleet Type/Name assignment for MAC (WGFleet) vs PIL (Metier6)
                             fleetType= ifelse(CI$Stock=='mac.27.nea', 'WGFleet',"Metier6" ),
                             fleet = ifelse(CI$Stock=='mac.27.nea',"ES_ALL", CI$Fleet),
                             domainCatchDis = CI$domainCatchDis,
                             domainCatchBMS = "",
                             domainBiology = CI$domainBiology,
                             variableType = "OfficialWeight",
                             total = ifelse(CI$CatchCategory == "Lan",
                                            CI$CATON, NA),
                             variance = "",
                             PSUtype = "fishing_trip",
                             num_PSUs = CI$num_PSU,
                             num_trips= CI$NumSamplesLngt,
                             comment = ""
) %>% distinct()

# Adjust AreaType based on Stock Code
census_catches<-census_catches %>%
  mutate(
    AreaType = case_when(
      stock == "mac.27.nea" ~ "Div",
      stock == "pil.27.8c9a" ~ "SubDiv",
      TRUE ~ AreaType
    )    )

headtail(census_catches)


####################################################################################################
###********************************** Create Estimated Catches (EST) Table *********************************

# est: Estimated catches (Discards) from SI table
est <- SI[SI$CatchCategory != "Lan", ]
est<-filter(est,!is.na(CatchCategory))

# Get number of samples for discards from SD (where CatchCategory != Lan)
xx <- unique(SD[SD$CatchCategory != "Lan", c("NumSamplesLngt", "key")])
est <- merge(est, xx, all.x = T)
est$NumSamplesLngt[is.na(est$NumSamplesLngt)] <- 0
est<-subset(est, !is.na(SpeciesName))

# Construct final ESTIMATED_CATCHES table
estimated_catches <- data.frame(VesselFlagCountry = est$Country,
                                year = est$Year,
                                workingGroup = est$ExpertGroup,
                                stock = est$Stock,
                                speciesCode = est$AphiaID,
                                CatchCategory	= est$CatchCategory,
                                domainCatch = est$domain,
                                variableType = "OfficialWeight",
                                total = est$CATON,
                                mean = NA,
                                varianceTotal = NA,
                                varinaceMean = NA,
                                PSUtype = NA,
                                numPSU = est$NumSamplesLngt,
                                numSamples = est$NumSamplesLngt # Should be numPSU for estimated catches
)
head(estimated_catches)


###################################################
###**************************** Create Length Distribution (SD) Table *****************
# SD: Distribution data (Length, Age, etc.) derived from Samp_mix_CL_CE_MQD
# Clean up SD strings
cols_to_be_rectified <- names(SD)[vapply(SD, is.character, logical(1))]
#It cleans the text data to prevent errors caused by spaces in names, codes, areas, etc
SD[, cols_to_be_rectified] <- lapply(SD[, cols_to_be_rectified], trimws)

# Join Stock Information
SD$cod.FAO<- SD$Species
StockListbyEG$Year<-as.integer(StockListbyEG$Year)
SD<- left_join(SD, StockListbyEG) 

# Recode / Create distribution-specific columns
SD <-SD%>%
  mutate(
    CANUMtype = "Lenght",
    
    AgeLength =length,
    bvType = ifelse(CANUMtype == "age", "Age", "Length"), # Biological Variable Type
    ageType = ifelse(bvType == "Age", "ageyear", ""),
    # Assign correct number of PSUs/Measurements based on bvType
    numPSUs = ifelse(bvType == "Age", NumSamplesAge, NumSamplesLngt),
    numMeasurements = ifelse(bvType == "Age", NumAgeMeas, numLngtMeas)
  )

# Define distribution domain (by ICES Area)
SD<- define_domain(SD, use_stock_area = FALSE)

# Construct final DISTRIBUTIONS table
distributions = data.frame(VesselFlagCountry = SD$Country,
                           year = SD$Year,
                           workingGroup = SD$ExpertGroup,
                           stock = SD$Stock,
                           speciesCode = SD$AphiaID,
                           CatchCategory	= SD$CatchCategory,
                           domainBiology = SD$domain,
                           fishDomain = "",
                           bvType = SD$bvType,
                           bvValue	= SD$AgeLength,
                           AgeType = SD$ageType,
                           AgeGroupPlus = "",
                           variableType = "",
                           total = SD$numbercaught, # Raised/Estimated NumberCaught
                           mean = SD$meanweight,
                           varianceTotal = NA,
                           varianceMean = NA,
                           PSUtype = "",
                           numPSUs = SD$numPSU,
                           numSamples = SD$NumSamplesLngt , # Samples containing Length data
                           numMeasurements = SD$numMeasurements)

# --- Stack Total (NumberCaught) and Mean Weight (Final format requirement) ---
distributions <- rbind(distributions, distributions)
distributions$variableType <- rep(c("Number", "WeightLive"),
                                  each = nrow(distributions)/2)

# Isolate NumberCaught and MeanWeight values into 'total' and 'mean' columns, respectively
distributions[distributions$variableType == "Number", "mean"] <- NA
distributions[distributions$variableType != "Number", "total"] <- NA

distributions <- distributions[order(distributions$domainBiology),] 
distributions <- distributions %>%
  filter(!is.na(bvValue)) %>%
  distinct()
distributions<-distinct(distributions)
headtail(distributions)


###**************************** Effort Data (HI) Table *****************
# HI: Effort data (Harvest Intensity) derived from combined sample/landings/effort table



HI <- Samp_mix_CL_CE_MQD%>%
  fselect(
    Year = year, Country,ExpertGroup , Stock,Season=  Q,
    FishingArea= area, Fleet=metier6 ,  CatchCategory, cod.FAO,Effort
  ) %>%mutate(
    SeasonType="Quarter",
    
  )  %>%
  
  distinct()

# Recode effort unit
HI$quarter <- ifelse(HI$SeasonType == "Quarter", 
                     HI$Season, NA)
HI$UnitEffort <- "Kwd"
effort_relation <- data.frame(UnitEffort = c("dop", "kwd", "fd", "hf", "kh", "NoV", "tr"),
                              variableType = c("ScientificDaysAtSea",
                                               "scientifickWDaysAtSea",
                                               "scientificFishingDays",
                                               "scientificVesselFishingHour",
                                               "scientificVesselKgPrHour",
                                               "numberOfUniqueVessels",
                                               "numberOfDominantTrips"))
HI <- left_join(HI, effort_relation)

# Aggregate effort by unique key using data.table (for speed)
setDT(HI)
HI[, key := paste(Country, Year, Season, FishingArea, Fleet, UnitEffort, sep = "_")]
HI[, Effort := as.numeric(Effort)]

# Group and sum effort
HI <- HI[, .(total = sum(Effort, na.rm = TRUE)),
         by = .(Country, Year, quarter, FishingArea,
                ExpertGroup, Stock, Fleet, variableType, key)]

# Construct final EFFORT table
effort <- data.frame(vesselFlagCountry = HI$Country,
                     year = HI$Year,
                     workingGroup = HI$ExpertGroup,
                     Stock = HI$Stock,
                     quarter = HI$quarter,
                     area = HI$FishingArea,
                     fisheriesManagementUnit = "",
                     metier6 = "",
                     fleetType= "" ,
                     fleet = HI$Fleet,
                     variableType = HI$variableType,
                     total = HI$total
)


# Assign FMU based on Stock (PIL uses area as FMU)
effort <- effort %>%
  mutate(
    fisheriesManagementUnit = case_when(
      workingGroup == "WGHANSA" ~ area, # WGHANSA stocks (PIL) use area as FMU
      TRUE ~ ""
    )
  )



headtail(effort)


#############Spatial landings details (CL_all) ###########
# Final table summarizing spatial landing details
CL_all<-CL_all %>%
  mutate(
    fishManUnit = case_when(
      Stock == "mac.27.nea" ~ NA_character_,
      Stock == "pil.27.8c9a" ~ area,
      TRUE ~ NA_character_
    )
  ) %>%
  relocate(fishManUnit, .after = area)



# Assign aggregated fleet categories (for final spatial output)
CL_all <- CL_all %>%
  mutate(
    Fleet = case_when(
      cod.FAO == "MAC" ~ "ES_ALL",
      cod.FAO == "PIL" & grepl("^PS", metier6, ignore.case = TRUE) ~ "Purseseine",
      grepl("^(OTB|PTB)", metier6, ignore.case = TRUE) ~ "Trawler",
      TRUE ~ "Artisanal" # Default category
    )
  )

# Construct final SPATIAL_LANDINGS_DETAILS table
Spatial_landings_details <- tibble::tibble(
  vesflagCou     = CL_all$Country,
  year           = CL_all$year,
  wg             = CL_all$ExpertGroup,
  stock          = CL_all$Stock,
  specCode       = CL_all$cod.FAO,
  catchCat       = CL_all$CatchCategory,
  seasonType     = "Quarter",
  seasonVal      = CL_all$Q,
  areaType       ="AreaICES",
  areaVal        = CL_all$area,
  fishManUnit    = CL_all$fishManUnit,
  statRect       = CL_all$statisticalRectangle,
  dSoucstatRect  = CL_all$landingLocation, # Data Source for statistical Rectangle
  metier6        = CL_all$metier6,
  fleet          = CL_all$Fleet
) %>% unique()




base_dir<-getwd()
create_project_folders <- function(base_dir = ".") {
  # Lista de carpetas a crear
  folders <- c(
    "data",
    "data_scripts",
    "method",
    "method_scripts",
    "output",
    "output_scripts",
    "report",
    "report_scripts",
    "utilities_scripts"
  )
  
  # Crear cada carpeta si no existe
  for (folder in folders) {
    dir.create(file.path(base_dir, folder), showWarnings = FALSE, recursive = TRUE)
  }
  
  message("Project folders created in: ", normalizePath(base_dir))
}

# Ejemplo de uso:
# create_project_folders("C:/Users/Usuario/WKRDBESRaiseStock/R_template")
create_project_folders()
#######################
## Section 4) output
#######################
# Save final results to the output directory.  Store and source any required scripts from output_scripts.

# Make all the directories we will need
SPL <- Spatial_landings_details %>%
  full_join(stockfmu %>% mutate(stock = Stock), by = "stock") %>%
  mutate(ExpertGroup = wg)


stocks <- StockListbyEG$Stock
WGs <- unique(StockListbyEG$ExpertGroup)

for (wg in WGs) {
  # Get all stocks for this WG
  stocks <- unique(StockListbyEG$Stock[StockListbyEG$ExpertGroup == wg])
  
  
  for (myStock in stocks) {
    dirName <- "output"
    icesTAF::mkdir(paste0(dirName,"/",wg,"/",myStock,"/census_catches"))
    icesTAF::mkdir(paste0(dirName,"/",wg,"/",myStock,"/estimated_catches"))
    icesTAF::mkdir(paste0(dirName,"/",wg,"/",myStock,"/distributions"))
    icesTAF::mkdir(paste0(dirName,"/",wg,"/",myStock,"/effort"))
    icesTAF::mkdir(paste0(dirName,"/",wg,"/",myStock,"/Spatial_landings_details"))
    
    # Filter data for current stock and WG
    stock_data <- SI[SI$ExpertGroup == wg & SI$Stock == myStock, ]
    effort_data <- HI[HI$ExpertGroup == wg & HI$Stock == myStock, ]
    length_data <- SD[SD$ExpertGroup == wg & SD$Stock == myStock, ]
    Spatial_data<- SPL[SPL$ExpertGroup == wg & SPL$Stock == myStock, ]
    
    # Split into Landings and Discards
    landings_data <- subset(stock_data, CatchCategory == "Lan") %>% fselect(-ExpertGroup)
    discards_data <- subset(stock_data, CatchCategory == "RegDis") %>% fselect(-ExpertGroup)
    
    # InterCatch matrix without column names
    EFFORT <-   effort_data%>%
      fselect(-ExpertGroup) %>%
      mutate(Stock = "")
    
    DIST <- length_data %>%
      fselect(-ExpertGroup) 
    SPATIAL<-   Spatial_data%>%
      fselect(-ExpertGroup)
    
    
    # Write to CSV
    fwrite(landings_data, paste0(dirName, "/", wg, "/", myStock, "/census_catches/", myStock, "_Landings.csv"), row.names = FALSE, sep = ";")
    fwrite(discards_data, paste0(dirName, "/", wg, "/", myStock, "/estimated_catches/", myStock, "_Discards.csv"), row.names = FALSE, sep = ";")
    fwrite( EFFORT, paste0(dirName, "/", wg, "/", myStock, "/effort/", myStock, "_2024_Effort.csv"), col.names = FALSE, row.names = FALSE, sep = ";")
    fwrite( DIST, paste0(dirName, "/", wg, "/", myStock, "/distributions/", myStock, "_2024_distributions.csv"), col.names = FALSE, row.names = FALSE, sep = ";")
    fwrite( SPATIAL, paste0(dirName, "/", wg, "/", myStock, "/Spatial_landings_details/", myStock, "_2024_Spatial_landings_details.csv"), col.names = FALSE, row.names = FALSE, sep = ";")
    
    
  }
}











  
  
  

