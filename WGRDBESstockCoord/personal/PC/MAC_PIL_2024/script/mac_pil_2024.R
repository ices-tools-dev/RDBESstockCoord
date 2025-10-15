##############################################
rm(list = ls())
# Librerías necesarias
# Vector con los paquetes deseados
packs <- c(
  "dplyr", "data.table", "janitor", "readr", "readxl", "lubridate", 
  "stringr", "icesVocab", "icesSD", "tidyr", "FSA", "collapse"
)

# Instala los que faltan y carga todos
invisible(lapply(packs, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}))

#tablas necesarias

###********************************** Stocklist ****************************
###*
library(icesSD)

# --- 1. Descargar y filtrar datos de 2024 ---
y2024 <- getSD(year = 2024) %>%
  select(
    ActiveYear,
    ExpertGroup,
    Stock = StockKeyLabel,
    SpeciesName = SpeciesScientificName
  ) %>%
  filter(Stock %in% c("pil.27.8c9a", "mac.27.nea")) %>%
  distinct()  # elimina duplicados de una vez
head (y2024)
# --- 2. Cargar y limpiar lista por área ---
StockListbyArea <- read.csv(
  "boot/initial/data/StockAssessmentGraphs_2025.csv",
  header = TRUE, sep = ";", encoding = "UTF-8"
) %>%
  filter(Year == 2024) %>%
  transmute(  # más rápido que select() + rename()
    Stock = FishStock,
    SpeciesName,
    ICESArea = gsub("~", ",", trimws(ICES.Areas..splited.with.character.....))
  ) %>%
  distinct()

# --- 3. Códigos FAO ---
codes_FAO <- getCodeList("SpecASFIS") %>%
  transmute(
    cod.FAO = Key,
    SpeciesName = Description,
    SpeciesCode = Id
  ) %>%
  distinct()

# --- 4. Unión de todo ---
StockListbyEG <- y2024 %>%
  left_join(StockListbyArea, by = c("Stock", "SpeciesName")) %>%
  left_join(codes_FAO, by = "SpeciesName") %>%
  mutate(
    AphiaID = SpeciesCode
  ) %>%
  filter(
    cod.FAO %in% c("MAC", "PIL"),
    Stock %in% c("mac.27.nea", "pil.27.8c9a")
  ) %>%
  distinct(Stock, .keep_all = TRUE) %>%
  select( Year=ActiveYear,ExpertGroup,
    Stock, SpeciesName, cod.FAO, AphiaID,ICESArea
  )

head(StockListbyEG)
#setwd("C:/Users/Usuario/Nextcloud/RDBES/2025/RAISESTOCK2")





SA <- fread("boot/initial/data/Sample.csv", quote = "", encoding = "Latin-1") %>%
  fselect(SAid, SSid, SamplingScheme=SAcommSizeCatScale,  CatchCategory = SAcatchCategory, speciesCode = SAspeciesCode, SAspeciesCodeFAO, SAsex, SAcommSizeCat, 
          SAtotalWeightLive, SAsampleWeightLive) %>% 
  filter(!is.na( SAtotalWeightLive) &   SAsampleWeightLive>0 ) %>% 
  fmutate(speciesCode = as.character(speciesCode)); headtail (SA)

FM <- fread("boot/initial/data/FrequencyMeasure.csv", quote = "", encoding = "Latin-1") %>%
  fselect(FMid, SAid, FMclassMeasured, FMnumberAtUnit, FMtypeMeasured, FMtypeAssessment); head (FM)

SS <- fread("boot/initial/data/SpeciesSelection.csv", quote = "", encoding = "Latin-1") %>%
  fselect(SSid, LEid, SLid, SSsampled,SSspeciesListName) %>%
 # filter(SLid %in% c(207,523))%>%
  arrange(SSspeciesListName); head (SS)#

LE <- fread("boot/initial/data/LandingEvent.csv", quote = "", encoding = "Latin-1") %>%
  fselect(Country= LEcountry,LEid, OSid,  LEmetier6, LEarea,LEunitName, LEdate,LElocode ,LEencryptedVesselCode, LElocationType,
          LEfisheriesManagementUnit) %>% arrange(desc(LEfisheriesManagementUnit)); head (LE)

PARAMETERS<- fread("boot/initial/data/MAC_PIL_Parameters.csv", encoding = "Latin-1") %>%
  select(
    Year = ActiveYear, ExpertGroup, Stock = StockKeyLabel,  Q,cod.FAO,
    SpeciesName, speciesCode, FMU = Fisheries_management_unit, area, a, b
  ) %>%
  
  filter(!is.na(a) & a != "") %>%
  filter(cod.FAO %in% c('PIL', 'MAC')) %>% 
  mutate(area = ifelse(cod.FAO== "PIL" & area %in% c("27.8.c", "27.9.a"), FMU, area)  ,
    speciesCode = as.character(speciesCode)
  ) %>%
  
  mutate(Country="ES") %>% 
  select(-FMU) %>%
  unique()

headtail(PARAMETERS)


rm(Samp)
source("boot/initial/utilities/funciones/procesar_Samp.R")



Samp <- procesar_Samp(SA, FM, SS, LE, PARAMETERS, muestreos_FMU) %>% filter(unitName!="Trip")

headtail(Samp)
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
    totalWeightLive = totalWeightLive / 1000,
    sampleWeightLive = sampleWeightLive / 1000,
    meanweight,
    length = length/ 10,
    N = FMnumberAtUnit,
    Npond = round((totalWeightLive * N) / sampleWeightLive, 2)
  ) %>%
  group_by(
    Country,SLid, SamplingScheme, ExpertGroup , Stock , date,unitName, SAid, metier6, Q, area, CatchCategory,cod.FAO, SpeciesName,
    speciesCode, SAcommSizeCat, totalWeightLive, sampleWeightLive, meanweight, length
  ) %>%
  summarise(
    N = sum(N),
    Npond = sum(Npond),
    .groups = "drop"
  )

Samp_Lengths <-Samp_Lengths %>% group_by(metier6, Q ,  area, CatchCategory ,cod.FAO) %>% 
  mutate(NumSamplesLngt= n_distinct(unitName),
         num_PSU = n_distinct(date),
         numLngtMeas =sum(N))
head(Samp_Lengths) %>% as.data.frame()
tabyl(Samp, CatchCategory)


Samp_mix_MQD<- Samp_Lengths %>%
  distinct() %>%
  mutate(Weight_sample_length = N * meanweight) %>%
  group_by(metier6, Q, area, CatchCategory,cod.FAO, SpeciesName, speciesCode, NumSamplesLngt ,num_PSU,     numLngtMeas) %>%
  summarise(SampleWeightLive = sum(Weight_sample_length, na.rm = TRUE), .groups = "drop") %>%
  left_join(
    Samp_Lengths %>%
      distinct() %>%
      group_by(ExpertGroup, Stock, metier6, Q, area, CatchCategory,cod.FAO, speciesCode, meanweight,
               length,, NumSamplesLngt ,num_PSU,     numLngtMeas) %>%
      summarise(
        N_length = sum(N, na.rm = TRUE),
        N_Pond_length = sum(Npond, na.rm = TRUE),
        .groups = "drop"
      )
  ) %>% filter(!(cod.FAO == "PIL" & area == "27.8.b")) %>% 
  select(ExpertGroup, Stock, metier6, Q, area,CatchCategory, cod.FAO, SampleWeightLive,
         meanweight, length, N_length, N_Pond_length, NumSamplesLngt ,num_PSU,     numLngtMeas) %>%
 
  ungroup() %>%
  as.data.frame()
headtail(Samp_mix_MQD)


subset(Samp_mix_MQD,cod.FAO == "MAC" & Q == 4 & metier6 == "PS_SPF_>0_0_0"  & area=='27.9.a') %>% as.data.frame()


subset(Samp_mix_MQD,cod.FAO == "MAC" & Q == 1 & metier6 == "OTB_DEF_70-99_0_0"  & area=='27.8.a') %>% as.data.frame()





CL_all <- fread("boot/initial/data/CommercialLanding.csv") %>%
  fselect(
   Country= CLlandingCountry, CLid, year = CLyear, metier6 = CLmetier6, Q = CLquarter, area = CLarea,  
    CLspeciesCode, statisticalRectangle = CLstatisticalRectangle, landingLocation = CLlandingLocation,
    #CLencryptedVesselIds, CLnumberOfUniqueVessels,
    CLspeciesFaoCode, CatchCategory = CLcatchCategory, CLnationalFishingActivity, CLsamplingScheme,
    CLWeight = CLofficialWeight
  ) %>%  
  # filter(CatchCategory == "Lan") %>%
  mutate(speciesCode = as.character(CLspeciesCode)) %>%
  
  filter(!is.na(CLid)  &  CLspeciesCode %in% c('127023', '126421') & Country== "ES")   %>%
  mutate(cod.FAO= ifelse(speciesCode==127023, 'MAC', 'PIL')) %>% 
  filter(!(cod.FAO == "PIL" & area == "27.8.b")) %>% 
  select(-CLspeciesCode,-CLspeciesFaoCode)
source("boot/initial/utilities/funciones/assign_fmu.R")
CL_all <- assign_fmu(CL_all, PARAMETERS)

CL_all%>% arrange (Country) %>% headtail(9) %>% select(1,2,,7,8,9,11,12,14,15)


tabyl(CL_all, CatchCategory)      

filter(CL_all, metier6 ==c('LLS_DEF_0_0_0')  & area %in% c('27.9.a'))

CL_ES_MQD <- CL_all %>%
  group_by(Country,ExpertGroup, Stock,metier6, Q, area, CatchCategory, speciesCode,cod.FAO) %>%
  summarise(
    officialWeight = sum(CLWeight, na.rm = TRUE),
    .groups = "drop"
  ) %>%arrange(metier6) %>%  as.data.frame()

head(CL_ES_MQD)
filter(CL_ES_MQD, metier6 ==c('LLS_DEF_0_0_0')  & area %in% c('27.9.a'))
subset(CL_ES_MQD,cod.FAO == "MAC" & Q == 1 & metier6 == "OTB_DEF_70-99_0_0"  & area=='27.8.a') %>% as.data.frame()

Samp_mix_CLMQD <- full_join(
  Samp_mix_MQD,
  CL_ES_MQD,relationship = "many-to-many") %>% 
  #filter((!is.na(length ))) %>% 
  # select(-CLsamplingScheme) %>% 
  unique() %>%  as.data.frame() %>% arrange(Country)
  #filter(!is.na(officialWeight))

headtail(Samp_mix_CLMQD)
subset(Samp_mix_CLMQD ,cod.FAO == "MAC" & Q == 1 & metier6 == "OTB_DEF_70-99_0_0"  & area=='27.8.a') %>% as.data.frame()
subset(Samp_mix_CLMQD,cod.FAO == "MAC" & Q ==2 & metier6 == "LLS_DEF_0_0_0"  & area=='27.9.a') %>% as.data.frame()

head (Samp_mix_CLMQD)
source("boot/initial/utilities/funciones/calc_numbercaught.R")
Samp_mix_CLMQD2 <- Samp_mix_CLMQD %>%
  calc_numbercaught() %>%
  select(Country, ExpertGroup, Stock, metier6, Q, area, CatchCategory, 
         speciesCode,cod.FAO,
         SampleWeightLive, meanweight, length, N_Pond_length,numLngtMeas,
         NumSamplesLngt, num_PSU,
         officialWeight, numbercaught,) %>%
  distinct() %>%
  left_join(
    Samp_Lengths %>%
      count(Country, metier6, Q, CatchCategory, area, cod.FAO, name = "NumSamplesLngt") %>%
      distinct()
  ) %>% mutate(
    Country = "ES",
    officialWeight = ifelse(is.na(officialWeight), 0, officialWeight),
    numbercaught = ifelse(is.na(numbercaught), 0, numbercaught),
    speciesCode = case_when(
      is.na(speciesCode) & cod.FAO == "MAC" ~ "127023",
      is.na(speciesCode) & cod.FAO == "PIL" ~ "126421",
      TRUE ~ speciesCode
    )
  ) %>%
  as.data.frame()




headtail(Samp_mix_CLMQD2)





source("boot/initial/utilities/funciones/asignar_FMU.R")
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
  
  

stockfmu<-Samp_mix_CLMQD[,c('area', 'Stock', 'ExpertGroup', "cod.FAO")] %>%
  filter(!is.na(ExpertGroup) &   Stock %in% c("pil.27.8c9a", "mac.27.nea"  )) %>% 
  arrange(ExpertGroup, Stock) %>% unique()
headtail(stockfmu,2)
CE_MQD<- CE %>%
  group_by(year,Country,Q , area, FMU,metier6) %>%
  summarise(Effort = sum(CEofficialkWFishingDays, na.rm = TRUE), .groups = "drop")


head (CE_MQD)
head (stockfmu)

CE_MQD2 <- bind_rows(
  CE_MQD %>%
    left_join(stockfmu %>% filter(Stock == "mac.27.nea"), by = c("area" = "area")),
  CE_MQD %>%
    left_join(stockfmu %>% filter(Stock == "pil.27.8c9a"), by = c("FMU" = "area"))
) %>% unique() %>% 
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





# Realizar el join por los campos deseados, ejemplo por Stock y area_join
Samp_mix_CL_CE_MQD <- Samp_mix_CLMQD2 %>%
  left_join(CE_MQD2) %>% unique()

Samp_mix_CL_CE_MQD <-Samp_mix_CL_CE_MQD %>%
  mutate(
    fishManUnit = case_when(
      Stock == "mac.27.nea" ~ NA_character_,
      Stock == "pil.27.8c9a" ~ area,
      TRUE ~ NA_character_
    )
  ) %>%
  relocate(fishManUnit, .after = area)

headtail(Samp_mix_CL_CE_MQD)

subset(Samp_mix_CL_CE_MQD,cod.FAO == "MAC" & Q == 1 & metier6 == "OTB_DEF_70-99_0_0"  & area=='27.8.a') %>% as.data.frame()

#rm(CE_MQD, codes_FAO, FM, LE, SA, SS, y2024, StockListbyArea)







###********************************** create Census catches from SAMP CLMQD ********************************
###*
###*
###*sAM
head (Samp_mix_CL_CE_MQD)
colSums(is.na(Samp_mix_CL_CE_MQD))
SI<- Samp_mix_CL_CE_MQD%>% mutate(

  Year= year,
  Season= Q,
  SeasonType= "Quarter",
  Species=cod.FAO,
  AreaType ="Div",
  FishingArea= area,
  Fleet=metier6,
  CATON= officialWeight
  
) %>% fselect(-SampleWeightLive, -meanweight, -length,-N_Pond_length,-numbercaught, -year) %>% 
  distinct()

headtail( SI)

SI$key = paste(SI$Country,
                           SI$Year,
                           SI$Season,
                           SI$Species,
                           SI$FishingArea,
                           SI$Fleet,
                           SI$CatchCategory,
                           
                           sep = "_")


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

#headtail(SD)
SI<-distinct(SI)
SD$key = paste(SD$Country,
                         SD$Year,
                         SD$Season,
                         SD$Species,
                         SD$FishingArea,
                         SD$Fleet,
                         SD$CatchCategory,
                         sep = "_")


#head( SI)
#clean up SI
SI <- SI[! (SI$CATON == 0 & SI$CatchCategory == "Lan"), ]
SI <- SI[!duplicated(SI$key), ]

cols_to_be_rectified <- names(SI)[vapply(SI, is.character, logical(1))]
#SI[, cols_to_be_rectified] <- lapply(SI[, cols_to_be_rectified, with = F], trimws)


#SI <- left_join(SI, unique(StockListbyEG[, c("Species", "AphiaID", "StockCode")])) 
StockListbyEG$Year<-as.integer(StockListbyEG$Year)
head (StockListbyEG)
head(SI,2)
StockListbyEG <- StockListbyEG %>%
  rename(Species = cod.FAO)
SI <- left_join(SI, StockListbyEG)
#setnames(Samp_mix_CLMQD, "ExpertGroup", "EG")
#attach stockcode and WG
#Samp_mix_CLMQD <- stock_relation(Samp_mix_CLMQD)
head (SI)
# create domains, and fill in discard domain for covered landings domains.
SI$key2 <- substr(SI$key, 1, nchar(SI$key)-4)
tbl <- unique(SI[, c("key2", "CatchCategory")])
#head (tbl)

has_dis <- tbl[duplicated(tbl$key2), "key2"]

#If the stock area consists of multiple ICES areas and the estimation/distribution covers the entire stock, 
#'all' is used in the domain definition (option: use_stock_area = TRUE); 
#if the estimation/distribution is per ICES area, then the ICES area is used in the domain definition (option: use_stock_area = FALSE)


define_domain <- function(df, use_stock_area = TRUE) {
  df <- df %>%
    mutate(domain = if (use_stock_area) {
      paste(Season, 'all', Fleet, sep = "_")
    } else {
      paste(Season, FishingArea, Fleet, sep = "_")
    })
  
  return(df)
}

SI<- define_domain(SI, use_stock_area = FALSE)
#SI2<- define_domain(SI, use_stock_area = TRUE)
headtail(SI)

#create columns for format
SI$domainCatchDis <- ifelse(SI$key2 %in% has_dis | SI$CatchCategory == "DIS",
                                        SI$domain, "")

SI$domainBiology <- ifelse(SI$key %in% SD$key,
                                       SI$domain, "")

SI$quarter <- ifelse(SI$SeasonType == "Quarter", 
                                 SI$Season, NA)

#SI <- SI[order(SI$key), ]
#headtail(SI)
tabyl (SI, CatchCategory)
CI<-SI[SI$CatchCategory == "Lan", ]
headtail(CI)    
#make final table
census_catches <- data.frame(VesselFlagCountry = CI$Country,
                             year = CI$Year,
                             workingGroup = CI$ ExpertGroup,
                             stock = CI$Stock,
                             speciesCode = CI$AphiaID,
                             CatchCategory	= CI$CatchCategory,
                             quarter = CI$quarter,
                             AreaType =CI$AreaType,
                             area = CI$FishingArea,
                             fisheriesManagementUnit = NA,
                             metier6 =  CI$Fleet,
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
                             num_trips= "",
                             comment = ""
) %>% distinct()

#library(stringr)

census_catches<-census_catches %>%
  mutate(
    AreaType = case_when(
      stock == "mac.27.nea" ~ "Div",
      stock == "pil.27.8c9a" ~ "SubDiv",
      TRUE ~ AreaType
    )    )
  
headtail(census_catches)




####################################################################################################
###********************************** create Estimated catches from SI *********************************
est <- SI[SI$CatchCategory != "Lan", ]
est<-filter(est,!is.na(CatchCategory))
#get number samples from sd
headtail(Samp_mix_CL_CE_MQD)
tabyl(est, CatchCategory)
xx <- unique(SD[SD$CatchCategory != "Lan", c("NumSamplesLngt", "key")])
est <- merge(est, xx, all.x = T)
est$NumSamplesLngt[is.na(est$NumSamplesLngt)] <- 0
est<-subset(est, !is.na(SpeciesName))
#make final table
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
                                numSamples = est$NumSamplesLngt
)
head(estimated_catches)




###################################################
###**************************** create length age and whatever distribution from SD *****************
#clean up SD
cols_to_be_rectified <- names(SD)[vapply(SD, is.character, logical(1))]
SD[, cols_to_be_rectified] <- lapply(SD[, cols_to_be_rectified], trimws)
SD$cod.FAO<- SD$Species


StockListbyEG$Year<-as.integer(StockListbyEG$Year)
SD<- left_join(SD, StockListbyEG) 

#attach stockcode and WG
#Sample_Lengths <- stock_relation(Sample_Lengths)
headtail(SD)
#recode / create costum columns
SD <-SD%>%
  mutate(
    CANUMtype = "Lenght",
  
    AgeLength =length,
    bvType = ifelse(CANUMtype == "age", "Age", "Length"),
    ageType = ifelse(bvType == "Age", "ageyear", ""),
    numPSUs = ifelse(bvType == "Age", NumSamplesAge, NumSamplesLngt),
    numMeasurements = ifelse(bvType == "Age", NumAgeMeas, numLngtMeas)
  )

#If the stock area consists of multiple ICES areas and the estimation/distribution covers the entire stock, 
#'all' is used in the domain definition (option: use_stock_area = TRUE); 
#if the estimation/distribution is per ICES area, then the ICES area is used in the domain definition (option: use_stock_area = FALSE)
SD<- define_domain(SD, use_stock_area = FALSE)
headtail(SD)
subset(SD, Species== "MAC"  & Q==4  & metier6 =='PS_SPF_>0_0_0' & area =='27.9.a')
#make final table
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
                           total = SD$numbercaught,
                           mean = SD$meanweight,
                           varianceTotal = NA,
                           varianceMean = NA,
                           PSUtype = "",
                           numPSUs = SD$numPSU,
                           numSamples = SD$NumSamplesLngt ,
                           numMeasurements = SD$numMeasurements)

subset(distributions, domainBiology== "4_27.9.a_PS_SPF_>0_0_0")

# stack total and mean into their own lines --> om numbers en mean wight 'onder ' elkaar te hebben ipv 'naast' elkaar
distributions <- rbind(distributions, distributions)
distributions$variableType <- rep(c("Number", "WeightLive"),
                                  each = nrow(distributions)/2)

distributions[distributions$variableType == "Number", "mean"] <- NA
distributions[distributions$variableType != "Number", "total"] <- NA

distributions <- distributions[order(distributions$domainBiology),] 
distributions <- distributions %>%
  filter(!is.na(bvValue)) %>%
  distinct()
distributions<-distinct(distributions)
headtail(distributions)



###**************************** effort data from HI *****************
source("boot/initial/utilities/funciones/asignar_FMU.R")
head (Samp_mix_CL_CE_MQD)
stockfmu<-Samp_mix_CL_CE_MQD[,c('area', 'Stock', 'ExpertGroup', "cod.FAO")] %>% 
  filter(Stock %in% c("pil.27.8c9a", "mac.27.nea")) %>%arrange(Stock) %>%  unique()

stockfmu
HI <- Samp_mix_CL_CE_MQD%>%
  fselect(
    Year = year, Country,ExpertGroup , Stock,Season=  Q,
    FishingArea= area, Fleet=metier6 ,  CatchCategory, cod.FAO,Effort
  ) %>%mutate(
              SeasonType="Quarter",
            
              )  %>%
  
  distinct()

#code translate
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
headtail(HI)
# sum by key
HI$key = paste(HI$Country,
               HI$Year,
               HI$Season,
               HI$FishingArea,
               HI$Fleet,
               HI$UnitEffort,
               sep = "_")
setDT(HI)
# Crear la clave
HI[, key := paste(Country, Year, Season, FishingArea, Fleet, UnitEffort, sep = "_")]

# Convertir Effort a numérico
HI[, Effort := as.numeric(Effort)]

# Agrupar y sumar
# Agrupar y sumar
HI <- HI[, .(total = sum(Effort, na.rm = TRUE)),
         by = .(Country, Year, quarter, FishingArea,
                ExpertGroup, Stock, Fleet, variableType, key)]

head (HI)

#attach WG
#CE <- assign_workinggroup(combined_HI)

#order and output format
#combined_HI <- combined_HI[order(combined_HI$key), ]
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



effort <- effort %>%
  mutate(
    fisheriesManagementUnit = case_when(
      workingGroup == "WGHANSA" ~ area,
      TRUE ~ ""
    )
  )
setDT(effort)
effort[Stock != "mac.27.nea", area := substr(area, 1, 6)]
headtail(effort)
subset(effort, year==2024 & fleet=='PS_SPF_>0_0_0' & area %in% c('27.8.c', '27.8.c.w' ,'27.8.c.e'    ) & quarter==2 )

subset(effort, year==2024 & fleet=='OTB_DEF_70-99_0_0' & area %in% c('27.8.a' ) & quarter==1)












#############Spatial landings details###########
CL_all<-CL_all %>%
     mutate(
         fishManUnit = case_when(
             Stock == "mac.27.nea" ~ NA_character_,
             Stock == "pil.27.8c9a" ~ area,
             TRUE ~ NA_character_
           )
       ) %>%
     relocate(fishManUnit, .after = area)
 headtail(CL_all)
headtail (CL_ES_MQD)
tabyl(CL_all, metier6)
CL_all <- CL_all %>%
  mutate(area = substr(area, 1, 6))

CL_all <- CL_all %>%
  mutate(
    Fleet = case_when(
      cod.FAO == "MAC" ~ "ES_ALL",
      cod.FAO == "PIL" & grepl("^PS", metier6, ignore.case = TRUE) ~ "Purseseine",
      grepl("^(OTB|PTB)", metier6, ignore.case = TRUE) ~ "Trawler",
      TRUE ~ "Artisanal"
    )
  )

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
  dSoucstatRect  = CL_all$landingLocation,
  metier6        = CL_all$metier6,
  fleet          = CL_all$Fleet
) %>% unique()

headtail(Spatial_landings_details)
##############################################
### WKRDBES_RaiseStock R template
##############################################
# This template follows the TAF structure - the aim is to make it easier to migrate 
# code to TAF at a future date.
library(icesTAF)


#######################
## Section 1) utilities
#######################
# Store and source any commmon functions and code that you will use later from utilities_scripts. 

source("utilities_scripts/commonFunctions.R")
# Define WGWIDE stocks
head (StockListbyEG)
WG<- StockListbyEG$ExpertGroup
stocks <- StockListbyEG$Stock
library(icesTAF)
taf.skeleton()
#######################
## Section 2) data
#######################
# Load and preprocess data. Store and source any required scripts from data_scripts.

# Make all the directories we will need
icesTAF::mkdir("data")

# Read some RDBES format data
CL <- read.csv("boot/initial/data/CommercialLanding.csv")
# do some processing on it
CL$extraCol <- "Test"
# Save the results 
save(CL,file="data/CL.rda")

SPL<-Spatial_landings_details
headtail(SPL)
stockfmu$stock<-stockfmu$Stock
SPL<-SPL %>% full_join(stockfmu) %>% 
  mutate(ExpertGroup=wg); headtail(SPL)
#######################
## Section 3) method
#######################
# Perform estimation. Store and source any required scripts from method_scripts.

# Make all the directories we will need
icesTAF::mkdir("method")


for (myStock in stocks) {
  dirName <- "method"
  icesTAF::mkdir(paste0(dirName,"/",wg,"/",myStock,"/census_catches"))
  icesTAF::mkdir(paste0(dirName,"/",wg,"/",myStock,"/estimated_catches"))
  icesTAF::mkdir(paste0(dirName,"/",wg,"/",myStock,"/distributions"))
  icesTAF::mkdir(paste0(dirName,"/",wg,"/",myStock,"/effort"))
  icesTAF::mkdir(paste0(dirName,"/",wg,"/",myStock,"/ Spatial_landings_details"))
}
# Run the estimation scripts - these should save their results to the relevant directory
# mac.27.nea
source('method_scripts/WGWIDE/mac_27_nea.R')


#######################
## Section 4) output
#######################
# Save final results to the output directory.  Store and source any required scripts from output_scripts.

# Make all the directories we will need

icesTAF::mkdir("output5")

stocks <- StockListbyEG$Stock
WGs <- unique(StockListbyEG$ExpertGroup)

for (wg in WGs) {
  # Get all stocks for this WG
  stocks <- unique(StockListbyEG$Stock[StockListbyEG$ExpertGroup == wg])
  
  
for (myStock in stocks) {
  dirName <- "output5"
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
  fwrite( EFFORT, paste0(dirName, "/", wg, "/", myStock, "/effort/", myStock, "_2024_eFFORT.csv"), col.names = FALSE, row.names = FALSE, sep = ";")
  fwrite( DIST, paste0(dirName, "/", wg, "/", myStock, "/distributions/", myStock, "_2024_LENGTHS.csv"), col.names = FALSE, row.names = FALSE, sep = ";")
  fwrite( SPATIAL, paste0(dirName, "/", wg, "/", myStock, "/Spatial_landings_details/", myStock, "_2024_Spatial_landings_details.csv"), col.names = FALSE, row.names = FALSE, sep = ";")
  
  
}
}




  
  
  
