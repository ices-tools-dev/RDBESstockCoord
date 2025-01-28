library(data.table)
library(icesVocab)
library(RstoxData)
library(dplyr)

  dat_path <- getwd()
  out_path <- file.path(dat_path, "Output")

  ###********************************** Functions ****************************

  adjustCatchCat<-function(df)
  {
    df[df$CatchCategory=="L",]$CatchCategory<-"LAN"
    df[df$CatchCategory=="D",]$CatchCategory<-"DIS"
    df[df$CatchCategory=="B",]$CatchCategory<-"BMS"
    return(df)
  }
  
  stock_relation <- function(df) {
    df$Stock <- sapply(1:nrow(df), function(i) {
      FishingArea <- df$FishingArea[i]
      AphiaID <- df$AphiaID[i]
      
      match <- StockListbyEG$StockCode[sapply(1:nrow(StockListbyEG), function(j) {
      ICESArea <- StockListbyEG$ICESArea[j]
      StockAphiaID <- StockListbyEG$AphiaID[j]
        
      ICESArea_clean <- trimws(unlist(strsplit(ICESArea, ",")))
        
      FishingArea %in% ICESArea_clean && AphiaID == StockAphiaID
      })]
      
      # Als er een match is, retourneer de eerste StockCode, anders NA
      if (length(match) > 0) {
        return(match[1])
      } else {
        return(NA)
      }
    })
    df <- df %>%
      left_join(StockListbyEG %>% select(StockCode, EG), 
                by = c("Stock" = "StockCode")) %>%
      rename(workinggroup = EG)
    return(df)  
   }
  
  
  define_domain <- function(df, use_stock_area = TRUE) {
    df <- df %>%
      mutate(domain = if (use_stock_area) {
        paste(Season, 'all', Fleet, sep = "_")
      } else {
        paste(Season, FishingArea, Fleet, sep = "_")
      })
    
    return(df)
  }
  
  
    effort_relation <- data.frame(UnitEffort = c("dop", "kwd", "fd", "hf", "kh", "NoV", "tr"),
                                variableType = c("ScientificDaysAtSea",
                                                 "scientifickWDaysAtSea",
                                                 "scientificFishingDays",
                                                 "scientificVesselFishingHour",
                                                 "scientificVesselKgPrHour",
                                                 "numberOfUniqueVessels",
                                                 "numberOfDominantTrips"))
        
  
  assign_workinggroup <- function(combined_HI) {
    combined_SI <-combined_SI
    unique_workinggroups <- unique(combined_SI$workinggroup)
    
    if (length(unique_workinggroups) == 1) {
      combined_HI$workinggroup<-unique_workinggroups
      return(combined_HI)
    }   
    
    area_grouping <- aggregate(workinggroup ~ FishingArea, data = combined_SI, function(x) unique(x))
    area_grouping$num_workinggroups <- sapply(area_grouping$workinggroup, length)
    
    if (all(area_grouping$num_workinggroups == 1)) {
      area_grouping <- area_grouping[, c("FishingArea", "workinggroup")] 
      combined_HI <- merge(combined_HI, area_grouping, by = "FishingArea", all.x = TRUE)
      return(combined_HI)
    }
    if (any(area_grouping$num_workinggroups > 1)) {
      combined_HI$workinggroup <- "Manual input required"
      print("Manual input required: multiple working groups are possible per area")
      print(area_grouping)
      return(combined_HI)
    }
    return(combined_HI)
  }
  
  
  convExchange <- function(dat_path = getwd(), #folder with intercatch echange format files
                           output_format = c("to_environment", "to_file"), #where should the output go
                           out_path = getwd()){ #if it should be in files, where
  
    if (output_format == "to_environment"){
      
      assign("census_catches", census_catches, .GlobalEnv)
      assign("estimated_catches", estimated_catches, .GlobalEnv)
      assign("distributions", distributions, .GlobalEnv)
      assign("effort", effort, .GlobalEnv)
      
    } else{
      
      write.csv(census_catches, paste0(out_path, "/census_catches.csv"), 
                row.names = F, quote = F)
      write.csv(estimated_catches, paste0(out_path, "/estimated_catches.csv"), 
                row.names = F, quote = F)
      write.csv(distributions, paste0(out_path, "/distributions.csv"), 
                row.names = F, quote = F)
      write.csv(effort, paste0(out_path, "/effort.csv"), 
                row.names = F, quote = F)
    } 
  }
  
  
  ###********************************** Stocklist ****************************
  #downloaded from: https://stockdatabase.ices.dk/default.aspx #specify the active year
  StockListbyEG<-read.csv(paste0(dat_path,"/StockList/EGsStocksByYear.csv"),header = TRUE, sep = ",", encoding = "UTF-8")
  StockListbyEG <- StockListbyEG[nzchar(StockListbyEG$StockCode), ]
  
  #downloaded from:https://standardgraphs.ices.dk/stockList.aspx
  StockListbyArea<-read.csv(paste0(dat_path,"/StockList/StockAssessmentGraphs_2025124mfqskttyclbazq2wfl5zbnzy.csv"),header = TRUE, sep = ",", encoding = "UTF-8")
  names(StockListbyArea)[[3]]<-"StockCode"
  codes_FAO <- getCodeList("SpecASFIS")
  names(codes_FAO)[[3]]<-"SpeciesName"
  codes_aph <- getCodeList("SpecWoRMS")
  names(codes_aph)[[3]]<-"SpeciesName"
  
  #Add Ices areas
  StockListbyEG <- merge(StockListbyEG, StockListbyArea[, c("StockCode", "ICES.Areas..splited.with.character.....", "SpeciesName")], by = c("StockCode"),all.x = TRUE)
  StockListbyEG <- StockListbyEG[!duplicated(StockListbyEG[, c("StockCode")]), ]
  
  names(StockListbyEG)[[4]]<-"ICESArea"
  StockListbyEG$ICESArea <- gsub("~", ",", StockListbyEG$ICESArea)
  StockListbyEG$ICESArea <- trimws(StockListbyEG$ICESArea)
  
  #Add FAO code
  StockListbyEG <- merge(StockListbyEG, codes_FAO[, c("SpeciesName", "Key")], by = c("SpeciesName"),all.x = TRUE)
  names(StockListbyEG)[[6]]<-"Species"
  
  #Add AphiaID
  StockListbyEG <- merge(StockListbyEG, codes_aph[, c("SpeciesName", "Key")], by = c("SpeciesName"),all.x = TRUE)
  names(StockListbyEG)[[7]]<-"AphiaID"
  
  #Add Year
  StockListbyEG$Year<-2023
  
  #Correction ICESArea
  StockListbyEG[StockListbyEG$StockCode =="cod.27.21",]$ICESArea<-"27.3.a.21"
  
    
  ###********************************** Load in intercatch exchange format files ****************************
  lst <- list.files(dat_path, pattern = ".csv", full.names = T)
  
  combined_HI <- data.table()  
  combined_SI <- data.table()  
  combined_SD <- data.table()  
  
  
  for (file in lst) {
    cat("Processing:", file, "\n")  
    IC<-parseInterCatch(file, encoding = "UTF-8")
    
    combined_HI <- bind_rows(combined_HI, IC$HI)
    combined_SI <- bind_rows(combined_SI, IC$SI)
    combined_SD <- bind_rows(combined_SD, IC$SD)
  }
  
  print(head(combined_HI))
  print(head(combined_SI))
  print(head(combined_SD))
  
  #re-code
  combined_SI<-adjustCatchCat(combined_SI)
  combined_SD<-adjustCatchCat(combined_SD)
  
  
  ###********************************** create Census catches from SI *********************************
  
  # set the match key 
  combined_HI$key = paste(combined_HI$Country,
                          combined_HI$Year,
                          combined_HI$Season,
                          combined_HI$FishingArea,
                          combined_HI$Fleet,
                          combined_HI$UnitEffort,
                          sep = "_")
  
  combined_SI$key = paste(combined_SI$Country,
                          combined_SI$Year,
                          combined_SI$Season,
                          combined_SI$Species,
                          combined_SI$FishingArea,
                          combined_SI$Fleet,
                          combined_SI$CatchCategory,
                          sep = "_")
  
  combined_SD$key = paste(combined_SD$Country,
                          combined_SD$Year,
                          combined_SD$Season,
                          combined_SD$Species,
                          combined_SD$FishingArea,
                          combined_SD$Fleet,
                          combined_SD$CatchCategory,
                          sep = "_")

  #clean up SI
  combined_SI <- combined_SI[! (combined_SI$CATON == 0 & combined_SI$CatchCategory == "LAN"), ]
  combined_SI <- combined_SI[!duplicated(combined_SI$key), ]
  
  cols_to_be_rectified <- names(combined_SI)[vapply(combined_SI, is.character, logical(1))]
  combined_SI[, cols_to_be_rectified] <- lapply(combined_SI[, cols_to_be_rectified, with = F], trimws)
  
  
  #attach aphiaId
  combined_SI <- merge(combined_SI, unique(StockListbyEG[, c("Species", "AphiaID")]), by = c("Species")) 
  
  #attach stockcode and WG
  combined_SI <- stock_relation(combined_SI)
  
  # create domains, and fill in discard domain for covered landings domains.
  combined_SI$key2 <- substr(combined_SI$key, 1, nchar(combined_SI$key)-4)
  tbl <- unique(combined_SI[, c("key2", "CatchCategory")])
  has_dis <- tbl[duplicated(key2), ]$key2 
  
  #If the stock area consists of multiple ICES areas and the estimation/distribution covers the entire stock, 
  #'all' is used in the domain definition (option: use_stock_area = TRUE); 
  #if the estimation/distribution is per ICES area, then the ICES area is used in the domain definition (option: use_stock_area = FALSE)
  combined_SI <- define_domain(combined_SI, use_stock_area = FALSE)
  
  #create columns for format
  combined_SI$domainCatchDis <- ifelse(combined_SI$key2 %in% has_dis | combined_SI$CatchCategory == "DIS",
                                       combined_SI$domain, "")
  
  combined_SI$domainBiology <- ifelse(combined_SI$key %in% combined_SD$key,
                                      combined_SI$domain, "")
  
  combined_SI$quarter <- ifelse(combined_SI$SeasonType == "Quarter", 
                                combined_SI$Season, NA)
  
  combined_SI <- combined_SI[order(combined_SI$key), ]
  
  #make final table
  census_catches <- data.frame(VesselFlagCountry = combined_SI$Country,
                               year = combined_SI$Year,
                               workingGroup = combined_SI$workinggroup,
                               stock = combined_SI$Stock,
                               speciesCode = combined_SI$AphiaID,
                               CatchCategory	= combined_SI$CatchCategory,
                               quarter = combined_SI$quarter,
                               area = combined_SI$FishingArea,
                               fisheriesManagementUnit = NA,
                               metier6 = NA,
                               fleet = combined_SI$Fleet,
                               domainCatchDis = combined_SI$domainCatchDis,
                               domainCatchBMS = "",
                               domainBiology = combined_SI$domainBiology,
                               variableType = "ScientificWeight_kg",
                               total = ifelse(combined_SI$CatchCategory == "LAN",
                                              combined_SI$CATON, NA),
                               comment = combined_SI$InfoStockCoordinator
  )
  
  ###********************************** create Estimated catches from SI *********************************
  est <- combined_SI[combined_SI$CatchCategory != "LAN", ]
  
  #get number samples from sd
  xx <- unique(combined_SD[combined_SD$CatchCategory != "LAN", c("NumSamplesLngt", "key")])
  est <- merge(est, xx, by = "key", all.x = T)
  est$NumSamplesLngt[is.na(est$NumSamplesLngt)] <- 0
  
  #make final table
  estimated_catches <- data.frame(VesselFlagCountry = est$Country,
                                 year = est$Year,
                                 workingGroup = est$workinggroup,
                                 stock = est$Stock,
                                 speciesCode = est$AphiaID,
                                 CatchCategory	= est$CatchCategory,
                                 domainCatch = est$domain,
                                 variableType = "ScientificWeight_kg",
                                 total = est$CATON,
                                 mean = NA,
                                 varianceTotal = NA,
                                 varinaceMean = NA,
                                 PSUtype = NA,
                                 numPSU = est$NumSamplesLngt,
                                 numSamples = est$NumSamplesLngt
  )
  
  estimated_catches <- estimated_catches[order(estimated_catches$domainCatch), ]
  
  ###**************************** create length age and whatever distribution from SD *****************
  #clean up SD
  cols_to_be_rectified <- names(combined_SD)[vapply(combined_SD, is.character, logical(1))]
  combined_SD[, cols_to_be_rectified] <- lapply(combined_SD[, cols_to_be_rectified, with = F], trimws)
  
  #attach aphiaId
  combined_SD <- merge(combined_SD, unique(StockListbyEG[, c("Species", "AphiaID")]), by = c("Species")) 
  
  #attach stockcode and WG
  combined_SD <- stock_relation(combined_SD)
  
  #recode / create costum columns
  combined_SD <- combined_SD %>%
    mutate(
      bvType = ifelse(CANUMtype == "age", "Age", "Length"),
      ageType = ifelse(bvType == "Age", "ageyear", ""),
      numPSUs = ifelse(bvType == "Age", NumSamplesAge, NumSamplesLngt),
      numMeasurements = ifelse(bvType == "Age", NumAgeMeas, NumLngtMeas)
    )
  
  #If the stock area consists of multiple ICES areas and the estimation/distribution covers the entire stock, 
  #'all' is used in the domain definition (option: use_stock_area = TRUE); 
  #if the estimation/distribution is per ICES area, then the ICES area is used in the domain definition (option: use_stock_area = FALSE)
  combined_SD <- define_domain(combined_SD, use_stock_area = FALSE)
  
  #make final table
  distributions = data.frame(VesselFlagCountry = combined_SD$Country,
                             year = combined_SD$Year,
                             workingGroup = combined_SD$workinggroup,
                             stock = combined_SD$Stock,
                             speciesCode = combined_SD$AphiaID,
                             CatchCategory	= combined_SD$CatchCategory,
                             domainBiology = combined_SD$domain,
                             fishDomain = "",
                             bvType = combined_SD$bvType,
                             bvValue	= combined_SD$AgeLength,
                             AgeType = combined_SD$ageType,
                             AgeGroupPlus = "",
                             variableType = "",
                             total = combined_SD$NumberCaught,
                             mean = combined_SD$MeanWeight,
                             varianceTotal = NA,
                             varianceMean = NA,
                             PSUtype = "",
                             numPSUs = combined_SD$numPSUs,
                             numSamples = combined_SD$numPSUs,
                             numMeasurements = combined_SD$numMeasurements)
  
  # stack total and mean into their own lines --> om numbers en mean wight 'onder ' elkaar te hebben ipv 'naast' elkaar
  distributions <- rbind(distributions, distributions)
  distributions$variableType <- rep(c("Number", "WeightLive"),
                                   each = nrow(distributions)/2)
  
  distributions[distributions$variableType == "Number", "mean"] <- NA
  distributions[distributions$variableType != "Number", "total"] <- NA
 
  distributions <- distributions[order(distributions$domainBiology),] 
    
  ###**************************** effort data from HI *****************
  setDT(combined_HI)
  combined_HI <- combined_HI[! is.na(combined_HI$UnitEffort) & !duplicated(combined_HI$key), ]
  
  #code translate
  combined_HI$quarter <- ifelse(combined_HI$SeasonType == "Quarter", 
                                combined_HI$Season, NA)
  
  combined_HI <- merge(combined_HI, effort_relation, by = "UnitEffort")
  
  # sum by key
  combined_HI <- combined_HI[ ,. (total = sum(Effort)),
            by = .(Country, Year, quarter, Season, FishingArea,
            Fleet, variableType, key)]

  
  #attach WG
  combined_HI <- assign_workinggroup(combined_HI)
  
  #order and output format
  combined_HI <- combined_HI[order(combined_HI$key), ]
  effort <- data.frame(vesselFlagCountry = combined_HI$Country,
                       year = combined_HI$Year,
                       workingGroup = combined_HI$workinggroup,
                       quarter = combined_HI$quarter,
                       area = combined_HI$FishingArea,
                       fisheriesManagementUnit = "",
                       metier6 = "",
                       fleet = combined_HI$Fleet,
                       variableType = combined_HI$variableType,
                       total = combined_HI$total
  )

   
  ###**************************** output results to environment or file *****************                         
  
  convExchange(dat_path = dat_path, output_format = "to_environment")

  convExchange(dat_path = dat_path, output_format = "to_file", out_path = out_path)


