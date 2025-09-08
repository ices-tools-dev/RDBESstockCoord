#' InterCatch exchange format to RCEF
#'
#' @param dat_path folder with intercatch echange format files
#' @param stock_relation
#' @param output_format where should the output go
#' @param out_path if it should be in files, where
#'
#' @returns
#' @export
#'
#' @examples
convExchange <- function(dat_path = getwd(),
                         stock_relation = stock_relation,
                         metier6 = NULL,
                         output_format = c("to_environment", "to_file"), #
                         out_path = getwd()) {

  #fixed relations
  effort_relation <- data.frame(UnitEffort = c("dop", "kWd", "fd", "hf", "kh", "NoV", "tr"),
                                variableType = c("ScientificDaysAtSea",
                                                 "scientifickWDaysAtSea",
                                                 "scientificFishingDays",
                                                 "scientificVesselFishingHour",
                                                 "scientificVesselKgPrHour",
                                                 "numberOfUniqueVessels",
                                                 "numberOfDominantTrips")
  )


  ###********************************** Load in intercatch eschange format files ****************************
  lst <- list.files(dat_path, pattern = ".csv", full.names = T)

  print("Creating RCEF from:")
  print(basename(lst))

  dat <- rbindlist(lapply(lst, read.csv,
                          col.names = paste0("V", 1:34), header = F, check.names = F),
                   fill = T)

  #deal with potential white space for some reason..
  cols_to_be_rectified <- names(dat)[vapply(dat, is.character, logical(1))]
  dat[, cols_to_be_rectified] <- lapply(dat[, cols_to_be_rectified, with = F], trimws)

  #re-code
  dat$V12[dat$V12 == "L"] <- "LAN"
  dat$V12[dat$V12 == "D"] <- "DIS"
  dat$V12[dat$V12 == "B"] <- "BMS"

  ##intercatch naming
  hi_names <- c("RecordType", "Country", "Year", "SeasonType", "Season",
                "Fleet", "AreaType", "FishingArea", "DepthRange", "UnitEffort",
                "Effort", "AreaQualifier")

  si_names <- c("RecordType","Country","Year", "SeasonType", "Season", "Fleet",
                "AreaType", "FishingArea", "DepthRange", "Species", "Stock_orig",
                "CatchCategory","ReportingCategory","DataToForm","Usage",
                "SamplesOrigin", "QualityFlag", "UnitCaton", "Caton", "OffLandings",
                "VarCaton", "InfoFleet", "InfoStockCoordinator", "InfoGeneral")

  sd_names <- c("RecordType","Country","Year","SeasonType","Season","Fleet",
                "AreaType","FishingArea","DepthRange","Species","Stock_orig",
                "CatchCategory","ReportingCategory","Sex","CANUMtype",
                "AgeLength","PlusGroup","SampledCatch","NumSamplesLngt",
                "NumLngtMeas","NumSamplesAge","NumAgeMeas","unitMeanWeight",
                "unitCANUM","UnitAgeOrLength","UnitMeanLength","Maturity",
                "NumberCaught","MeanWeight","MeanLength","varNumLanded",
                "varWgtLanded","varLgtLanded")


  # split into intercatch formats
  hi <- dat[dat$V1 == "HI", 1:length(hi_names)]
  si <- dat[dat$V1 == "SI", 1:length(si_names)]
  sd <- dat[dat$V1 == "SD", 1:length(sd_names)]

  names(hi) <- hi_names
  names(si) <- si_names
  names(sd) <- sd_names

  ###********************************** create Census catches from SI *********************************

  # set the match key
  hi$key = paste(hi$Country,
                 hi$Year,
                 hi$Season,
                 hi$FishingArea,
                 hi$Fleet,
                 hi$UnitEffort,
                 sep = "_")

  si$key = paste(si$Country,
                 si$Year,
                 si$Season,
                 si$Species,
                 si$FishingArea,
                 si$Fleet,
                 si$CatchCategory,
                 sep = "_")

  sd$key = paste(sd$Country,
                 sd$Year,
                 sd$Season,
                 sd$Species,
                 sd$FishingArea,
                 sd$Fleet,
                 sd$CatchCategory,
                 sep = "_")

  #clean up SI
  si <- si[! (si$Caton == 0 & si$CatchCategory == "LAN"), ]
  si <- si[!duplicated(si$key), ] # [YR] Shouldn't there be an error or aggregation instead?

  #merge with code list for the stock area and wg
  stock_relation$FishingArea <- stock_relation$ICESArea
  si <- merge(si, stock_relation, by = c("Species", "FishingArea"))


  # create domains, and fill in discard domain for covered landings domains.
  si$key2 = paste(si$Country,
                  si$Year,
                  si$Season,
                  si$Species,
                  si$FishingArea,
                  si$Fleet, # Without CatchCategory (for matching across them).
                  sep = "_")
  tbl <- unique(si[, c("key2", "CatchCategory")])

  ## Used keys per catch category:
  has_categ <- sapply(c("DIS", "Logbook Registered Discard", "BMS"),
                      function(cat)
               {
                   tbl2 <- tbl[tbl$CatchCategory %in% c(cat, "LAN"), ]
                   tbl2[duplicated(key2), ]$key2
               })

  si$domain = paste(si$Season,
                    si$FishingArea,
                    si$Fleet,
                    sep = "_")

  ## create columns for format
  si$domainCatchDis <- ifelse((si$key2 %in% unlist(has_categ[c("DIS", "Logbook Registered Discard")]) &
                               si$CatchCategory == "LAN") |
                              si$CatchCategory %in% c("DIS", "Logbook Registered Discard"),
                              si$domain, "")

  si$domainCatchBMS <- ifelse((si$key2 %in% has_categ[["BMS"]] &
                               si$CatchCategory == "LAN") |
                              si$CatchCategory %in% c("BMS"),
                              si$domain, "")

  si$domainBiology <- ifelse(si$key %in% sd$key,
                             si$domain, "")


  si$quarter <- ifelse(si$SeasonType == "Quarter",
                       si$Season, NA)

  si <- si[order(si$key), ]

  #make final table
  census_catches <- data.frame(VesselFlagCountry = si$Country,
                               year = si$Year,
                               workingGroup = si$EG,
                               stock = si$StockCode,
                               speciesCode = si$speciesCode,
                               catchCategory	= si$CatchCategory,
                               quarter = si$quarter,
                               area = si$FishingArea,
                               fisheriesManagementUnit = NA,
                               metier6 = NA,
                               fleet = si$Fleet,
                               domainCatchDis = si$domainCatchDis,
                               domainCatchBMS = si$domainCatchBMS,
                               domainBiology = si$domainBiology,
                               variableType = "ScientificWeight_kg",
                               total = ifelse(si$CatchCategory == "LAN",
                                              si$Caton, NA),
                               comment = si$InfoStockCoordinator
  )

  if (!is.null(metier6) && tolower(metier6) == "fleet")
  {
      census_catches <- census_catches %>%
          dplyr::mutate(metier6 = fleet)
  }

  ###********************************** create Estimated catches from SI *********************************
  est <- si[si$CatchCategory != "LAN", ]

  #get number samples from sd
  xx <- unique(sd[sd$CatchCategory != "LAN", c("NumSamplesLngt", "key")])
  est <- merge(est, xx, by = "key", all.x = T)
  est$NumSamplesLngt[is.na(est$NumSamplesLngt)] <- 0

  #make final table
  estimated_catches <- data.frame(VesselFlagCountry = est$Country,
                                  year = est$Year,
                                  workingGroup = est$EG,
                                  stock = est$StockCode,
                                  speciesCode = est$speciesCode,
                                  catchCategory	= est$CatchCategory,
                                  domainCatch = est$domain,
                                  variableType = "ScientificWeight_kg",
                                  total = est$Caton,
                                  mean = NA,
                                  varianceTotal = NA,
                                  varianceMean = NA,
                                  PSUtype = NA,
                                  numPSU = est$NumSamplesLngt,
                                  numTrips = est$NumSamplesLngt
  )

  estimated_catches <- estimated_catches[order(estimated_catches$domainCatch), ]

  ###**************************** create length age and whatever distribution from sd *****************
  sd <- merge(sd, stock_relation, by = c("Species", "FishingArea"))

  #recode / create costum columns
  sd$bvType <- ifelse(tolower(sd$CANUMtype) == "age", "Age", "Length")
  sd$ageType <- ifelse(sd$bvType == "Age", "ageyear", "")
  sd$numPSUs <- ifelse(sd$bvType == "Age", sd$NumSamplesAge, sd$NumSamplesLngt)
  sd$numMeasurements <- ifelse(sd$bvType == "Age", sd$NumAgeMeas, sd$NumLngtMeas)

  sd$domain = paste(sd$Season,
                    sd$FishingArea,
                    sd$Fleet,
                    sep = "_")

  #make final table
  distributions = data.frame(VesselFlagCountry = sd$Country,
                             year = sd$Year,
                             workingGroup = sd$EG,
                             stock = sd$StockCode,
                             speciesCode = sd$speciesCode,
                             catchCategory	= sd$CatchCategory,
                             domainBiology = sd$domain,
                             fishDomain = "",
                             bvType = sd$bvType,
                             bvValue	= sd$AgeLength,
                             AgeType = sd$ageType,
                             AgeGroupPlus = "",
                             variableType = "",
                             total = sd$NumberCaught,
                             mean = sd$MeanWeight,
                             varianceTotal = NA,
                             varianceMean = NA,
                             PSUtype = "",
                             numPSUs = sd$numPSUs,
                             numTrips = sd$numPSUs,
                             numMeasurements = sd$numMeasurements)

  # stack total and mean into their own lines
  distributions <- rbind(distributions, distributions)
  distributions$variableType <- rep(c("Number", "WeightLive"),
                                    each = nrow(distributions)/2)

  distributions[distributions$variableType == "Number", "mean"] <- NA
  distributions[distributions$variableType != "Number", "total"] <- NA

  distributions <- distributions[order(distributions$domainBiology),]

  ###**************************** effort data from HI *****************
  setDT(hi)
  hi <- hi[! is.na(hi$UnitEffort) & !duplicated(hi$key), ]

  #code translate
  hi$quarter <- ifelse(hi$SeasonType == "Quarter",
                       hi$Season, NA)

  hi <- merge(hi, effort_relation, by = "UnitEffort")

  # sum by key
  hi$Effort <- as.numeric(hi$Effort)
  hi <- hi[ ,. (total = sum(Effort)),
            by = .(Country, Year, quarter, Season, FishingArea,
                   Fleet, variableType, key)]

  #add aditional information
  area_wg <- stock_relation[stock_relation$EG %in% sd$EG, ]
  area_wg <- unique(area_wg[, c("FishingArea", "EG")])

  hi <- merge(hi, area_wg, by = c("FishingArea"))

  #order and output format
  hi <- hi[order(hi$key), ]
  effort <- data.frame(vesselFlagCountry = hi$Country,
                       year = hi$Year,
                       workingGroup = hi$EG,
                       quarter = hi$Season,
                       area = hi$FishingArea,
                       fisheriesManagementUnit = "",
                       metier6 = "",
                       fleet = hi$Fleet,
                       variableType = hi$variableType,
                       total = hi$total
  )

  if (!is.null(metier6) && tolower(metier6) == "fleet")
  {
      effort <- effort %>%
          dplyr::mutate(metier6 = fleet)
  }



  ###**************************** output results to environment or file *****************
  if ("to_environment" %in% output_format){

    assign("census_catches", census_catches, .GlobalEnv)
    assign("estimated_catches", estimated_catches, .GlobalEnv)
    assign("distributions", distributions, .GlobalEnv)
    assign("effort", effort, .GlobalEnv)

  }
  if ("to_file" %in% output_format){

    write.csv(census_catches, paste0(out_path, "/census_catches.csv"),
              row.names = F, quote = F)
    write.csv(estimated_catches, paste0(out_path, "/estimated_catches.csv"),
              row.names = F, quote = F)
    write.csv(distributions, paste0(out_path, "/distributions.csv"),
              row.names = F, quote = F)
    write.csv(effort, paste0(out_path, "/effort.csv"),
              row.names = F, quote = F)
  }

  if("to_list" %in% output_format) {
   return( list("census_catches" = census_catches,
                "estimated_catches"= estimated_catches,
                "distributions"= distributions,
                "effort"= effort))
  }
}
