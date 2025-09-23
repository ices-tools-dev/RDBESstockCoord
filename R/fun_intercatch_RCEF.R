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
                         out_path = getwd(),
                         file_prefix = "")
{

    ## fixed relations
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

    ## deal with potential white space for some reason..
    cols_to_be_rectified <- names(dat)[vapply(dat, is.character, logical(1))]
    dat[, cols_to_be_rectified] <- lapply(dat[, cols_to_be_rectified, with = F], trimws)

    ## re-code
    dat$V12[dat$V12 == "L"] <- "LAN"
    dat$V12[dat$V12 == "D"] <- "DIS"
    dat$V12[dat$V12 == "B"] <- "BMS"
    dat$V12[dat$V12 == "Logbook Registered Discard"] <- "RegDIS"

    table(dat$V12)

    ## intercatch naming
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


    ## split into intercatch formats
    hi <- dat[dat$V1 == "HI", 1:length(hi_names)]
    si <- dat[dat$V1 == "SI", 1:length(si_names)]
    sd <- dat[dat$V1 == "SD", 1:length(sd_names)]

    names(hi) <- hi_names
    names(si) <- si_names
    names(sd) <- sd_names

###********************************** create Census catches from SI *********************************

    ## set the match key
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
    has_categ <- sapply(c("DIS", "RegDIS", "BMS"),
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
    si$domainCatchDis <- ifelse((si$key2 %in% unlist(has_categ[c("DIS", "RegDIS")]) &
                                 si$CatchCategory == "LAN") |
                                si$CatchCategory %in% c("DIS", "RegDIS"),
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
   
    ## make final table
    census_catches <- data.frame(vesselFlagCountry = si$Country,
                                 year = si$Year,
                                 workingGroup = si$EG,
                                 stock = si$StockCode,
                                 speciesCode = si$speciesCode,
                                 catchCategory	= si$CatchCategory,
                                 seasonType = ifelse(is.na(as.numeric(si$quarter)),
                                                     "Year", "Quarter"),
                                 seasonValue = ifelse(is.na(as.numeric(si$quarter)),
                                                      as.numeric(si$year),
                                                      as.numeric(si$quarter)),
                                 areaType = "ICESArea",
                                 areaValue = si$FishingArea,
                                 fleetType = "WGFleet",
                                 fleetValue = si$Fleet,
                                 metier6 = si$Fleet,
                                 fisheriesManagementUnit = NA,
                                 domainCatchDis = si$domainCatchDis,
                                 domainCatchBMS = si$domainCatchBMS,
                                 domainBiology = si$domainBiology,
                                 variableTypeUnit = "kg",
                                 WGWeight = as.numeric(ifelse(si$CatchCategory == "LAN",
                                                              si$Caton, NA)),
                                 OfficialWeight = as.numeric(ifelse(si$CatchCategory == "LAN",
                                                                    si$OffLandings, NA)),
                                 comment = si$InfoStockCoordinator) %>%
        dplyr::mutate(domainCatchDis = ifelse(domainCatchDis %in% "", NA, domainCatchDis),
                      domainCatchBMS = ifelse(domainCatchBMS %in% "", NA, domainCatchBMS),
                      domainBiology = ifelse(domainBiology %in% "", NA, domainBiology)) %>%
        tidyr::pivot_longer(WGWeight:OfficialWeight, names_to = "variableType", values_to = "total") %>%
        filter(!is.na(total))

    ## if (!is.null(metier6) && tolower(metier6) == "fleet")
    ## {
    ##     census_catches <- census_catches %>%
    ##         dplyr::mutate(metier6 = fleet)
    ## }

    ## get number samples from sd
    xx <- unique(sd[sd$CatchCategory != "LAN", c("NumSamplesLngt", "key")])
    si <- merge(si, xx, by = "key", all.x = T)

    catches <- data.frame(vesselFlagCountry = si$Country,
                          year = si$Year,
                          workingGroup = si$EG,
                          stock = si$StockCode,
                          speciesCode = si$speciesCode,
                          catchCategory	= sub("Logbook Registered Discard", "DIS", si$CatchCategory),
                          seasonType = ifelse(is.na(as.numeric(si$quarter)),
                                              "Year", "Quarter"),
                          seasonValue = ifelse(is.na(as.numeric(si$quarter)),
                                               as.numeric(si$Year),
                                               as.numeric(si$quarter)),
                          areaType = "ICESArea",
                          areaValue = si$FishingArea,
                          fisheriesManagementUnit = NA,
                          metier6 = si$Fleet,
                          fleetType = "WGFleet",
                          fleetValue = si$Fleet,
                          domainCatchDis = si$domainCatchDis,
                          domainCatchBMS = si$domainCatchBMS,
                          domainBiology = si$domainBiology,
                          variableTypeUnit = "kg",
                          WGWeight = as.numeric(si$Caton),
                          OfficialWeight = as.numeric(si$OffLandings),
                          mean = NA,
                          varianceTotal = NA,
                          varianceMean = NA,
                          PSUtype = NA,
                          numPSU = si$NumSamplesLngt,
                          numTrips = si$NumSamplesLngt,
                          comment = si$InfoStockCoordinator) %>%
        dplyr::mutate(domainCatchDis = ifelse(domainCatchDis %in% "", NA, domainCatchDis),
                      domainCatchBMS = ifelse(domainCatchBMS %in% "", NA, domainCatchBMS),
                      domainBiology = ifelse(domainBiology %in% "", NA, domainBiology)) %>%
        tidyr::pivot_longer(WGWeight:OfficialWeight, names_to = "variableType", values_to = "total") %>%
        filter(!is.na(total)) %>%
        dplyr::relocate(comment, .after = last_col())
        

    ## if (!is.null(metier6) && tolower(metier6) == "fleet")
    ## {
    ##     catches <- catches %>%
    ##         dplyr::mutate(metier6 = fleet)
    ## }

    catches %>% group_by(variableType, catchCategory) %>% slice_sample(n = 2) %>% as.data.frame()

###********************************** create Estimated catches from SI *********************************
    est <- si[si$CatchCategory != "LAN", ]

    ## get number samples from sd
    xx <- unique(sd[sd$CatchCategory != "LAN", c("NumSamplesLngt", "key")])
    est <- merge(est, xx, by = "key", all.x = T)
    est$NumSamplesLngt[is.na(est$NumSamplesLngt)] <- 0

                                        #make final table
    estimated_catches <- data.frame(vesselFlagCountry = est$Country,
                                    year = est$Year,
                                    workingGroup = est$EG,
                                    stock = est$StockCode,
                                    speciesCode = est$speciesCode,
                                    catchCategory	= est$CatchCategory,
                                    domainCatch = est$domain,
                                    ## total = as.numeric(est$Caton),
                                    variableTypeUnit = "kg",
                                    WGWeight = as.numeric(est$Caton),
                                    OfficialWeight = as.numeric(est$OffLandings),
                                    mean = NA,
                                    varianceTotal = NA,
                                    varianceMean = NA,
                                    PSUtype = NA,
                                    numPSU = est$NumSamplesLngt,
                                    numTrips = est$NumSamplesLngt) %>%
        dplyr::mutate(domainCatch = ifelse(domainCatch %in% "", NA, domainCatch)) %>%
        tidyr::pivot_longer(WGWeight:OfficialWeight, names_to = "variableType", values_to = "total") %>%
        filter(!is.na(total))

    estimated_catches %>% group_by(catchCategory, variableType) %>% slice_sample(n = 2) %>% as.data.frame()

    estimated_catches <- estimated_catches[order(estimated_catches$domainCatch), ]

###**************************** create length age and whatever distribution from sd *****************
    sd <- merge(sd, stock_relation, by = c("Species", "FishingArea"))

    ## recode / create costum columns
    sd$bvType <- ifelse(tolower(sd$CANUMtype) == "age", "Age", "Length")
    sd$ageType <- ifelse(sd$bvType == "Age", "ageyear", "")
    sd$numPSUs <- ifelse(sd$bvType == "Age", sd$NumSamplesAge, sd$NumSamplesLngt)
    sd$numMeasurements <- ifelse(sd$bvType == "Age", sd$NumAgeMeas, sd$NumLngtMeas)

    sd$domain = paste(sd$Season,
                      sd$FishingArea,
                      sd$Fleet,
                      sep = "_")
    ## browser()

    head(sd, 2)

    ## make final table
    distributions = data.frame(vesselFlagCountry = sd$Country,
                               year = sd$Year,
                               workingGroup = sd$EG,
                               stock = sd$StockCode,
                               speciesCode = sd$speciesCode,
                               catchCategory	= sd$CatchCategory,
                               domainBiology = sd$domain,
                               fishDomain = "",
                               bvType = sd$CANUMtype,
                               bvTypeUnit = sd$UnitAgeOrLength,
                               bvValue	= sd$AgeLength,
                               ## AgeType = sd$ageType,
                               AgeGroupPlus = sd$PlusGroup,
                               variableType = "",
                               total = as.numeric(sd$NumberCaught), # Need mean weight and length as well.
                               mean = as.numeric(sd$MeanWeight),
                               varianceTotal = NA,
                               varianceMean = NA,
                               PSUtype = "",
                               numPSUs = sd$numPSUs,
                               numTrips = sd$numPSUs,
                               numMeasurements = sd$numMeasurements) %>%
        dplyr::mutate(domainBiology = ifelse(domainBiology %in% "", NA, domainBiology))

    ## stack total and mean into their own lines
    distributions <- rbind(distributions, distributions)
    distributions$variableType <- rep(c("Number", "WeightLive"),
                                      each = nrow(distributions)/2)

    distributions[distributions$variableType == "Number", "mean"] <- NA
    distributions[distributions$variableType != "Number", "total"] <- NA

    distributions <- distributions[order(distributions$domainBiology),]

    distributions2 <- data.frame(vesselFlagCountry = sd$Country,
                                 year = sd$Year,
                                 workingGroup = sd$EG,
                                 stock = sd$StockCode,
                                 speciesCode = sd$speciesCode,
                                 catchCategory	= sd$CatchCategory,
                                 domainBiology = sd$domain,
                                 fishDomain = "",
                                 bvType = sd$CANUMtype,
                                 bvTypeUnit = sd$UnitAgeOrLength,
                                 bvValue	= sd$AgeLength,
                                 ## AgeType = sd$ageType,
                                 AgeGroupPlus = sd$PlusGroup,
                                 Number.variableTypeUnit = sd$unitCANUM,
                                 Number.value = sd$NumberCaught,
                                 Number.variance = sd$varNumLanded,
                                 WeightLive.variableTypeUnit = sd$unitMeanWeight,
                                 WeightLive.value = sd$MeanWeight,
                                 WeightLive.variance = sd$varWgtLanded,
                                 MeanLength.variableTypeUnit = sd$UnitMeanLength,
                                 MeanLength.value = sd$MeanLength,
                                 MeanLength.variance = sd$varLgtLanded,
                                 PSUtype = "",
                                 numPSUs = sd$numPSUs,
                                 numTrips = sd$numPSUs,
                                 numMeasurements = sd$numMeasurements) %>%
        dplyr::mutate(domainBiology = ifelse(domainBiology %in% "", NA, domainBiology)) %>%
        tidyr::pivot_longer(Number.variableTypeUnit:MeanLength.variance,
                            names_to = c("variableType", ".value"),
                            names_sep = "[.]",
                            values_drop_na = TRUE) %>%
        mutate(valueType = case_when(variableType %in% c("Number") ~ "Total",
                                     variableType %in% c("WeightLive", "MeanLength") ~ "Mean",
                                     TRUE ~ NA_character_)) %>%
        filter(! is.na(value))

    ## distributions2 %>% group_by(variableType, catchCategory) %>% slice_sample(n = 1) %>% as.data.frame()

###**************************** effort data from HI *****************
    setDT(hi)
    hi <- hi[! is.na(hi$UnitEffort) & !duplicated(hi$key), ]

    ## code translate
    hi$quarter <- ifelse(hi$SeasonType == "Quarter",
                         hi$Season, NA)

    hi <- merge(hi, effort_relation, by = "UnitEffort")

    ## sum by key
    hi$Effort <- as.numeric(hi$Effort)
    hi <- hi[ ,. (total = sum(Effort)),
             by = .(Country, Year, quarter, Season, FishingArea,
                    Fleet, variableType, key)]

    ## add aditional information
    area_wg <- stock_relation[stock_relation$EG %in% sd$EG, ]
    area_wg <- unique(area_wg[, c("FishingArea", "EG")])

    hi <- merge(hi, area_wg, by = c("FishingArea"))

                                        #order and output format
    hi <- hi[order(hi$key), ]
    effort <- data.frame(vesselFlagCountry = hi$Country,
                         year = hi$Year,
                         workingGroup = hi$EG,
                         quarter = as.numeric(hi$Season),
                         area = hi$FishingArea,
                         fisheriesManagementUnit = "",
                         metier6 = "",
                         fleet = hi$Fleet,
                         variableType = hi$variableType,
                         total = as.numeric(hi$total)
                         )

    if (!is.null(metier6) && tolower(metier6) == "fleet")
    {
        effort <- effort %>%
            dplyr::mutate(metier6 = fleet)
    }



###**************************** output results to environment or file *****************
    if ("to_environment" %in% output_format)
    {
        assign("catches", catches, .GlobalEnv)
        assign("census_catches", census_catches, .GlobalEnv)
        assign("estimated_catches", estimated_catches, .GlobalEnv)
        assign("distributions", distributions2, .GlobalEnv)
        assign("effort", effort, .GlobalEnv)

    }
    if ("to_file" %in% output_format)
    {
        write.csv(catches,
                  file = file.path(out_path, paste0(file_prefix, "catches_RCEF_v15.csv")),
                  row.names = FALSE, quote = FALSE,  na = "")
        write.csv(census_catches,
                  file = file.path(out_path, paste0(file_prefix, "census_catches.csv")),
                  row.names = FALSE, quote = FALSE,  na = "")
        write.csv(estimated_catches,
                  file = file.path(out_path, paste0(file_prefix, "estimated_catches.csv")),
                  row.names = FALSE, quote = FALSE,  na = "")
        write.csv(distributions, 
                  file = file.path(out_path, paste0(file_prefix, "distributions.csv")),
                  row.names = FALSE, quote = FALSE,  na = "")
        write.csv(distributions2, 
                  file = file.path(out_path, paste0(file_prefix, "distributions_RCEF_v15.csv")),
                  row.names = FALSE, quote = FALSE,  na = "")
        write.csv(effort, 
                  file = file.path(out_path, paste0(file_prefix, "effort.csv")),
                  row.names = FALSE, quote = FALSE,  na = "")
    }

    if("to_list" %in% output_format)
    {
        return(list("census_catches" = census_catches,
                    "estimated_catches"= estimated_catches,
                    "distributions"= distributions,
                    "effort"= effort))
    }
}
