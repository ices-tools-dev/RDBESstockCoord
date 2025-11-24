#' InterCatch exchange format to RCEF v16
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
                         file_prefix = "",
                         file_suffix = paste0("_RCEF_v", getOption("RCEF_version"))) # Delayed evaluation makes it okay!
{
    options("RCEF_version" = "17.0")

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
    dat$V12[dat$V12 %in% "L"] <- "LAN"
    dat$V12[dat$V12 %in% "D"] <- "DIS"
    dat$V12[dat$V12 %in% "B"] <- "BMS"
    dat$V12[dat$V12 %in% "Logbook Registered Discard"] <- "RegDIS"

    ## table(dat$V12)

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
                  "NumLngtMeas","NumSamplesAge","NumAgeMeas","UnitMeanWeight",
                  "UnitCANUM","UnitAgeOrLength","UnitMeanLength","Maturity",
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

    ## clean up SI
    
    if (any(duplicated(si$key))) 
      warning("\n\n## ", sum(duplicated(si$key)), " dupplicated keys!")
    
    si <- si[!duplicated(si$key), ] # [YR] Shouldn't there be an error or aggregation instead?
                                    # [JS] probably, but for me it is mos often an artifact of length and age files with overlapping information

    ## merge with code list for the stock area and wg
    stock_relation$FishingArea <- stock_relation$ICESArea
    si <- merge(si, stock_relation, by = c("Species", "FishingArea"))


    ## create domains, and fill in discard domain for covered landings domains.
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
                 }, simplify = FALSE)

    si$domain = paste(si$Season,
                      si$FishingArea,
                      si$Fleet,
                      sep = "_")

    ## create columns for format
    si$domainCatchDis <- ifelse((si$key2 %in% unlist(has_categ[c("DIS", "RegDIS")]) &
                                 si$CatchCategory == "LAN") |
                                si$CatchCategory %in% c("DIS", "RegDIS"),
                                si$domain, NA)

    si$domainCatchBMS <- ifelse((si$key2 %in% has_categ[["BMS"]] &
                                 si$CatchCategory == "LAN") |
                                si$CatchCategory %in% c("BMS"),
                                si$domain, NA)

    si$domainBiology <- ifelse(si$key %in% sd$key,
                               si$domain, NA)


    si$quarter <- ifelse(si$SeasonType == "Quarter",
                         si$Season, NA)

    si <- si[order(si$key), ]

###********************************** combined census and estimated catches from SI *********************************
    ## make final table:
    xx <- unique(sd[sd$CatchCategory != "LAN", c("NumSamplesLngt", "key")])
    si <- merge(si, xx, by = "key", all.x = T)

    si$PSUtype = ifelse(!is.na(si$NumSamplesLngt),
                     "fishing trip", NA)
    
    si <- melt(si, measure.vars = c("Caton", "OffLandings"))
    
    catches <- data.frame(vesselFlagCountry = si$Country,
                          year = si$Year,
                          workingGroup = si$EG,
                          stock = si$StockCode,
                          speciesCode = si$speciesCode,
                          catchCategory	= sub("Logbook Registered Discard|RegDIS", "DIS", si$CatchCategory),
                          seasonType = ifelse(is.na(as.numeric(si$quarter)),
                                              "Year", "Quarter"),
                          seasonValue = ifelse(is.na(as.numeric(si$quarter)),
                                               as.numeric(si$Year),
                                               as.numeric(si$quarter)),
                          areaType = "ICESArea",
                          areaValue = si$FishingArea,
                          fisheriesManagementUnit = NA,
                          metier6 = "MIS_MIS_0_0_0",
                          fleetType = "WGFleet",
                          fleetValue = si$Fleet,
                          domainCatchDis = si$domainCatchDis,
                          domainCatchBMS = si$domainCatchBMS,
                          domainBiology = si$domainBiology,
                          sourceType = ifelse(si$variable == "Caton", "WGValue", "Official"),
                          variableType = "WeightLive",
                          variableUnit = si$UnitCaton,
                          total = si$value,
                          variance = NA,
                          PSU = si$PSUtype,
                          numPSUs = si$NumSamplesLngt,
                          numTrips = si$NumSamplesLngt,
                          comment = si$InfoStockCoordinator)
        

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

    #stack the data
    sd <- melt(sd, measure.vars = c("NumberCaught", "MeanWeight", "MeanLength"),
                variable.name = "valueType", value.name = "value",
                variable.factor = F , na.rm = T)
    
    sd <- sd[sd$value != "-9", ]
    
    #rename
    sd$unit <- ifelse(sd$valueType == "NumberCaught", sd$UnitCANUM,
                      ifelse(sd$valueType == "MeanWeight", sd$UnitMeanWeight,
                      sd$UnitMeanLength))
    
    sd[sd$valueType == "NumberCaught", "variableType"] <- "Number"
    sd[sd$valueType == "MeanWeight", "variableType"] <- "WeightLive"
    sd[sd$valueType == "MeanLength", "variableType"] <- "MeanLength"

    sd$PSUtype = ifelse(!is.na(sd$numPSUs),
                        "fishing trip", NA)
    
    ## make final table
    distributions <- data.frame(vesselFlagCountry = sd$Country,
                                year = sd$Year,
                                workingGroup = sd$EG,
                                stock = sd$StockCode,
                                speciesCode = sd$speciesCode,
                                catchCategory	= sd$CatchCategory,
                                domainBiology = sd$domain,
                                distributionType = sd$CANUMtype,
                                distributionUnit = sd$UnitAgeOrLength,
                                distributionValue	= sd$AgeLength,
                                ageGroupPlus = ifelse(sd$PlusGroup == "-9", NA, 
                                                      as.numeric(sd$PlusGroup)),
                                attributeType = "sex",
                                attibuteValue = sd$Sex,
                                variableType = sd$variableType,
                                variableUnit = ifelse(sd$value == "k", 
                                                      "1000_pcs", sd$unit),
                                valueType = ifelse(sd$valueType %like% "Mean", 
                                                   "Mean", "Totoal"),
                                value = sd$value,
                                variance = NA,
                                PSUtype = sd$PSUtype,
                                numPSUs = as.numeric(sd$numPSUs),
                                numTrips = as.numeric(sd$numPSUs),
                                numMeasurements = sd$numMeasurements)
    

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
                         seasonType = ifelse(is.na(as.numeric(hi$quarter)),
                                             "Year", "Quarter"),
                         seasonValue = ifelse(is.na(as.numeric(hi$quarter)),
                                              as.numeric(hi$Year),
                                              as.numeric(hi$quarter)),
                         areaType = "ICESArea",
                         areaValue = hi$FishingArea,
                         fisheriesManagementUnit = "",
                         metier6 = "MIS_MIS_0_0_0",
                         fleetType = "WGFleet",
                         fleetValue = hi$Fleet,
                         variableType = hi$variableType,
                         total = as.numeric(hi$total)
                         )

    
###**************************** output results to environment or file *****************
    if ("to_environment" %in% output_format)
    {
        attr(catches, "RCEF_version") <- getOption("RCEF_version")
        attr(distributions, "RCEF_version") <- getOption("RCEF_version")
        attr(effort, "RCEF_version") <- getOption("RCEF_version")
        
        assign("catches", catches, .GlobalEnv)
        assign("distributions", distributions, .GlobalEnv)
        assign("effort", effort, .GlobalEnv)

    }
    if ("to_file" %in% output_format)
    {
        write.csv(catches,
                  file = file.path(out_path,
                                   paste0(file_prefix, "catches", file_suffix, ".csv")),
                  row.names = FALSE, quote = FALSE,  na = "")
        write.csv(distributions, 
                  file = file.path(out_path,
                                   paste0(file_prefix, "distributions", file_suffix, ".csv")),
                  row.names = FALSE, quote = FALSE,  na = "")
        write.csv(effort, 
                  file = file.path(out_path,
                                   paste0(file_prefix, "effort", file_suffix, ".csv")),
                  row.names = FALSE, quote = FALSE,  na = "")
    }

    if("to_list" %in% output_format)
    {
        res <- list("catches" = catches,
                    "distributions"= distributions,
                    "effort"= effort)
        attr(res, "RCEF_version") <- getOption("RCEF_version")
        
        return(res)
    }
}
