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
    options("RCEF_version" = "16.0")

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

    ## clean up SI
    
    ## si <- si[! (si$Caton == 0 & si$CatchCategory == "LAN"), ] # This create inconsistencies such as BMS or discards
    ##                                     # without corresponding landings (=0)
    if (any(duplicated(si$key))) warning("\n\n## ", sum(duplicated(si$key)), " dupplicated keys!")
    si <- si[!duplicated(si$key), ] # [YR] Shouldn't there be an error or aggregation instead?

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

###********************************** combined census and estimated catches from SI *********************************
    ## make final table:
    xx <- unique(sd[sd$CatchCategory != "LAN", c("NumSamplesLngt", "key")])
    si <- merge(si, xx, by = "key", all.x = T)

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
                          metier6 = NA_character_,
                          fleetType = "WGFleet",
                          fleetValue = si$Fleet,
                          domainCatchDis = si$domainCatchDis,
                          domainCatchBMS = si$domainCatchBMS,
                          domainBiology = si$domainBiology,
                          variableUnit = "kg",
                          WGWeight = as.numeric(si$Caton),
                          OfficialWeight = as.numeric(si$OffLandings),
                          mean = NA,
                          varianceTotal = NA,
                          varianceMean = NA,
                          PSUtype = NA,
                          numPSUs = si$NumSamplesLngt,
                          numTrips = si$NumSamplesLngt,
                          comment = si$InfoStockCoordinator) %>%
        dplyr::mutate(domainCatchDis = ifelse(domainCatchDis %in% "", NA, domainCatchDis),
                      domainCatchBMS = ifelse(domainCatchBMS %in% "", NA, domainCatchBMS),
                      domainBiology = ifelse(domainBiology %in% "", NA, domainBiology),
                      PSUtype = ifelse(is.na(PSUtype) & !is.na(numPSUs) & !is.na(numTrips) & numPSUs == numTrips,
                                       "fishing trip", PSUtype)) %>%
        tidyr::pivot_longer(WGWeight:OfficialWeight, names_to = "variableType", values_to = "total") %>%
        filter(!is.na(total)) %>%
        dplyr::relocate(comment, .after = last_col())
        

    if (!is.null(metier6) && tolower(metier6) == "fleet")
    {
        catches <- catches %>%
            dplyr::mutate(metier6 = fleetValue)
    }else{
        if (is.null(catches$metier6))
        {
            catches$metier6 <- NA_character_
        }
    }

    catches <- catches %>% dplyr::relocate(metier6, .before = fleetType) %>%
        dplyr::arrange(across(vesselFlagCountry:speciesCode),
                       across(seasonType:metier6),
                       variableType, catchCategory)

    ## catches %>% group_by(variableType, catchCategory) %>% slice_sample(n = 2) %>% as.data.frame()

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

    ## make final table
    distributions <- data.frame(vesselFlagCountry = sd$Country,
                                year = sd$Year,
                                workingGroup = sd$EG,
                                stock = sd$StockCode,
                                speciesCode = sd$speciesCode,
                                catchCategory	= sd$CatchCategory,
                                domainBiology = sd$domain,
                                bvType = sd$CANUMtype,
                                bvUnit = sd$UnitAgeOrLength,
                                bvValue	= sd$AgeLength,
                                ## AgeType = sd$ageType,
                                AgeGroupPlus = as.numeric(sd$PlusGroup),
                                attributeType = "sex",
                                attibuteValue = sd$Sex,
                                Number.variableUnit = sd$unitCANUM,
                                Number.value = as.numeric(sd$NumberCaught),
                                Number.variance = as.numeric(sd$varNumLanded),
                                WeightLive.variableUnit = sd$unitMeanWeight,
                                WeightLive.value = as.numeric(sd$MeanWeight),
                                WeightLive.variance = as.numeric(sd$varWgtLanded),
                                MeanLength.variableUnit = sd$UnitMeanLength,
                                MeanLength.value = as.numeric(sd$MeanLength),
                                MeanLength.variance = as.numeric(sd$varLgtLanded),
                                PSUtype = NA,
                                numPSUs = as.numeric(sd$numPSUs),
                                numTrips = as.numeric(sd$numPSUs),
                                numMeasurements = sd$numMeasurements) %>%
        dplyr::mutate(domainBiology = ifelse(domainBiology %in% "", NA, domainBiology)) %>%
        tidyr::pivot_longer(Number.variableUnit:MeanLength.variance,
                            names_to = c("variableType", ".value"),
                            names_sep = "[.]",
                            values_drop_na = TRUE) %>%
        mutate(valueType = case_when(variableType %in% c("Number") ~ "Total",
                                     variableType %in% c("WeightLive", "MeanLength") ~ "Mean",
                                     TRUE ~ NA_character_),
               PSUtype = ifelse(is.na(PSUtype) & !is.na(numPSUs) & !is.na(numTrips) & numPSUs == numTrips,
                                "fishing trip", PSUtype),
               dplyr::across(where(is.numeric), ~dplyr::na_if(.x, -9)),
               variableUnit = case_when(variableUnit %in% c("K") ~ "1000_pcs", # Need to complete mapping!
                                        TRUE ~ variableUnit)) %>%
        filter(! is.na(value)) %>%
        dplyr::arrange(across(vesselFlagCountry:domainBiology), bvType, variableType, bvValue)

    ## distributions %>% group_by(variableType, catchCategory) %>% slice_sample(n = 1) %>% as.data.frame()
    ## distributions %>% group_by(catchCategory) %>% slice_sample(n = 1) %>% as.data.frame()

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
                         metier6 = "",
                         fleetType = "WGFleet",
                         fleetValue = hi$Fleet,
                         variableType = hi$variableType,
                         total = as.numeric(hi$total)
                         )

    if (!is.null(metier6) && tolower(metier6) == "fleet")
    {
        effort <- effort %>%
            dplyr::mutate(metier6 = fleetValue)
    }



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
