#' Convert InterCatch Exchange Format to RCEF (Regional Coordination of Fisheries Data)
#'
#' This function converts fisheries data from the InterCatch exchange format
#' (HI, SI, SD records) into the RCEF format, producing four main output tables:
#' census catches, estimated catches, biological distributions, and effort data.
#' This is an updated version with tidyverse integration and metier6 handling.
#'
#' @param dat_path Character string. Path to the folder containing InterCatch
#'   exchange format files (.csv files). Default is current working directory.
#' @param stock_relation Data frame. A lookup table containing stock relationship
#'   information with columns including Species, ICESArea, EG (expert group),
#'   StockCode, and speciesCode. Used to map species and areas to stock codes
#'   and working groups.
#' @param output_format Character string. Specifies where the output should go.
#'   Options are "to_environment" (creates objects in global environment) or
#'   "to_file" (saves as CSV files). Default is c("to_environment", "to_file").
#' @param out_path Character string. Output path for CSV files when
#'   output_format = "to_file". Default is current working directory.
#' @param metier6 Character string or NULL. Specifies how to handle metier6 field.
#'   When set to "Fleet", the fleet information will be used to populate the
#'   metier6 column in both census_catches and effort tables. Default is NULL.
#' @param file_prefix
#' @param file_suffix
#'
#' @author Jonathan Stounberg & Jean-Baptiste Lecomte
#' @returns
#' @export
#'
#' @examples
convExchange_tidy <- function(
    dat_path = getwd(),
    stock_relation = stock_relation,
    metier6 = NULL,
    output_format = c("to_environment", "to_file"),
    out_path = getwd(),
    file_prefix = "",
    file_suffix = paste0("_RCEF_v", getOption("RCEF_version"))
) {
  options("RCEF_version" = "16.0")
  effort_relation <- data.frame(
    UnitEffort = c("dop", "kWd", "fd", "hf", "kh", "NoV", "tr"),
    variableType = c(
      "ScientificDaysAtSea", "scientifickWDaysAtSea", "scientificFishingDays",
      "scientificVesselFishingHour", "scientificVesselKgPrHour",
      "numberOfUniqueVessels", "numberOfDominantTrips"
    ),
    stringsAsFactors = FALSE
  )
  files <- list.files(dat_path, pattern = ".csv", full.names = TRUE)
  print("Creating RCEF from:")
  print(basename(files))

  dat <- files %>%
    lapply(utils::read.csv,
           col.names = paste0("V", 1:34), header = FALSE, check.names = FALSE) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), trimws))

  # Recoding catch categories
  dat$V12 <- dplyr::recode(dat$V12,
                           L = "LAN",
                           D = "DIS",
                           B = "BMS",
                           `Logbook Registered Discard` = "RegDIS")

  hi_names <- c("RecordType", "Country", "Year", "SeasonType", "Season",
                "Fleet", "AreaType", "FishingArea", "DepthRange", "UnitEffort",
                "Effort", "AreaQualifier")
  si_names <- c("RecordType", "Country", "Year", "SeasonType", "Season", "Fleet",
                "AreaType", "FishingArea", "DepthRange", "Species", "Stock_orig",
                "CatchCategory", "ReportingCategory", "DataToForm", "Usage",
                "SamplesOrigin", "QualityFlag", "UnitCaton", "Caton", "OffLandings",
                "VarCaton", "InfoFleet", "InfoStockCoordinator", "InfoGeneral")
  sd_names <- c("RecordType", "Country", "Year", "SeasonType", "Season", "Fleet",
                "AreaType", "FishingArea", "DepthRange", "Species", "Stock_orig",
                "CatchCategory", "ReportingCategory", "Sex", "CANUMtype",
                "AgeLength", "PlusGroup", "SampledCatch", "NumSamplesLngt",
                "NumLngtMeas", "NumSamplesAge", "NumAgeMeas", "unitMeanWeight",
                "unitCANUM", "UnitAgeOrLength", "UnitMeanLength", "Maturity",
                "NumberCaught", "MeanWeight", "MeanLength", "varNumLanded",
                "varWgtLanded", "varLgtLanded")

  hi <- dat[dat$V1 == "HI", 1:length(hi_names)]; names(hi) <- hi_names
  si <- dat[dat$V1 == "SI", 1:length(si_names)]; names(si) <- si_names
  sd <- dat[dat$V1 == "SD", 1:length(sd_names)]; names(sd) <- sd_names

  # Keys
  hi$key <- with(hi, paste(Country, Year, Season, FishingArea, Fleet, UnitEffort, sep = "_"))
  si$key <- with(si, paste(Country, Year, Season, Species, FishingArea, Fleet, CatchCategory, sep = "_"))
  sd$key <- with(sd, paste(Country, Year, Season, Species, FishingArea, Fleet, CatchCategory, sep = "_"))

  # SI cleaning and merging
  si <- si[!duplicated(si$key), ]
  stock_relation$FishingArea <- stock_relation$ICESArea
  si <- merge(si, stock_relation, by = c("Species", "FishingArea"))

  # Create domain strings for matching
  si$key2 <- with(si, paste(Country, Year, Season, Species, FishingArea, Fleet, sep = "_"))
  tbl <- unique(si[, c("key2", "CatchCategory")])
  has_categ <- sapply(c("DIS", "RegDIS", "BMS"), function(cat) {
    tbl2 <- tbl[tbl$CatchCategory %in% c(cat, "LAN"), ]
    tbl2[duplicated(tbl2$key2), ]$key2
  }, simplify = FALSE)
  si$domain <- with(si, paste(Season, FishingArea, Fleet, sep = "_"))
  si$domainCatchDis <- ifelse(
    (si$key2 %in% unlist(has_categ[c("DIS", "RegDIS")]) & si$CatchCategory == "LAN") |
      si$CatchCategory %in% c("DIS", "RegDIS"),
    si$domain, ""
  )
  si$domainCatchBMS <- ifelse(
    (si$key2 %in% has_categ[["BMS"]] & si$CatchCategory == "LAN") |
      si$CatchCategory %in% c("BMS"),
    si$domain, ""
  )
  si$domainBiology <- ifelse(si$key %in% sd$key, si$domain, "")
  si$quarter <- ifelse(si$SeasonType == "Quarter", si$Season, NA)
  si <- si[order(si$key), ]

  # Census catches (Landings)
  census_catches <- data.frame(
    vesselFlagCountry = si$Country,
    year = si$Year,
    workingGroup = si$EG,
    stock = si$StockCode,
    speciesCode = si$speciesCode,
    catchCategory = sub("Logbook Registered Discard|RegDIS", "DIS", si$CatchCategory),
    seasonType = ifelse(is.na(as.numeric(si$quarter)), "Year", "Quarter"),
    seasonValue = ifelse(is.na(as.numeric(si$quarter)), as.numeric(si$Year), as.numeric(si$quarter)),
    areaType = "ICESArea",
    areaValue = si$FishingArea,
    fisheriesManagementUnit = NA_character_,
    metier6 = if (!is.null(metier6) && tolower(metier6) == "fleet") si$Fleet else NA_character_,
    fleetType = "WGFleet",
    fleetValue = si$Fleet,
    domainCatchDis = ifelse(si$domainCatchDis == "", NA, si$domainCatchDis),
    domainCatchBMS = ifelse(si$domainCatchBMS == "", NA, si$domainCatchBMS),
    domainBiology = ifelse(si$domainBiology == "", NA, si$domainBiology),
    variableType = "ScientificWeight_kg",
    WGWeight = as.numeric(ifelse(si$CatchCategory == "LAN", si$Caton, NA)),
    OfficialWeight = as.numeric(ifelse(si$CatchCategory == "LAN", si$OffLandings, NA)),
    comment = si$InfoStockCoordinator
  )

  est <- si[si$CatchCategory != "LAN", ]
  xx <- unique(sd[sd$CatchCategory != "LAN", c("NumSamplesLngt", "key")])
  est <- merge(est, xx, by = "key", all.x = TRUE)
  est$NumSamplesLngt[is.na(est$NumSamplesLngt)] <- 0
  estimated_catches <- data.frame(
    vesselFlagCountry = est$Country,
    year = est$Year,
    workingGroup = est$EG,
    stock = est$StockCode,
    speciesCode = est$speciesCode,
    catchCategory = est$CatchCategory,
    domainCatch = est$domain,
    variableType = "ScientificWeight_kg",
    total = as.numeric(est$Caton),
    mean = NA,
    varianceTotal = NA,
    varianceMean = NA,
    PSUtype = NA,
    numPSU = est$NumSamplesLngt,
    numTrips = est$NumSamplesLngt
  )
  estimated_catches <- estimated_catches[order(estimated_catches$domainCatch), ]

  # Distributions (biology data)
  sd <- merge(sd, stock_relation, by = c("Species", "FishingArea"))
  sd$bvType <- ifelse(tolower(sd$CANUMtype) == "age", "Age", "Length")
  sd$ageType <- ifelse(sd$bvType == "Age", "ageyear", "")
  sd$numPSUs <- ifelse(sd$bvType == "Age", sd$NumSamplesAge, sd$NumSamplesLngt)
  sd$numMeasurements <- ifelse(sd$bvType == "Age", sd$NumAgeMeas, sd$NumLngtMeas)
  sd$domain <- with(sd, paste(Season, FishingArea, Fleet, sep = "_"))
  distributions <- data.frame(
    vesselFlagCountry = sd$Country,
    year = sd$Year,
    workingGroup = sd$EG,
    stock = sd$StockCode,
    speciesCode = sd$speciesCode,
    catchCategory = sd$CatchCategory,
    domainBiology = sd$domain,
    bvType = sd$CANUMtype,
    bvUnit = sd$UnitAgeOrLength,
    bvValue = sd$AgeLength,
    AgeType = sd$ageType,
    AgeGroupPlus = as.numeric(sd$PlusGroup),
    variableType = rep(c("Number", "WeightLive"), each = nrow(sd)),
    total = c(as.numeric(sd$NumberCaught), rep(NA, nrow(sd))),
    mean = c(rep(NA, nrow(sd)), as.numeric(sd$MeanWeight)),
    varianceTotal = NA, varianceMean = NA, PSUtype = NA,
    numPSUs = c(sd$numPSUs, sd$numPSUs),
    numTrips = c(sd$numPSUs, sd$numPSUs),
    numMeasurements = c(sd$numMeasurements, sd$numMeasurements)
  )
  distributions <- distributions[order(distributions$domainBiology), ]

  # Effort
  hi <- hi[!is.na(hi$UnitEffort) & !duplicated(hi$key), ]
  hi$quarter <- ifelse(hi$SeasonType == "Quarter", hi$Season, NA)
  hi <- merge(hi, effort_relation, by = "UnitEffort")
  hi$Effort <- as.numeric(hi$Effort)

  hi <- hi %>%
    dplyr::group_by(Country, Year, quarter, Season, FishingArea, Fleet, variableType, key) %>%
    dplyr::summarise(total = sum(Effort), .groups = "drop")
  area_wg <- stock_relation[stock_relation$EG %in% sd$EG, ]
  area_wg <- unique(area_wg[, c("FishingArea", "EG")])
  hi <- merge(hi, area_wg, by = c("FishingArea"))
  hi <- hi[order(hi$key), ]
  effort <- data.frame(
    vesselFlagCountry = hi$Country,
    year = hi$Year,
    workingGroup = hi$EG,
    quarter = hi$Season,
    area = hi$FishingArea,
    fisheriesManagementUnit = "",
    metier6 = if (!is.null(metier6) && tolower(metier6) == "fleet") hi$Fleet else "",
    fleet = hi$Fleet,
    variableType = hi$variableType,
    total = hi$total
  )

  # Output to file or environment
  if ("to_file" %in% output_format) {
    write.csv(census_catches, file.path(out_path, paste0(file_prefix, "census_catches", file_suffix, ".csv")), row.names = FALSE, quote = FALSE, na = "")
    write.csv(estimated_catches, file.path(out_path, paste0(file_prefix, "estimated_catches", file_suffix, ".csv")), row.names = FALSE, quote = FALSE, na = "")
    write.csv(distributions, file.path(out_path, paste0(file_prefix, "distributions", file_suffix, ".csv")), row.names = FALSE, quote = FALSE, na = "")
    write.csv(effort, file.path(out_path, paste0(file_prefix, "effort", file_suffix, ".csv")), row.names = FALSE, quote = FALSE, na = "")
  }
  if ("to_environment" %in% output_format) {
    assign("census_catches", census_catches, envir = .GlobalEnv)
    assign("estimated_catches", estimated_catches, envir = .GlobalEnv)
    assign("distributions", distributions, envir = .GlobalEnv)
    assign("effort", effort, envir = .GlobalEnv)
  }
  if ("to_list" %in% output_format) {
    return(list(
      census_catches = census_catches,
      estimated_catches = estimated_catches,
      distributions = distributions,
      effort = effort
    ))
  }
}
