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
#'
#' @details
#' The function processes three types of InterCatch records:
#' \itemize{
#'   \item HI (Header Information): Contains effort data by fleet and area
#'   \item SI (Species Information): Contains catch data by species and category
#'   \item SD (Sample Data): Contains age/length distribution data
#' }
#'
#' The function creates domains for catch estimation and links biological
#' sampling data to catch categories (landings, discards, BMS). It handles
#' the conversion of effort units and creates appropriate domains for different
#' catch categories.
#'
#' Key processing steps include:
#' \itemize{
#'   \item Data cleaning and whitespace trimming using tidyverse functions
#'   \item Recoding of catch categories (L→LAN, D→DIS, B→BMS)
#'   \item Creation of unique keys for matching across data types
#'   \item Domain creation for catch estimation procedures
#'   \item Effort data aggregation by fleet and area
#' }
#'
#' @returns
#' When output_format = "to_environment", creates four data frames in the global environment:
#' \itemize{
#'   \item \code{census_catches}: Census catch data with landing information
#'   \item \code{estimated_catches}: Estimated catch data for discards and BMS
#'   \item \code{distributions}: Age/length distribution data from biological sampling
#'   \item \code{effort}: Fishing effort data by fleet and area
#' }
#'
#' When output_format = "to_file", saves the same four tables as CSV files
#' in the specified output path.
#'
#' @section Required Packages:
#' The function requires the following packages:
#' \itemize{
#'   \item \code{dplyr}: For data manipulation (bind_rows, mutate, across, filter, etc.)
#'   \item \code{utils}: For reading/writing CSV files
#'   \item \code{magrittr}: For pipe operator (\%>\%)
#' }
#'
#' @section Input File Format:
#' Input CSV files should contain InterCatch exchange format data with:
#' \itemize{
#'   \item 34 columns (V1 to V34)
#'   \item First column (V1) indicating record type: "HI", "SI", or "SD"
#'   \item No header row
#'   \item Proper encoding to avoid whitespace issues
#' }
#'
#' @section Effort Unit Conversions:
#' The function automatically converts InterCatch effort units to RCEF variable types:
#' \itemize{
#'   \item "dop" → "ScientificDaysAtSea"
#'   \item "kWd" → "scientifickWDaysAtSea"
#'   \item "fd" → "scientificFishingDays"
#'   \item "hf" → "scientificVesselFishingHour"
#'   \item "kh" → "scientificVesselKgPrHour"
#'   \item "NoV" → "numberOfUniqueVessels"
#'   \item "tr" → "numberOfDominantTrips"
#' }
#'
#' @section Output Table Structures:
#' \subsection{census_catches}{
#'   Contains landing data with columns: VesselFlagCountry, year, workingGroup,
#'   stock, speciesCode, catchCategory, quarter, area, fisheriesManagementUnit,
#'   metier6, fleet, domainCatchDis, domainCatchBMS, domainBiology, variableType,
#'   total, comment
#' }
#'
#' \subsection{estimated_catches}{
#'   Contains discard and BMS estimates with columns: VesselFlagCountry, year,
#'   workingGroup, stock, speciesCode, catchCategory, domainCatch, variableType,
#'   total, mean, varianceTotal, varianceMean, PSUtype, numPSU, numTrips
#' }
#'
#' \subsection{distributions}{
#'   Contains age/length distribution data with columns: VesselFlagCountry, year,
#'   workingGroup, stock, speciesCode, catchCategory, domainBiology, fishDomain,
#'   bvType, bvValue, AgeType, AgeGroupPlus, variableType, total, mean,
#'   varianceTotal, varianceMean, PSUtype, numPSUs, numTrips, numMeasurements
#' }
#'
#' \subsection{effort}{
#'   Contains effort data with columns: vesselFlagCountry, year, workingGroup,
#'   quarter, area, fisheriesManagementUnit, metier6, fleet, variableType, total
#' }
#'
#' @examples
#' \dontrun{
#' # Basic usage with environment output
#' convExchange_tidy(
#'   dat_path = "data/intercatch/",
#'   stock_relation = my_stock_relation,
#'   output_format = "to_environment"
#' )
#'
#' # Save to files with metier6 populated from fleet
#' convExchange_tidy(
#'   dat_path = "data/intercatch/",
#'   stock_relation = my_stock_relation,
#'   output_format = "to_file",
#'   out_path = "output/rcef/",
#'   metier6 = "Fleet"
#' )
#'
#' # Using with specific file pattern
#' convExchange_tidy(
#'   dat_path = "data/2023_intercatch/",
#'   stock_relation = stock_lookup_table,
#'   output_format = "to_environment",
#'   metier6 = "Fleet"
#' )
#' }
#'
#' @seealso
#' \url{https://www.ices.dk/data/data-portals/Pages/InterCatch.aspx} for InterCatch format documentation
#'
#' @note
#' This function assumes that the stock_relation table contains all necessary
#' mappings for species and areas found in the InterCatch data. Missing mappings
#' will result in data loss during the merge operations.
#'
#' @author Jonathan Stounberg & Jean-Baptiste Lecomte
#' @export
convExchange_tidy <- function(dat_path = getwd(),
                              stock_relation = stock_relation,
                              output_format = c("to_list", "to_file"),
                              out_path = getwd(),
                              metier6 = NULL) {
  effort_relation <- data.frame(
    UnitEffort = c("dop", "kWd", "fd", "hf", "kh", "NoV", "tr"),
    variableType = c(
      "ScientificDaysAtSea", "scientifickWDaysAtSea", "scientificFishingDays",
      "scientificVesselFishingHour", "scientificVesselKgPrHour",
      "numberOfUniqueVessels", "numberOfDominantTrips"
    )
  )
  ## Load InterCatch exchange format files
  files <- list.files(dat_path, pattern = ".csv", full.names = TRUE)
  print("Creating RCEF from:")
  print(basename(files))
  dat <- files %>%
    lapply(utils::read.csv,
           col.names = paste0("V", 1:34), header = FALSE, check.names = FALSE) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), trimws))

  # Recoding catch categories
  dat$V12 <- dplyr::recode(dat$V12, L = "LAN", D = "DIS", B = "BMS")

  # Assign column names per record type
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

  hi <- dat[dat$V1 == "HI", 1:length(hi_names)]; names(hi) <- hi_names
  si <- dat[dat$V1 == "SI", 1:length(si_names)]; names(si) <- si_names
  sd <- dat[dat$V1 == "SD", 1:length(sd_names)]; names(sd) <- sd_names

  # Keys
  hi$key <- paste(hi$Country, hi$Year, hi$Season, hi$FishingArea, hi$Fleet, hi$UnitEffort, sep = "_")
  si$key <- paste(si$Country, si$Year, si$Season, si$Species, si$FishingArea, si$Fleet, si$CatchCategory, sep = "_")
  sd$key <- paste(sd$Country, sd$Year, sd$Season, sd$Species, sd$FishingArea, sd$Fleet, sd$CatchCategory, sep = "_")

  # SI cleaning and merging
  si <- si[!(si$Caton == 0 & si$CatchCategory == "LAN"), ]
  si <- si[!duplicated(si$key), ]
  stock_relation$FishingArea <- stock_relation$ICESArea
  si <- merge(si, stock_relation, by = c("Species", "FishingArea"))

  # Domain creation
  si$key2 <- substr(si$key, 1, nchar(si$key)-4)
  tbl <- unique(si[, c("key2", "CatchCategory")])
  has_dis <- tbl[duplicated(tbl$key2), ]$key2
  si$domain <- paste(si$Season, si$FishingArea, si$Fleet, sep = "_")
  si$domainCatchDis <- ifelse((si$key2 %in% has_dis | si$CatchCategory == "DIS") & si$CatchCategory != "DIS", si$domain, NA)
  si$domainCatchBMS <- ifelse(si$CatchCategory == "DIS", si$domain, NA)
  si$domainBiology <- ifelse(si$key %in% sd$key, si$domain, NA)
  si$quarter <- ifelse(si$SeasonType == "Quarter", si$Season, NA)
  si <- si[order(si$key), ]

  census_catches <- data.frame(
    VesselFlagCountry = si$Country,
    year = si$Year,
    workingGroup = si$EG,
    stock = si$StockCode,
    speciesCode = si$speciesCode,
    catchCategory = si$CatchCategory,
    quarter = si$quarter,
    area = si$FishingArea,
    fisheriesManagementUnit = NA,
    metier6 = NA,
    fleet = si$Fleet,
    domainCatchDis = si$domainCatchDis,
    domainCatchBMS = si$domainCatchBMS,
    domainBiology = si$domainBiology,
    variableType = "ScientificWeight_kg",
    total = as.numeric(ifelse(si$CatchCategory == "LAN", si$Caton, NA)),
    comment = si$InfoStockCoordinator
  ) %>%
    dplyr::mutate(dplyr::across(where(is.character), ~ dplyr::na_if(., "")))

  if (metier6 == "Fleet") {
    census_catches <- census_catches %>%
      dplyr::mutate(metier6 = fleet)
  }

  est <- si[si$CatchCategory != "LAN", ]
  xx <- unique(sd[sd$CatchCategory != "LAN", c("NumSamplesLngt", "key")])
  est <- merge(est, xx, by = "key", all.x = TRUE)
  est$NumSamplesLngt[is.na(est$NumSamplesLngt)] <- 0

  estimated_catches <- data.frame(
    VesselFlagCountry = est$Country,
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

  # SD processing
  sd <- merge(sd, stock_relation, by = c("Species", "FishingArea"))
  sd$bvType <- ifelse(tolower(sd$CANUMtype) == "age", "Age", "Length")
  sd$ageType <- ifelse(sd$bvType == "Age", "ageyear", NA)
  sd$numPSUs <- ifelse(sd$bvType == "Age", sd$NumSamplesAge, sd$NumSamplesLngt)
  sd$numMeasurements <- ifelse(sd$bvType == "Age", sd$NumAgeMeas, sd$NumLngtMeas)
  sd$domain <- paste(sd$Season, sd$FishingArea, sd$Fleet, sep = "_")

  distributions <- data.frame(
    VesselFlagCountry = sd$Country,
    year = sd$Year,
    workingGroup = sd$EG,
    stock = sd$StockCode,
    speciesCode = sd$speciesCode,
    catchCategory = sd$CatchCategory,
    domainBiology = sd$domain,
    fishDomain = NA,
    bvType = sd$bvType,
    bvValue = sd$AgeLength,
    AgeType = sd$ageType,
    AgeGroupPlus = NA,
    variableType = NA,
    total = as.numeric(sd$NumberCaught),
    mean = as.numeric(sd$MeanWeight),
    varianceTotal = NA,
    varianceMean = NA,
    PSUtype = NA,
    numPSUs = sd$numPSUs,
    numTrips = sd$numPSUs,
    numMeasurements = sd$numMeasurements
  )
  # Stack to "Number" and "WeightLive"
  distributions <- rbind(distributions, distributions)
  distributions$variableType <- rep(c("Number", "WeightLive"), each = nrow(distributions)/2)
  distributions[distributions$variableType == "Number", "mean"] <- NA
  distributions[distributions$variableType != "Number", "total"] <- NA
  distributions <- distributions[order(distributions$domainBiology), ]

  # Effort aggregation
  hi <- hi %>%
    dplyr::filter(!is.na(UnitEffort)) %>%
    dplyr::distinct(key, .keep_all = TRUE)
  hi$quarter <- ifelse(hi$SeasonType == "Quarter", hi$Season, NA)
  hi <- merge(hi, effort_relation, by = "UnitEffort")
  hi <- hi %>%
    dplyr::mutate(Effort = as.numeric(Effort)) %>%
    dplyr::group_by(Country, Year, quarter, Season, FishingArea, Fleet, variableType, key) %>%
    dplyr::summarise(total = sum(Effort), .groups = "drop")
  area_wg <- stock_relation[stock_relation$EG %in% sd$EG, ]
  area_wg <- unique(area_wg[, c("FishingArea", "EG")])
  hi <- merge(hi, area_wg, by = "FishingArea")
  hi <- hi[order(hi$key), ]

  effort <- data.frame(
    vesselFlagCountry = hi$Country,
    year = hi$Year,
    workingGroup = hi$EG,
    quarter = hi$Season,
    area = hi$FishingArea,
    fisheriesManagementUnit = NA,
    metier6 = NA,
    fleet = hi$Fleet,
    variableType = hi$variableType,
    total = hi$total
  )
  if (metier6 == "Fleet") {
    effort <- effort %>%
      dplyr::mutate(metier6 = fleet)
  }

  # Output routing
  if ("to_list" %in% output_format) {
    return(list(census_catches = census_catches,
                estimated_catches = estimated_catches,
                distributions = distributions,
                effort = effort)
    )
  }
  if ("to_files" %in% output_format) {
    utils::write.csv(census_catches, paste0(out_path, "/census_catches.csv"), row.names = FALSE, quote = FALSE)
    utils::write.csv(estimated_catches, paste0(out_path, "/estimated_catches.csv"), row.names = FALSE, quote = FALSE)
    utils::write.csv(distributions, paste0(out_path, "/distributions.csv"), row.names = FALSE, quote = FALSE)
    utils::write.csv(effort, paste0(out_path, "/effort.csv"), row.names = FALSE, quote = FALSE)
  }
}
