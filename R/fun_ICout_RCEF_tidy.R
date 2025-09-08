#' Convert InterCatch output to RCEF format
#'
#' This function reads InterCatch data files for a specified year and converts them
#' to the Regional Coordination of Fisheries (RCEF) exchange format. It processes
#' stock overview, numbers, and mean weight data files, reshapes the data, and
#' creates the standardized HI (Header Information), SI (Stock Information), and
#' SD (Sample Data) record types.
#'
#' @param dat_path Character string. Path to the folder containing IC data organized by year.
#'   The folder structure should be: dat_path/year/files.txt
#' @param years Numeric or character. Year(s) for which IC data will be loaded.
#' @param stock_relation Data frame or NULL. Stock relationship table for conversion.
#'   If NULL (default), the function will create one using makeRelation().
#' @param output_format Character vector. Output options, can be "to_list"
#'   and/or "to_file". Default is both options.
#'
#' @return The function processes data and calls convExcahcnge() to generate output
#'   according to the specified output_format. No direct return value, but creates
#'   RCEF formatted data files and/or loads data to environment.
#'
#' @details
#' The function expects the following input files in the year folder:
#' \itemize{
#'   \item StockOverview.txt - Contains catch overview data by stock
#'   \item *Numbers*.txt - Contains numbers-at-age/length data (skip first 2 lines)
#'   \item *MeanWeigth*.txt - Contains mean weight data (skip first line)
#' }
#'
#' The function performs the following transformations:
#' \itemize{
#'   \item Standardizes catch categories (Landings->LAN, Discards->DIS, BMS landing->BMS)
#'   \item Reshapes wide format age/length data to long format
#'   \item Extracts species codes from stock names
#'   \item Creates RCEF record types (HI, SI, SD) with appropriate units and formatting
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Convert IC data for 2023 to RCEF format
#' ICout_RCEF(dat_path = "/path/to/IC_data",
#'            years = 2023,
#'            output_format = c("to_environment", "to_file"))
#'
#' # Use custom stock relation table
#' my_stock_relation <- read.csv("custom_stock_relation.csv")
#' ICout_RCEF(dat_path = "/path/to/IC_data",
#'            years = 2022,
#'            stock_relation = my_stock_relation,
#'            output_format = "to_file")
#' }
#'
#' @seealso
#' \code{\link{makeRelation}} for creating stock relationship tables,
#' \code{\link{convExcahcnge}} for the conversion process
#'
#' @author Jonathan Stounberg & Jean-Baptiste Lecomte
#'
#' @import purrr
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @importFrom utils read.table write.csv
#'
ICout_RCEF_tidy <- function(dat_path,
                            years,
                            stock_relation = NULL,
                            output_format = c("to_list", "to_file")) {

  path <- file.path(dat_path, years)
  files <- list.files(path, pattern = ".txt", full.names = TRUE)

  # Catch category conversion helper
  recode_catch <- function(x) dplyr::recode(x,
                                            Landings = "LAN",
                                            Discards = "DIS",
                                            `BMS landing` = "BMS",
                                            L = "LAN",
                                            D = "DIS",
                                            B = "BMS")

  # Stock overview
  overview <- files %>%
    purrr::keep(~ stringr::str_detect(.x, "StockOverview")) %>%
    purrr::map_dfr(~ utils::read.table(.x, header = TRUE, sep = "\t")) %>%
    dplyr::mutate(
      Species = toupper(stringr::str_remove(Stock, "\\..*")),
      Catch.Cat. = recode_catch(Catch.Cat.)
    )

  # Numbers
  numbers <- files %>%
    purrr::keep(~ stringr::str_detect(.x, "Numbers")) %>%
    purrr::map_dfr(~ utils::read.table(.x, header = TRUE, sep = "\t", skip = 2)) %>%
    dplyr::mutate(Catch.Cat. = recode_catch(Catch.Cat.)) %>%
    dplyr::select(-X)

  # MeanWeight
  mw <- files %>%
    purrr::keep(~ stringr::str_detect(.x, "MeanWeigth")) %>%
    purrr::map_dfr(~ utils::read.table(.x, header = TRUE, sep = "\t", skip = 1)) %>%
    dplyr::mutate(Catch.Cat. = recode_catch(Catch.Cat.)) %>%
    dplyr::select(-X)

  # Pivot and join
  num_l <- tidyr::pivot_longer(numbers, cols = 16:ncol(numbers), names_to = "CANUMtype", values_to = "NumberCaught")
  mw_l <- tidyr::pivot_longer(mw, cols = 16:ncol(mw), names_to = "CANUMtype", values_to = "MeanWeight")
  sd <- dplyr::inner_join(num_l, mw_l, by = setdiff(names(num_l), "NumberCaught"))

  sd <- sd %>%
    dplyr::mutate(
      AgeLength      = as.numeric(stringr::str_extract(CANUMtype, "[0-9]+")),
      sex            = stringr::str_extract(CANUMtype, "Undetermined|Male|Female"),
      CANUMtype      = stringr::str_extract(CANUMtype, "Age|Lngt"),
      UnitAgeOrLength= ifelse(CANUMtype == "Age", "year", "cm"),
      UnitMeanLength = ifelse(CANUMtype == "Age", "cm", NA)
    ) %>%
    dplyr::filter(NumberCaught > 0)

  # HI, SI, SD records
  hi <- overview %>% dplyr::transmute(
    RecordType = "HI",
    Country, Year, SeasonType = Season.type, Season, Fleet = Fleets,
    AreaType = NA, FishingArea = Area, DepthRange = NA,
    UnitEffort, Effort, AreaQualifier = NA
  )

  si <- overview %>% dplyr::transmute(
    RecordType = "SI",
    Country, Year, SeasonType = Season.type, Season, Fleet = Fleets,
    AreaType = NA, FishingArea = Area, DepthRange = NA,
    Species, Stock_orig = Stock,
    CatchCategory = Catch.Cat., ReportingCategory = Report.cat.,
    DataToForm = NA, Usage = "H", SamplesOrigin = NA,
    QualityFlag = NA, UnitCaton = "kg", Caton = Catch..kg,
    OffLandings = Catch..kg, VarCaton = -9,
    InfoFleet = NA, InfoStockCoordinator = NA, InfoGeneral = NA
  )

  sd <- sd %>% dplyr::transmute(
    RecordType = "SD",
    Country, Year, SeasonType = unique(si$SeasonType), Season, Fleet = Fleets,
    AreaType = NA, FishingArea = Area, DepthRange = "",
    Species = unique(si$Species), Stock_orig = Stock,
    CatchCategory = Catch.Cat., ReportingCategory = Report.cat.,
    Sex = sex, CANUMtype, AgeLength,
    PlusGroup = -9,
    SampledCatch, NumSamplesLngt = NumSamplesLength,
    NumLngtMeas = NumLengthMeasurements,
    NumSamplesAge, NumAgeMeas = NumAgeMeasurement,
    unitMeanWeight = "g", unitCANUM = "K",
    UnitAgeOrLength, UnitMeanLength,
    Maturity = "", NumberCaught = NumberCaught / 1000,
    MeanWeight, MeanLength = NA,
    varNumLanded = -9, varWgtLande = -9, varLgtLanded = -9
  )

  # Output files
  if("to_file" %in% output_format){
    dir.create(file.path(dat_path, "tmp"), showWarnings = FALSE)
    utils::write.csv(sd, file.path(dat_path, "tmp/sd.csv"), row.names = FALSE, quote = FALSE)
    utils::write.csv(si, file.path(dat_path, "tmp/si.csv"), row.names = FALSE, quote = FALSE)
    utils::write.csv(hi, file.path(dat_path, "tmp/hi.csv"), row.names = FALSE, quote = FALSE)
  }

  if("to_list" %in% output_format){
    return(list(hi = hi,
                sd = sd,
                si = si))
  }
}
