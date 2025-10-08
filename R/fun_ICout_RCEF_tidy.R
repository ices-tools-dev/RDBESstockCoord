#' Convert InterCatch output to RCEF format (tidy)
#'
#' Reads InterCatch data files for a specified year and converts them
#' to the RCEF exchange format using tidyverse semantics, aligned to ICout_RCEF.
#'
#' @param dat_path Path to the folder in which IC data is stored by year.
#' @param years Year for which IC data will be loaded.
#' @param stock_relation A stock relationship table; if NULL, makeRelation() is called.
#' @param metier6 Optional metier6 mapping passed through to convExchange().
#' @param output_format Character vector: "to_environment" and/or "to_file".
#' @param out_path Output path for convExchange() when writing files.
#' @param keep_temp_file Logical; keep the intermediate tmp folder if TRUE.
#' @param file_prefix Optional file prefix for convExchange() output.
#'
#' @return Invisibly returns convExchange() result; writes/loads outputs per output_format.
#' @export
#'
#' @import dplyr tidyr purrr stringr
#' @importFrom utils read.table write.csv
ICout_RCEF_tidy <- function(dat_path,
                            years,
                            stock_relation = NULL,
                            metier6 = NULL,
                            output_format = c("to_environment", "to_file"),
                            out_path = getwd(),
                            keep_temp_file = FALSE,
                            file_prefix = "") {

  path <- file.path(dat_path, years)
  files <- list.files(path, pattern = ".txt", full.names = TRUE)

  # Guard files
  stopifnot(any(stringr::str_detect(files, "StockOverview\\.txt")))
  stopifnot(any(stringr::str_detect(files, "Numbers")))
  stopifnot(any(stringr::str_detect(files, "MeanWeigth")))

  # Load StockOverview
  overview <- files %>%
    purrr::keep(~ stringr::str_detect(.x, "StockOverview")) %>%
    purrr::map_dfr(~ utils::read.table(.x, header = TRUE, sep = "\t")) %>%
    dplyr::mutate(
      Species = toupper(stringr::str_remove(Stock, "\\..*")),
      Catch.Cat. = dplyr::case_when(
        Catch.Cat. == "Landings" ~ "LAN",
        Catch.Cat. == "Discards" ~ "DIS",
        Catch.Cat. == "BMS landing" ~ "BMS",
        TRUE ~ Catch.Cat.
      )
    )

  # Load Numbers
  numbers <- files %>%
    purrr::keep(~ stringr::str_detect(.x, "Numbers")) %>%
    purrr::map_dfr(~ utils::read.table(.x, header = TRUE, sep = "\t", skip = 2)) %>%
    dplyr::mutate(
      Catch.Cat. = dplyr::case_when(
        Catch.Cat. == "L" ~ "LAN",
        Catch.Cat. == "D" ~ "DIS",
        Catch.Cat. == "B" ~ "BMS",
        TRUE ~ Catch.Cat.
      )
    ) %>%
    dplyr::select(-dplyr::any_of("X"))

  # Load MeanWeight
  mw <- files %>%
    purrr::keep(~ stringr::str_detect(.x, "MeanWeigth")) %>%
    purrr::map_dfr(~ utils::read.table(.x, header = TRUE, sep = "\t", skip = 1)) %>%
    dplyr::mutate(
      Catch.Cat. = dplyr::case_when(
        Catch.Cat. == "Landings" ~ "LAN",
        Catch.Cat. == "Discards" ~ "DIS",
        Catch.Cat. == "BMS landing" ~ "BMS",
        TRUE ~ Catch.Cat.
      )
    ) %>%
    dplyr::select(-dplyr::any_of("X"))

  # Wide to long
  num_l <- tidyr::pivot_longer(
    numbers,
    cols = dplyr::all_of(colnames(numbers)[16:ncol(numbers)]),
    names_to = "CANUMtype",
    values_to = "NumberCaught"
  )

  mw_l <- tidyr::pivot_longer(
    mw,
    cols = dplyr::all_of(colnames(mw)[16:ncol(mw)]),
    names_to = "CANUMtype",
    values_to = "MeanWeight"
  )

  # Join
  sd <- dplyr::inner_join(
    num_l,
    mw_l,
    by = setdiff(colnames(num_l), "NumberCaught")
  ) %>%
    dplyr::mutate(
      AgeLength = as.numeric(stringr::str_extract(CANUMtype, "[0-9]+")),
      sex = stringr::str_extract(CANUMtype, "Undetermined|Male|Female"),
      CANUMtype = stringr::str_extract(CANUMtype, "Age|Lngt"),
      UnitAgeOrLength = ifelse(CANUMtype == "Age", "year", "cm"),
      UnitMeanLength = ifelse(CANUMtype == "Age", "cm", NA_character_)
    ) %>%
    dplyr::filter(NumberCaught > 0)

  # HI
  hi <- overview %>%
    dplyr::transmute(
      RecordType = "HI",
      Country,
      Year,
      SeasonType = Season.type,
      Season,
      Fleet = Fleets,
      AreaType = NA_character_,
      FishingArea = Area,
      DepthRange = NA_character_,
      UnitEffort,
      Effort,
      AreaQualifier = NA_character_
    )

  # SI with Caton/OffLandings split consistent with ICout_RCEF
  si <- overview %>%
    dplyr::transmute(
      RecordType = "SI",
      Country,
      Year,
      SeasonType = Season.type,
      Season,
      Fleet = Fleets,
      AreaType = NA_character_,
      FishingArea = Area,
      DepthRange = NA_character_,
      Species,
      Stock_orig = Stock,
      CatchCategory = Catch.Cat.,
      ReportingCategory = Report.cat.,
      DataToForm = NA_character_,
      Usage = "H",
      SamplesOri00gin = NA_character_,   # kept as-is to match current ICout_RCEF naming
      QualityFlag = NA_character_,
      UnitCaton = "kg",
      Caton = dplyr::if_else(
        Catch.Cat. %in% "Logbook Registered Discard",
        as.numeric(NA),
        Catch..kg
      ),
      OffLandings = dplyr::if_else(
        Catch.Cat. %in% "Logbook Registered Discard",
        Catch..kg,
        as.numeric(NA)
      ),
      VarCaton = -9,
      InfoFleet = NA_character_,
      InfoStockCoordinator = NA_character_,
      InfoGeneral = NA_character_
    )

  # SD
  # Note: SeasonType and Species taken as in ICout_RCEF (single unique per build)
  sd <- sd %>%
    dplyr::transmute(
      RecordType = "SD",
      Country,
      Year,
      SeasonType = unique(si$SeasonType)[1],
      Season,
      Fleet = Fleets,
      AreaType = NA_character_,
      FishingArea = Area,
      DepthRange = "",
      Species = unique(si$Species)[1],
      Stock_orig = Stock,
      CatchCategory = Catch.Cat.,
      ReportingCategory = Report.cat.,
      Sex = sex,
      CANUMtype,
      AgeLength,
      PlusGroup = -9,
      SampledCatch,
      NumSamplesLngt = NumSamplesLength,
      NumLngtMeas = NumLengthMeasurements,
      NumSamplesAge,
      NumAgeMeas = NumAgeMeasurement,
      unitMeanWeight = "g",
      unitCANUM = "K",
      UnitAgeOrLength,
      UnitMeanLength,
      Maturity = "",
      NumberCaught = NumberCaught / 1000,
      MeanWeight,
      MeanLength = NA_real_,
      varNumLanded = -9,
      varWgtLande = -9,
      varLgtLanded = -9
    )

  # Output files
  if("to_file" %in% output_format){
  # Temp files for convExchange
  tmp_dir <- file.path(dat_path, "tmp")
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)

  utils::write.csv(sd, file.path(tmp_dir, "sd.csv"), row.names = FALSE, quote = FALSE)
  utils::write.csv(si,     file.path(tmp_dir, "si.csv"), row.names = FALSE, quote = FALSE)
  utils::write.csv(hi,     file.path(tmp_dir, "hi.csv"), row.names = FALSE, quote = FALSE)
  }

  if("to_list" %in% output_format){
    return(list(hi = hi,
                sd = sd,
                si = si))
  }
}
