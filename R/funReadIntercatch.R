
funReadIntercatch <- function(dat_path = dat_path) {

  lst <- list.files(dat_path, pattern = ".csv", full.names = T)

  dat <- rbindlist(lapply(lst, read.csv,
                          col.names = paste0("V", 1:34), header = F, check.names = F),
                   fill = T)

  ## deal with potential white space for some reason..
  cols_to_be_rectified <- names(dat)[vapply(dat, is.character, logical(1))]
  dat[, cols_to_be_rectified] <- lapply(dat[, cols_to_be_rectified, with = F], trimws)

  ## re-code
  dat$V12[dat$V12 %in% "L"] <- "Lan"
  dat$V12[dat$V12 %in% "D"] <- "Dis"
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

  assign("hi", hi, .GlobalEnv)
  assign("si", si, .GlobalEnv)
  assign("sd", sd, .GlobalEnv)
}
