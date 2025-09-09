#' InterCatch output to RCEF format
#'
#' @param dat_path Path to the folder in which IC data is stored by year
#' @param years year for whih IC data will be loaded
#' @param output_format "to_environment" and/or "to_file"
#'
#' @returns
#' @export
#'
#' @examples
ICout_RCEF <- function(dat_path,
                       years,
                       stock_relation = NULL,
                       metier6 = NULL,
                       output_format = c("to_environment", "to_file"),
                       out_path = getwd(),
                       keep_temp_file = FALSE,
                       file_prefix = "")
{
    ## read in and adjust
    lst <- list.files(paste0(dat_path, "/", years),
                      pattern = ".txt", full.names = TRUE)

    stopifnot(any(grepl("StockOverview.txt", lst)))
    overview <- data.table::rbindlist(lapply(lst[lst %like% "StockOverview"],
                                             read.table,
                                             header = TRUE, sep = "\t"),
                                      fill = TRUE)


    overview$Species <- toupper(gsub("\\..*", "", overview$Stock))
    overview$Catch.Cat.[overview$Catch.Cat. == "Landings"] <- "LAN"
    overview$Catch.Cat.[overview$Catch.Cat. == "Discards"] <- "DIS"
    overview$Catch.Cat.[overview$Catch.Cat. == "BMS landing"] <- "BMS"

    ##
    stopifnot(any(grepl("Numbers", lst)))
    numbers <- rbindlist(lapply(lst[lst %like% "Numbers"],
                                read.table,
                                header = TRUE, sep = "\t", skip = 2),
                         fill = TRUE)

    numbers$Catch.Cat.[numbers$Catch.Cat. == "L"] <- "LAN"
    numbers$Catch.Cat.[numbers$Catch.Cat. == "D"] <- "DIS"
    numbers$Catch.Cat.[numbers$Catch.Cat. == "B"] <- "BMS"
    numbers$X <- NULL

                                        #
    stopifnot(any(grepl("MeanWeigth", lst)))
    mw <- rbindlist(lapply(lst[lst %like% "MeanWeigth"], read.table,
                           header = TRUE, sep = "\t", skip = 1),
                    fill = TRUE)

    mw$Catch.Cat.[mw$Catch.Cat. == "Landings"] <- "LAN"
    mw$Catch.Cat.[mw$Catch.Cat. == "Discards"] <- "DIS"
    mw$Catch.Cat.[mw$Catch.Cat. == "BMS landing"] <- "BMS"
    mw$X <- NULL

                                        #wide to long tables of biological data
    num_l <- melt(numbers, id.vars = names(numbers)[1:15],
                  measure.vars = names(numbers)[16:ncol(numbers)],
                  variable.name = "CANUMtype", value.name = "NumberCaught")

    mw_l <- melt(mw, id.vars = names(mw)[1:15],
                 measure.vars = names(mw)[16:ncol(mw)],
                 variable.name = "CANUMtype", value.name = "MeanWeight")


    sd <- merge(num_l, mw_l)
    sd$AgeLength <- as.numeric(str_extract(sd$CANUMtype, "[0-9]+"))
    sd$sex <- str_extract(sd$CANUMtype, "Undetermined|Male|Female")
    sd$CANUMtype <- str_extract(sd$CANUMtype, "Age|Lngt")
    sd$UnitAgeOrLength <- ifelse(sd$CANUMtype == "Age", "year", "cm")
    sd$UnitMeanLength <- ifelse(sd$CANUMtype == "Age", "cm", NA)

                                        #only include reported records
    sd <- sd[sd$NumberCaught > 0, ]

############# create exchange format
    hi <- data.frame(RecordType = "HI",
                     Country = overview$Country,
                     Year = overview$Year,
                     SeasonType = overview$Season.type,
                     Season = overview$Season,
                     Fleet = overview$Fleets,
                     AreaType = NA,
                     FishingArea = overview$Area,
                     DepthRange = NA,
                     UnitEffort = overview$UnitEffort,
                     Effort = overview$Effort,
                     AreaQualifier = NA)

    si <- data.frame(RecordType = "SI",
                     Country = overview$Country,
                     Year = overview$Year,
                     SeasonType = overview$Season.type,
                     Season = overview$Season,
                     Fleet = overview$Fleets,
                     AreaType = NA,
                     FishingArea = overview$Area,
                     DepthRange = NA,
                     Species = overview$Species,
                     Stock_orig = overview$Stock,
                     CatchCategory = overview$Catch.Cat.,
                     ReportingCategory = overview$Report.cat.,
                     DataToForm = NA,
                     Usage = "H",
                     SamplesOri00gin = NA,
                     QualityFlag = NA,
                     UnitCaton = "kg",
                     Caton = overview$Catch..kg,
                     OffLandings = overview$Catch..kg,
                     VarCaton = -9,
                     InfoFleet = NA,
                     InfoStockCoordinator = NA,
                     InfoGeneral = NA)

    sd <- data.frame(RecordType = "SD",
                     Country = sd$Country,
                     Year = sd$Year,
                     SeasonType = unique(si$SeasonType),
                     Season = sd$Season,
                     Fleet = sd$Fleet,
                     AreaType = NA,
                     FishingArea = sd$Area,
                     DepthRange = "",
                     Species = unique(si$Species),
                     Stock_orig = sd$Stock,
                     CatchCategory = sd$Catch.Cat.,
                     ReportingCategory = sd$Report.cat.,
                     Sex = sd$sex,
                     CANUMtype = sd$CANUMtype,
                     AgeLength = sd$AgeLength,
                     PlusGroup = -9,
                     SampledCatch = sd$SampledCatch,
                     NumSamplesLngt = sd$NumSamplesLength,
                     NumLngtMeas = sd$NumLengthMeasurements,
                     NumSamplesAge = sd$NumSamplesAge,
                     NumAgeMeas = sd$NumAgeMeasurement,
                     unitMeanWeight = "g",
                     unitCANUM = "K",
                     UnitAgeOrLength = sd$UnitAgeOrLength,
                     UnitMeanLength = sd$UnitMeanLength,
                     Maturity = "",
                     NumberCaught = sd$NumberCaught/1000,
                     MeanWeight = sd$MeanWeight,
                     MeanLength = NA,
                     varNumLanded = -9,
                     varWgtLande = -9,
                     varLgtLanded = -9)


                                        #create temp files ti input into the other fuction
    dir.create(paste0(dat_path, "/tmp"), showWarnings = FALSE)
    write.csv(sd, paste0(dat_path, "/tmp/sd.csv"),
              row.names = FALSE, quote = FALSE)
    write.csv(si, paste0(dat_path, "/tmp/si.csv"),
              row.names = FALSE, quote = 13)
    write.csv(hi, paste0(dat_path, "/tmp/hi.csv"),
              row.names = FALSE, quote = FALSE)


    if(is.null(stock_relation))
    {
        stock_relation <- makeRelation(StockListbyEG_file = "EGsStocksByYear.csv",
                                       StockListbyArea_file = "StockAssessmentGraphs_2025124mfqskttyclbazq2wfl5zbnzy.csv", ##StockAssessmentGraphs_2025.csv",
                                       StockListbyEG_path = path_to_data,
                                       StockListbyArea_path = path_to_data)
    }
    
    convExchange(dat_path = paste0(dat_path, "/tmp"),
                 stock_relation = stock_relation,
                 metier6 = metier6,
                 output_format = output_format,
                 out_path = out_path,
                 file_prefix = file_prefix)

    if( ! keep_temp_file)
    {
        unlink(paste0(dat_path, "/tmp"), recursive = TRUE)
    }
}





