#' Title
#'
#' @param StockListbyEG_file name of the file with its extension
#' @param StockListbyArea_file name of the file with its extension
#' @param StockListbyEG_path path to the file
#' @param StockListbyArea_path path to the file
#'
#' @returns
#' @export
#'
#' @examples
makeRelation <- function(StockListbyEG_file,
                         StockListbyArea_file,
                         StockListbyEG_path = NULL,
                         StockListbyArea_path = NULL) {


  ##
  codes_aph <- icesVocab::getCodeList("SpecWoRMS")
  names(codes_aph)[2] <- "speciesCode"
  names(codes_aph)[3] <- "SpeciesName"

  codes_FAO <- icesVocab::getCodeList("SpecASFIS")
  names(codes_FAO)[2] <- "Species"
  names(codes_FAO)[3] <- "SpeciesName"

  codes_aph <- merge(codes_FAO, codes_aph, by = "SpeciesName")

  ##
  stopifnot(file.exists(paste0(StockListbyEG_path, "/", StockListbyEG_file)))
  StockListbyEG <- read.csv(paste0(StockListbyEG_path, "/", StockListbyEG_file),
                            header = TRUE, sep = ",", encoding = "UTF-8")
  StockListbyEG <- StockListbyEG[nzchar(StockListbyEG$StockCode), ]

  stopifnot(file.exists(paste0(StockListbyArea_path, "/", StockListbyArea_file)))
  StockListbyArea<-read.csv(paste0(StockListbyArea_path, "/", StockListbyArea_file),
                            header = TRUE, sep = ",", encoding = "UTF-8")

  StockListbyArea$ICESArea <- gsub("~",
                                   ",",
                                   StockListbyArea$ICES.Areas..splited.with.character.....)

  StockListbyArea$ICESArea <- trimws(StockListbyArea$ICESArea)
  names(StockListbyArea)[3] <- "StockCode"

  #Add Ices areas
  StockListbyEG <- merge(StockListbyEG,
                         StockListbyArea[, c("StockCode", "ICESArea", "SpeciesName")],
                         by = c("StockCode"), all.x = TRUE)
  StockListbyEG <- StockListbyEG[!duplicated(StockListbyEG[, c("StockCode")]), ]

  fun <- function(j) {
    dat <- StockListbyEG[j, ]

    ## corrections to data
    dat[dat$StockCode == "cod.27.21", "ICESArea"] <- "27.3.a.21"
    dat[dat$StockCode == "pok.27.3a46", "ICESArea"] <- "27.3.a , 27.3.a.20 , 27.3.a.21 , 27.4 , 27.4.a , 27.4.b , 27.4.c , 27.6.a , 27.6.b , 27.6.b.1 , 27.6.b.2"

    ##
    dat <- tidyr::separate_longer_delim(dat, "ICESArea", ",")
    dat$ICESArea <- trimws(dat$ICESArea)
    dat <- merge(dat, codes_aph, by = "SpeciesName")
  }

  stock_relation <- rbind.fill(lapply(1:nrow(StockListbyEG), fun))

  assign("stock_relation", stock_relation, .GlobalEnv)
}
