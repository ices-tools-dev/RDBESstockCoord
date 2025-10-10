#' Function to create stock_relation from ICES databases 
#'
#' @param year Year from which to extract the ICES stock databases
#'
#' @returns
#' @export
#'
#' @examples
makeRelation2 <- function(year){
  require(icesSAG)
  require(icesSD)
  require(icesVocab)
  
  codes_aph <- icesVocab::getCodeList("SpecWoRMS")
  names(codes_aph)[3] <- "speciesCode" # note this doesn't start with capital letter (it is used like that in intercatch_RCEF_tidy)
  names(codes_aph)[4] <- "SpeciesName"
  
  codes_FAO <- icesVocab::getCodeList("SpecASFIS")
  names(codes_FAO)[3] <- "Species"
  names(codes_FAO)[4] <- "SpeciesName"
  
  codes_aph <- merge(codes_FAO, codes_aph, by = "SpeciesName")
  codes_aph <- subset(codes_aph, select=c("Species","SpeciesName","speciesCode"))
  
  # stock list by year
  StockListbyEG <- icesSD::getSD(year=year)
  StockListbyEG <- subset(StockListbyEG, select=c("ExpertGroup", "StockKey","StockKeyLabel","StockKeyDescription"))
  colnames(StockListbyEG)[colnames(StockListbyEG)=="StockKeyLabel"] <- "StockCode"
  colnames(StockListbyEG)[colnames(StockListbyEG)=="ExpertGroup"] <- "EG"
  StockListbyEG <- StockListbyEG[nzchar(StockListbyEG$StockCode), ]
  
  # stock list by area
  StockListbyArea <- icesSAG::getSAG(stock=NULL, year=year, data="source")
  StockListbyArea <- subset(StockListbyArea, select=c("StockKeyLabel", "ICES_Areas", "SpeciesName"))
  colnames(StockListbyArea)[colnames(StockListbyArea)=="ICES_Areas"] <- "ICESArea"
  colnames(StockListbyArea)[colnames(StockListbyArea)=="StockKeyLabel"] <- "StockCode"
  
  # merge both stock lists
  StockListbyEG <- merge(StockListbyEG,
                         StockListbyArea,
                         by = c("StockCode"), all.x = TRUE)
  StockListbyEG <- StockListbyEG[!duplicated(StockListbyEG[, c("StockCode")]), ]
  
  ## corrections to data
  StockListbyEG[StockListbyEG$StockCode == "cod.27.21", "ICESArea"] <- "27.3.a.21"
  StockListbyEG[StockListbyEG$StockCode == "pok.27.3a46", "ICESArea"] <- "27.3.a ~ 27.3.a.20 ~ 27.3.a.21 ~ 27.4 ~ 27.4.a ~ 27.4.b ~ 27.4.c ~ 27.6.a ~ 27.6.b ~ 27.6.b.1 ~ 27.6.b.2"
  StockListbyEG[StockListbyEG$StockCode == "pil.27.8c9a", "ICESArea"] <- "27.8.c.e ~ 27.8.c.w ~ 27.9.a.n ~ 27.9.a.s"
  
  # merge with codes_aph
  
  StockListbyEG <- merge(StockListbyEG, codes_aph, by = "SpeciesName")
  
  # separate ICESArea
  
  stock_relation <- tidyr::separate_longer_delim(StockListbyEG, "ICESArea", "~")
  stock_relation$ICESArea <- trimws(stock_relation$ICESArea)
  
  assign("stock_relation", stock_relation, .GlobalEnv)
  
}
