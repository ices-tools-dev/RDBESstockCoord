## this is a re-write of fun_make_relation from RDBESstockCoord
## names in getCodeList("SpecWoRMS") and getCodeList("SpecASFIS") seem to have changed - this fixes that
## also obtaining StockListbyArea is inefficient and incomplete - tidier from icesVocab

#' Function to create stock_relation from ICES databases 
#'
#' @param year Year from which to extract the ICES stock databases
#'
#' @returns
#' @export
#'
#' @examples
makeRelation <- function(year){
  require(icesVocab)
  require(icesSD)
  
  # species codes
  codes_aph <- icesVocab::getCodeList("SpecWoRMS")
  names(codes_aph)[names(codes_aph) == "Key"] <- "speciesCode"
  names(codes_aph)[names(codes_aph) == "Description"] <- "SpeciesName"

  codes_FAO <- icesVocab::getCodeList("SpecASFIS")
  names(codes_FAO)[names(codes_FAO) == "Key"] <- "Species"
  names(codes_FAO)[names(codes_FAO) == "Description"] <- "SpeciesName"

  codes_aph_FAO <- merge(codes_FAO, codes_aph, by = "SpeciesName")
  codes_aph_FAO <- subset(codes_aph_FAO, 
                          select=c("Species",
                                   "SpeciesName",
                                   "speciesCode"))
  
  # stock list by year
  StockListbyEG <- icesSD::getSD(year=year)
  StockListbyEG <- subset(StockListbyEG, 
                          select=c("ExpertGroup", 
                                   "StockKey",
                                   "StockKeyLabel",
                                   "SpeciesScientificName",
                                   "StockKeyDescription"))
  
  names(StockListbyEG)[names(StockListbyEG) == "StockKeyLabel"] <- "StockCode"
  names(StockListbyEG)[names(StockListbyEG) == "ExpertGroup"] <- "EG"
  names(StockListbyEG)[names(StockListbyEG) == "SpeciesScientificName"] <- "SpeciesName"

  StockListbyEG <- StockListbyEG[nzchar(StockListbyEG$StockCode), ]
  
  # stock list by area
  StockListbyArea <- getCodeTypeRelation("ICES_StockCode","ICES_Area")
  
  names(StockListbyArea)[names(StockListbyArea) == "ICES_StockCode"] <- "StockCode"
  names(StockListbyArea)[names(StockListbyArea) == "ICES_Area"] <- "ICESArea"
  
   # These fixes should be made in the ICES Vocab, not here, but for now...
  StockListbyArea[StockListbyArea$StockCode == "cod.27.21", "ICESArea"] <- "27.3.a.21"  
  StockListbyArea <- rbind(StockListbyArea[!StockListbyArea$StockCode == "pok.27.3a46",],
                          data.frame(StockCode = "pok.27.3a46",
                                     ICESArea = c("27.3.a","27.3.a.20","27.3.a.21",
                                                  "27.4","27.4.a","27.4.b","27.4.c",
                                                  "27.6.a","27.6.b","27.6.b.1","27.6.b.2")))

  StockListbyArea <- rbind(StockListbyArea[!StockListbyArea$StockCode == "pil.27.8c9a",],
                           data.frame(StockCode = "pil.27.8c9a",
                                      ICESArea = c("27.8.c.e","27.8.c.w","27.9.a.n","27.9.a.s")))

  stock_relation <- merge(StockListbyEG,
                                StockListbyArea,
                                by = c("StockCode"), all.x = TRUE)
  
  stock_relation <- merge(stock_relation,
                                codes_aph_FAO, 
                                by = "SpeciesName")
  
                          
  assign("stock_relation", stock_relation, .GlobalEnv)
  
  
}
