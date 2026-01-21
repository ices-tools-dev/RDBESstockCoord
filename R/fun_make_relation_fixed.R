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
  require(dplyr)
  require(icesVocab)
  require(icesSD)
  
  # species codes
  codes_aph <- icesVocab::getCodeList("SpecWoRMS")
  codes_aph <- codes_aph %>% 
    dplyr::rename(speciesCode=Key,
                  SpeciesName=Description )
  
  codes_FAO <- icesVocab::getCodeList("SpecASFIS")
  codes_FAO <- codes_FAO %>% 
    dplyr::rename(Species=Key,
                  SpeciesName=Description)
  
  codes_aph_FAO <- base::merge(codes_FAO, codes_aph, by = "SpeciesName")
  codes_aph_FAO <- base::subset(codes_aph_FAO, 
                                select=c("Species",
                                         "SpeciesName",
                                         "speciesCode"))
  
  # stock list by year
  StockListbyEG <- icesSD::getSD(year=year)
  StockListbyEG <- base::subset(StockListbyEG, 
                                select=c("ExpertGroup", 
                                         "StockKey",
                                         "StockKeyLabel",
                                         "SpeciesScientificName",
                                         "StockKeyDescription"))
  StockListbyEG <- StockListbyEG  %>% 
    dplyr::rename(StockCode=StockKeyLabel,
                  EG=ExpertGroup,
                  SpeciesName=SpeciesScientificName)
  StockListbyEG <- StockListbyEG[nzchar(StockListbyEG$StockCode), ]
  
  # stock list by area
  StockListbyArea <- getCodeTypeRelation("ICES_StockCode","ICES_Area")
  StockListbyArea <- StockListbyArea %>% 
    dplyr::rename(StockCode=ICES_StockCode,
                  ICESArea=ICES_Area)

  # These fixes should be made in the ICES Vocab, not here, but for now...
  StockListbyArea[StockListbyArea$StockCode == "cod.27.21", "ICESArea"] <- "27.3.a.21"  
  StockListbyArea <- rbind(StockListbyArea[!StockListbyArea$StockCode == "pok.27.3a46",],
                          data.frame(StockCode = "pok.27.3a46",
                                     ICESArea = c("27.3.a","27.3.a.20","27.3.a.21",
                                                  "27.4","27.4.a","27.4.b","27.4.c",
                                                  "27.6.a","27.6.b","27.6.b.1","27.6.b.2")))

  StockListbyArea <- rbind(StockListbyArea[!StockListbyArea$StockCode == "pil.27.8c9a",],
                           data.frame(StockCode = "pok.27.3a46",
                                      ICESArea = c("27.8.c.e","27.8.c.w","27.9.a.n","27.9.a.s")))

  stock_relation <- base::merge(StockListbyEG,
                                StockListbyArea,
                                by = c("StockCode"), all.x = TRUE)
  
  stock_relation <- base::merge(stock_relation,
                                codes_aph_FAO, 
                                by = "SpeciesName")
  
                          
  assign("stock_relation", stock_relation, .GlobalEnv)
  
  
}
