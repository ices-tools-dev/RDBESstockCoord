

makeRelation <- function(StockListbyEG_path = getwd(),
                         StockListbyArea = getwd()){

  
  ##
  codes_aph <- getCodeList("SpecWoRMS")
  names(codes_aph)[2] <- "speciesCode"
  names(codes_aph)[3] <- "SpeciesName"
  
  codes_FAO <- getCodeList("SpecASFIS")
  names(codes_FAO)[2] <- "Species"
  names(codes_FAO)[3] <- "SpeciesName"
  
  codes_aph <- merge(codes_FAO, codes_aph, by = "SpeciesName")
  
  ##
  
  StockListbyEG<-read.csv("Q:/20-forskning/20-dfad/users/jostou/home/wg_stock/WGRDBESstockCoord/personal/jost/EGsStocksByYear.csv",
                          header = TRUE, sep = ",", encoding = "UTF-8")
  StockListbyEG <- StockListbyEG[nzchar(StockListbyEG$StockCode), ]
  
  StockListbyArea<-read.csv("Q:/20-forskning/20-dfad/users/jostou/home/wg_stock/WGRDBESstockCoord/personal/jost/StockAssessmentGraphs_2025124mfqskttyclbazq2wfl5zbnzy.csv",
                            header = TRUE, sep = ",", encoding = "UTF-8")
  
  StockListbyArea$ICESArea <- gsub("~", ",",StockListbyArea$ICES.Areas..splited.with.character.....)
  StockListbyArea$ICESArea <- trimws(StockListbyArea$ICESArea)
  names(StockListbyArea)[3]<-"StockCode"
  
  #Add Ices areas
  StockListbyEG <- merge(StockListbyEG, StockListbyArea[, c("StockCode", "ICESArea", "SpeciesName")], 
                         by = c("StockCode"),all.x = TRUE)
  StockListbyEG <- StockListbyEG[!duplicated(StockListbyEG[, c("StockCode")]), ]
  
  fun <- function(j) {
    dat <- StockListbyEG[j, ]
    
    ## corrections to data
    dat[dat$StockCode == "cod.27.21", "ICESArea"] <- "27.3.a.21"
    ##
    
    dat <- separate_longer_delim(dat, "ICESArea", ",")
    dat$ICESArea <- trimws(dat$ICESArea)
    

    
    dat <- merge(dat, codes_aph, by = "SpeciesName")
  }
  stock_relation <- rbind.fill(lapply(1:nrow(StockListbyEG), fun))
  
  assign("stock_relation", stock_relation, .GlobalEnv)
}

