# Script information ------------------------------------------------------

# Title: Function to save SI, HI and SD files into a IC file
# Based on code developed by Gersom Costas for horse mackerel
# Sonia Sanchez (ssanchez@azti.es) and Leire Ibaibarriaga (libaibarriaga@azti.es) 
# Date: 2021/05/20

# VARIABLES:
# ICfile : list(hi.dat, si.dat, sd.dat)
# stkn   : stock name
# inst   : institute (azti/ieo)
# year   : year for the compiled data
# path   : place were file to be saved
# check  : if file for IC checks (default = FALSE --> file for combining, keeping catage=0 values)

saveICfile <- function(ICfile, stkn, inst="azti", year=2020, path=NULL, check=FALSE){

  # output file name
  
  fn <- paste("IC",stkn,"age",inst,year,sep="_")
  fname <- ifelse(check==FALSE, paste0(fn,".txt"), paste0(fn,"revIC.txt"))
  
  if (!is.null(path))
    fname <- file.path(path, fname)
  
  # HI
  
  HI <- ICfile$HI[c("RecordType","Country","Year","SeasonType","Season","Fleet",
                               "AreaType","FishingArea","DepthRange","UnitEffort","Effort","AreaQualifier")]
  
  # SI
  
  SI <- ICfile$SI[c("RecordType","Country","Year","SeasonType","Season","Fleet",
                           "AreaType","FishingArea","DepthRange","Species","Stock","CatchCategory",
                           "ReportingCategory","DataToFrom","Usage","SamplesOrigin","QualityFlag",
                           "UnitCATON","CATON","OffLandings","varCATON","InfoFleet","InfoStockCoordinator","InfoGeneral")] %>% 
    mutate(varCATON=replace(varCATON,is.na(varCATON),-9))
  
  # SD
  
  if (check==FALSE) {
    
    SD <- ICfile$SD %>% 
      mutate(MeanWeight=replace(MeanWeight,is.nan(MeanWeight),-9),
             MeanLength=replace(MeanLength,is.nan(MeanLength),-9))
    
  } else
    
    SD <- sd.dat %>% filter(!is.na(MeanWeight))
    
  SD <- SD[c("RecordType","Country","Year","SeasonType","Season","Fleet",
                           "AreaType","FishingArea","DepthRange","Species","Stock","CatchCategory",
                           "ReportingCategory","Sex","CANUMtype","AgeLength","PlusGroup","SampledCatch",
                           "NumSamplesLngt","NumLngtMeas","NumSamplesAge","NumAgeMeas","unitMeanWeight",
                           "unitCANUM","UnitAgeOrLength","UnitMeanLength","Maturity","NumberCaught",
                           "MeanWeight","MeanLength","varNumLanded","varWgtLanded","varLgtLanded")]
  
  
  # Save IC file
  
  trf <- function(x) {
    names(x) <- paste0("V",1:length(x))
    x <- x %>%  ungroup() %>% mutate(across(everything(), as.character))
    return(x)
  }
  
  write.table( bind_rows(trf(HI), trf(SI), trf(SD)), 
               file=fname, quote = F, sep = ",", na = "", dec = ".", 
               row.names = F, col.names = F)
  
}