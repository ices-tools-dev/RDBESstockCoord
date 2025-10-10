# Script information ------------------------------------------------------

# Title: Function to read IC files and create SI, HI and SD files
# Based on code developed by Gersom Costas for horse mackerel
# Sonia Sanchez (ssanchez@azti.es) and Leire Ibaibarriaga (libaibarriaga@azti.es) 
# Date: 2021/05/20

readICfile <- function(fname, path=NULL){

  if (!is.null(path)){
    fname <- file.path(path, fname)  
  }
  if (!file.exists(fname)){
    stop("This file doesn't exist: ", fname)
  }
  dat <- read.table(fname, header=F, sep=",", dec=".", na.strings ="", fill=T, col.names = paste0("V", 1:33)) 
  if (ncol(dat)!=33){
    stop("Incorrect number of columns")
  }
  
  HI <- filter(dat, V1=="HI")%>% 
    select_if(function(x) !(all(is.na(x)) | all(x==""))) %>%  #Remove empty columns in dataframe 
    mutate_all(~replace(., . == -9, NA)) # replace -9 to NA
  # na_if(-9)# replace -9 to NA
  names(HI)<-c("RecordType","Country","Year","SeasonType","Season","Fleet","AreaType","FishingArea","DepthRange","UnitEffort","Effort","AreaQualifier")
  
  HI <- HI %>%   
    mutate_if(is.character, as.factor) %>%    # transform all the characters into factors
    mutate(Effort=as.numeric(as.character(Effort))) # transform effort into numeric
  
  SI <- filter(dat, V1=="SI")%>%  
    select_if(function(x) !(all(is.na(x)) | all(x==""))) %>% #Remove empty columns in dataframe 
    mutate_all(~replace(., . == -9, NA)) # replace -9 to NA
  # na_if (-9)  # replace -9 to NA
  names(SI)<-c("RecordType","Country","Year","SeasonType","Season","Fleet","AreaType","FishingArea","DepthRange","Species","Stock","CatchCategory","ReportingCategory","DataToFrom","Usage","SamplesOrigin","QualityFlag","UnitCATON","CATON","OffLandings","varCATON","InfoFleet","InfoStockCoordinator","InfoGeneral")
  
  SI <- SI %>%   
    mutate_if(is.character, as.factor) %>%    # transform all the characters into factors
    mutate_at(vars(CATON,OffLandings), function(x) as.numeric(as.character(x))) # transform effort into numeric
  
  SD <- filter(dat, V1=="SD")%>%
    #   select_if(function(x) !(all(is.na(x)) | all(x==""))) %>% #Remove empty columns in dataframe. Not needed here. We need 33 columns 
    mutate_all(~replace(., . == -9, NA)) # replace -9 to NA
  # na_if (-9) # replace -9 to NA
  colnames(SD) <- c("RecordType","Country","Year","SeasonType","Season","Fleet","AreaType","FishingArea","DepthRange","Species","Stock","CatchCategory","ReportingCategory","Sex","CANUMtype","AgeLength","PlusGroup","SampledCatch","NumSamplesLngt","NumLngtMeas","NumSamplesAge","NumAgeMeas","unitMeanWeight","unitCANUM","UnitAgeOrLength","UnitMeanLength","Maturity","NumberCaught","MeanWeight","MeanLength","varNumLanded","varWgtLanded","varLgtLanded")
  
  SD <- SD %>% 
    mutate_if( is.character, as.factor) %>% 
    mutate_at(vars(AgeLength, NumSamplesLngt:NumAgeMeas, NumberCaught:MeanLength), function(x) as.numeric(as.character(x))) # transform effort into numeric
  
  out <- return(list(HI=HI, SI=SI, SD=SD))
  return(out)
}