# Script information ------------------------------------------------------

# Title: Function to transform SD files by length into ages
# Based on code developed by Gersom Costas for horse mackerel
# Sonia Sanchez (ssanchez@azti.es) and Leire Ibaibarriaga (libaibarriaga@azti.es) 
# Date: 2021/06/01

# PARAMETERS:
#
# lw      : NULL takes weights from IC, else takes from this object
#           el data.frame vendra del fichero lw_STKN_yyyy.csv con Season SeasonType a b unitsl unitsw
# sc.perc : TRUE if SampledCatch expressed as percentage, 
#           in other case in same units as catches

lngt2age <- function(IC, alk, lw = NULL, sc.perc = TRUE){

  require(tidyverse)
  require(measurements) # change units

  options(dplyr.summarise.inform = FALSE) # to avoid informative messages
  
  # check if IC and alk have the same length classes
  
  lclass <- min(diff(sort(unique(alk$AgeLength))))
  if ( min(diff(sort(unique(IC$SD$AgeLength)))) !=  lclass )
    stop("Check if the length classes used in the IC file and in the ALKs are the same")

  # join the IC file and the alk file 
  
  out <- left_join(IC$SD, alk, by=c("FishingArea", "SeasonType", "Season", "AgeLength"))%>%
    mutate(across(starts_with("a", ignore.case = FALSE),  ~ .x * NumberCaught, .names = "nage_{.col}"))
  
  # check the ALKs cover the IC file length-frequency distribution appropriately
  
  if (any(is.na(out$Total))){
    print("Missing ALK information in the following cases")
    print(subset(out, is.na(Total), select=c("FishingArea", "SeasonType", "Season", "AgeLength")))
    stop()
  }
  
  if (any(out$NumberCaught > 0 & out$Total ==0, na.rm=T)){ 
    print("Missing ALK information in the following cases")
    print(subset(out, (NumberCaught>0 & Total==0), select=c("FishingArea", "SeasonType", "Season", "AgeLength")))
    stop()
  }
  
  # check if mean weights are correct given length-weight relationship
  
  if(!is.null(lw)){
    if (unique(out$SeasonType)==unique(lw$SeasonTypeLW)){
      aux <- out %>%
        mutate(SeasonTypeLW=out$SeasonType,
               SeasonLW=Season)
    }else if (unique(out$SeasonType)=="Quarter" & unique(lw$SeasonTypeLW)=="Semester"){
      aux <- out %>% 
        mutate(SeasonLW=ifelse(Season %in% c(1,2), 1, 2),
               SeasonTypeLW="Semester") 
    }else if (unique(out$SeasonType)=="Quarter" & unique(lw$SeasonType)=="Year"){
      aux <- out %>% 
        mutate(SeasonLW=1,
               SeasonTypeLW="Year") 
    }else if (unique(out$SeasonType)=="Month" & unique(lw$SeasonType)=="Quarter"){
      aux <- out %>% 
        mutate(SeasonLW=ifelse(Season %in% c(1,2,3), 1, ifelse( Season%in% c(4,5,6), 2, ifelse(Season %in% c(7,8,9), 3, 4))),
               SeasonTypeLW="Quarter") 
    }else if (unique(out$SeasonType)=="Month" & unique(lw$SeasonType)=="Semester"){
      aux <- out %>% 
        mutate(SeasonLW=ifelse(Season %in% c(1:6), 1, 2),
               SeasonTypeLW="Semester") 
    }else if (unique(out$SeasonType)=="Month" & unique(lw$SeasonType)=="Year"){
      aux <- out %>% 
        mutate(SeasonLW=1,
               SeasonTypeLW="Year") 
    }else{
      stop(" Not appropriate SeasonTypes for the LW file")
    }
    aux <- left_join(aux, lw, by=c("SeasonTypeLW", "SeasonLW")) %>% 
      mutate(Length=conv_unit(AgeLength, unique(UnitMeanLength), unique(UnitMeanLengthLW)),
             MeanWeightLW=a*(Length+lclass/2)^b,
             MeanWeightLW=conv_unit(MeanWeightLW, unique(unitMeanWeightLW), unique(unitMeanWeight)),
             Diff= MeanWeightLW-MeanWeight)
    if(any(abs(aux$Diff)>0.001)){
      stop("The LW parameters do not match the MeanWeight in the IC file")
    }
  } # end length-weight model check 
  
  # Identify age of the PlusGroup using the ALK names
  
  ageclasses <- names(alk)[grep(pattern="^a[0-9]", names(alk))]
  age <- str_extract(ageclasses, "\\-*\\d+\\.*\\d*")
  
  idx <- grep(pattern="plus", ageclasses)
  plusgroup <- ifelse(length(idx)>0, str_extract(ageclasses[idx], "\\-*\\d+\\.*\\d*"), NA)

  if (all(out$AgeLength==out$MeanLength)){
    print("MeanLength corrected to the midpoint of the class")
    out <- out %>% 
      mutate(MeanLength=MeanLength+lclass/2)
  }
  
  # compute numbers-at-age, weights-at-age and lengths-at-age
  
  nage <- out %>% 
    group_by(ReportingCategory, CatchCategory, FishingArea, Fleet, Season) %>% 
    summarise(across(starts_with("nage_"), ~ sum(., na.rm=T))) %>% 
    ungroup()
  
  wage <- out %>% 
    group_by(ReportingCategory, CatchCategory, FishingArea, Fleet, Season) %>% 
    summarise(across(starts_with("nage"), ~ weighted.mean(MeanWeight, w=., na.rm=T), .names = "wage{.col}")) %>% 
    ungroup()
  
  lage <- out %>% 
    group_by(ReportingCategory, CatchCategory, FishingArea, Fleet, Season) %>% 
    summarise(across(starts_with("nage"), ~ weighted.mean(MeanLength, w=., na.rm=T), .names = "lage{.col}")) %>% 
    ungroup()
  
  # sops (sum of products between nage and wage)
  
  nage_long <- pivot_longer(nage, cols=-c(ReportingCategory:Season), values_to = "NumberCaught", names_to = c("nage","Age"), names_sep="_") %>% 
    select(-nage)
  wage_long <- pivot_longer(wage, cols=-c(ReportingCategory:Season), values_to = "MeanWeight", names_to = c("wage","Age"), names_sep="_")%>% 
    select(-wage)
  lage_long <- pivot_longer(lage, cols=-c(ReportingCategory:Season), values_to = "MeanLength", names_to = c("lage","Age"), names_sep="_")%>% 
    select(-lage)
  
  sop_long <- left_join(nage_long, wage_long, by=c("ReportingCategory","CatchCategory","FishingArea","Fleet","Season","Age")) %>% 
    mutate(sop=NumberCaught*MeanWeight) %>% 
    select(- c(NumberCaught:MeanWeight)) 
  
  sop <- sop_long %>% 
    pivot_wider(id_cols=ReportingCategory:Season, names_from=Age, values_from=sop, names_prefix="sop_") %>% 
    mutate(sop_total=rowSums(across(starts_with("sop_", ignore.case = FALSE)), na.rm=T) )
  
  checksop <- left_join(IC$SI, sop, by=c("ReportingCategory","CatchCategory", "FishingArea", "Fleet", "Season")) %>% 
    select(c("ReportingCategory","CatchCategory", "FishingArea", "Fleet", "Season","CATON", "sop_total")) %>% 
    mutate(dif=CATON-sop_total)
  
  # cases in SI without length distribution in SD

  if (sum(checksop$CATON>0 & is.na(checksop$sop_total), na.rm=T)>0){ 
    print("The following cases do not have length frequency distribution data")
    print(subset(checksop, CATON>0 & is.na(sop_total), select=c("ReportingCategory","CatchCategory", "FishingArea", "Fleet", "Season","CATON")))
  }
  
  # cases with big SOPs that should be checked

  if (sum(checksop$CATON>0 & checksop$dif>0.001, na.rm=T)>0){ 
    print("The following cases have large SOPs")
    print(subset(checksop, CATON>0 & dif>0.001))
  }
  
  # prepare output file
  
  SDage <- IC$SD %>% 
    select("RecordType","Country","Year","SeasonType","Season","Fleet","AreaType","FishingArea","DepthRange",
           "Species", "Stock", "CatchCategory", "ReportingCategory", "Sex", "SampledCatch", "unitMeanWeight",
           "unitCANUM", "UnitMeanLength", "Maturity",
           "NumSamplesLngt","NumLngtMeas") %>% 
    distinct() 
  
  if (sc.perc == FALSE) {
    SIcaton <- IC$SI %>% select(Country:ReportingCategory, CATON) # sampled catches
    SDage <- SDage %>% left_join(SIcaton) %>% mutate(SampledCatch = CATON) %>% select(-CATON)
  } 
  
  SDage <- SDage %>% 
    left_join(., nage_long, by=c("ReportingCategory","CatchCategory","FishingArea","Fleet","Season")) %>% 
    left_join(., wage_long, by=c("ReportingCategory","CatchCategory","FishingArea","Fleet","Season","Age")) %>% 
    left_join(., lage_long, by=c("ReportingCategory","CatchCategory","FishingArea","Fleet","Season","Age")) %>% 
    #filter(NumberCaught>0) %>%  ### quitar esto porque si no quitamos los 0s, y el formato IC los necesita
    mutate(AgeLength=str_extract(Age, "\\-*\\d+\\.*\\d*")) %>% # extract the number
    # mutate(AgeLength=str_sub(Age, start=2)) %>% 
    # mutate(AgeLength=recode_factor(AgeLength, `15plus`="15")) %>% 
    select(-Age) %>% 
    mutate(CANUMtype="age",
           PlusGroup=plusgroup, 
           UnitAgeOrLength="year",
           NumSamplesAge= -9,
           NumAgeMeas= -9,
           varNumLanded= -9,
           varWgtLanded= -9,
           varLgtLanded= -9) %>% 
    select("RecordType","Country","Year","SeasonType","Season","Fleet","AreaType","FishingArea","DepthRange","Species",
           "Stock","CatchCategory","ReportingCategory","Sex","CANUMtype","AgeLength","PlusGroup","SampledCatch",
           "NumSamplesLngt","NumLngtMeas","NumSamplesAge","NumAgeMeas","unitMeanWeight","unitCANUM","UnitAgeOrLength",
           "UnitMeanLength","Maturity","NumberCaught","MeanWeight","MeanLength","varNumLanded","varWgtLanded","varLgtLanded")
    
  
  # NOTE: age sampling is not included here
  
  out <- return(list(HI=IC$HI, SI=IC$SI, SD=SDage))
  return(out)
}
