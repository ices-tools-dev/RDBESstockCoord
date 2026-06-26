## Chun Chen, chun.chen@wur.nl
## script used to test RDBES format based raising for ple.27.420, based on Yves script


library(dplyr)
library(tidyr)
library(readr)
library(rlang)
library(data.table)
library(stringr)


mypath     <- "C:/Users/chen072/OneDrive - Wageningen University & Research/0_2026_RDBES/FLOW/ple27420/"

setwd(mypath)
scriptPath <- mypath 
dat_path   <- "./data_input"
dat_out_path <- "./data_output"
source(paste0(scriptPath, "0_Functions_other.R"))
source(paste0(scriptPath, "0_Functions_discards_raising.R"))
source(paste0(scriptPath, "0_Functions_age-length_alloc.R"))

## census <- read_csv(file.path(dataDir, "pok_2022_census_catches.csv"))

## catch_estimates <- read_csv(file.path(dat_out_path, "pok_2022_estimated_catches.csv"))

distributions <- read_csv(file.path(dat_out_path, "ple_2025_distributions_CEF_v17.1.csv"))

catch_data <- read_csv(file.path(dat_out_path, "ple_2025_catches_CEF_v17.1.csv"))

names(distributions)
## names(catch_estimates)
names(catch_data)

## catch_data %>% head(2) %>% as.data.frame()
## distributions %>% group_by(variableUnit) %>% slice_head(n = 1) %>% as.data.frame()

## ##################################################
## data preprocessing
## exclude ireland?
sum(catch_data$total[catch_data$vesselFlagCountry == "Ireland"])
catch_data <- catch_data %>%
  filter(vesselFlagCountry != "Ireland")
distributions <- distributions %>%
  filter(vesselFlagCountry != "Ireland")

## ##################################################
## define strata
mainCo <- c("Belgium", "Denmark" , "France",  "Germany", "Netherlands", "Norway","Sweden", "UK (England)", "UK(Scotland)") 

catch_data <- catch_data %>%
    mutate(Country = vesselFlagCountry,
           Season = seasonValue,
           #domainCatchDis_ctr = paste(vesselFlagCountry, domainCatchDis, sep = "_"),
           domainCatchDis_ctr = if_else(!is.na(domainCatchDis),
                                        paste(vesselFlagCountry, domainCatchDis, sep = "_"),
                                        NA_character_),
           ## That could be in a function as Metier6 is standard:------------------------
           gear = gsub("^(([^_]+)_([^_]+))_([^_]+)_.*$", "\\2", fleetValue),
           target = gsub("^(([^_]+)_([^_]+))_([^_]+)_.*$", "\\3", fleetValue),
           gear_target = gsub("^(([^_]+)_([^_]+))_([^_]+)_.*$", "\\1", fleetValue),
           mesh = gsub("^(([^_]+)_([^_]+))_([^_]+)_.*$", "\\4", fleetValue),
           Area1 = gsub("27\\.([[:digit:]]+)(\\..*)?", "\\1", areaValue)
    )
           ## ---------------------------------------------------------------------------
           ## TR1 def.:
           #FleetType = case_when((gear_target %in% c("OTB_DEF", "OTT_DEF") |
           #                       gear %in% c("SDN", "SSC", "PTB")) &
            #                     mesh %in% c(">=120", ">=220", "100-119", "120-219") ~ "TR1",
            #                     TRUE ~ "Other"))

define_raising_strata <- function (dat1) {
  dat1$Fleetgroup <- NA
  
  ##  TBB <100, >=100
  dat1$Fleetgroup[substr(dat1$fleetValue, 1,3) == "TBB" & substr(dat1$fleetValue, 9,13) == "70-99"] <- "TBB<100"
  dat1$Fleetgroup[substr(dat1$fleetValue, 1,3) == "TBB" & substr(dat1$fleetValue, 9,13) %in% c(">=120", "100-1")] <- "TBB>=100"
  ##  TBB/OTB shrimp
  dat1$Fleetgroup[substr(dat1$fleetValue, 1,3) %in% c("TBB", "OTB") & substr(dat1$fleetValue, 9,13) %in% c("16-31")] <- "TBB_OTB_16-31"
  ## OTB/OTM <100 >=100
  table(substr(dat1$fleetValue[substr(dat1$fleetValue, 1,3) %in% c("OTB", "OTM")], 9,13))
  dat1$Fleetgroup[substr(dat1$fleetValue, 1,3) %in% c("OTB", "OTM") & substr(dat1$fleetValue, 9,13) %in% c("", "32-69", "70-89", "70-99", "90-11")] <- "OTB<100"
  dat1$Fleetgroup[substr(dat1$fleetValue, 1,3) %in% c("OTB", "OTM") & substr(dat1$fleetValue, 9,13) %in% c(">=120", "100-1")] <- "OTB>=100"
  ind <- which(substr(dat1$fleetValue, 1,3) %in% c("OTB", "OTM"))
  unique(cbind(dat1$Fleetgroup[ind], dat1$fleetValue[ind]))            
  ## SSC <100 >=100
  table(substr(dat1$fleetValue[substr(dat1$fleetValue, 1,3) %in% c("SSC", "SDN")], 9,13))
  table(substr(dat1$fleetValue[substr(dat1$fleetValue, 1,3) %in% c("SSC", "SDN")], 4,13))
  dat1$Fleetgroup[substr(dat1$fleetValue, 1,3) %in% c("SSC", "SDN") & substr(dat1$fleetValue, 9,13) %in% c("", "32-69", "70-89", "70-99", "90-11")] <- "SSC_SDN<100"
  dat1$Fleetgroup[substr(dat1$fleetValue, 1,3) %in% c("SSC", "SDN") & substr(dat1$fleetValue, 9,13) %in% c(">=120", "100-1", "0_0_a", "All_0")] <- "SSC_SDN>=100"
  ind <- which(substr(dat1$fleetValue, 1,3) %in% c("SSC", "SDN"))
  unique(cbind(dat1$Fleetgroup[ind], dat1$fleetValue[ind]))
  ## GNS etc
  table(substr(dat1$fleetValue[substr(dat1$fleetValue, 1,3) %in% c("GNS", "GTR", "GTS")], 9,13))
  table(substr(dat1$fleetValue[substr(dat1$fleetValue, 1,3) %in% c("GNS", "GTR", "GTS")], 4,13))
  dat1$Fleetgroup[substr(dat1$fleetValue, 1,3) %in% c("GNS", "GTR", "GTS")] <- "GNS_GTR"
  ind <- which(substr(dat1$fleetValue, 1,3) %in% c("GNS", "GTR", "GTS"))
  unique(cbind(dat1$Fleetgroup[ind], dat1$fleetValue[ind]))
  ## other
  unique(dat1$fleetValue[is.na(dat1$Fleetgroup)])
  dat1$Fleetgroup[is.na(dat1$Fleetgroup)] <- "other"
  ind <- which(dat1$Fleetgroup == "other")
  unique(cbind(dat1$Fleetgroup[ind], dat1$fleetValue[ind]))
  
  ##
  dat1$Fleetgroup <- factor(dat1$Fleetgroup, levels=c("TBB<100", "TBB>=100", "OTB<100", "OTB>=100", "SSC_SDN<100", "SSC_SDN>=100", "TBB_OTB_16-31", "GNS_GTR", "other"))
  return(dat1)
}

catch_data <- define_raising_strata(catch_data)
head(catch_data, 2) %>% as.data.frame()
table(catch_data$Fleetgroup, useNA="always")

#define_raising_strata_simple <- function (dat1) {
  dat1$Fleetgroup <- NA
  
  ##  TBB <100, >=100
  dat1$Fleetgroup[substr(dat1$fleetValue, 1,3) == "TBB" & substr(dat1$fleetValue, 9,13) == "70-99"] <- "TBB<100"
  dat1$Fleetgroup[substr(dat1$fleetValue, 1,3) == "TBB" & substr(dat1$fleetValue, 9,13) %in% c(">=120", "100-1")] <- "TBB>=100"
  ## other
  unique(dat1$fleetValue[is.na(dat1$Fleetgroup)])
  dat1$Fleetgroup[is.na(dat1$Fleetgroup)] <- "other"
  ind <- which(dat1$Fleetgroup == "other")
  unique(cbind(dat1$Fleetgroup[ind], dat1$fleetValue[ind]))
  
  ##
  dat1$Fleetgroup <- factor(dat1$Fleetgroup, levels=c("TBB<100", "TBB>=100", "other"))
  return(dat1)
}

#catch_data <- define_raising_strata_simple(catch_data)
#head(catch_data, 2) %>% as.data.frame()
#table(catch_data$Fleetgroup, useNA="always")

## ###########################################################################
## checking available discards

## script_2_1_discards estimation coverage.R

## ###########################################################################
## checking submitted discards ratio

dis_ratio <- catch_data %>%
  filter(originType == "WGEstimate",
         variableType == "WeightLive",
         !is.na(total)) %>%
  #mutate(domainCatchDis_ctr = paste(vesselFlagCountry, domainCatchDis, sep = "_")) %>%
  group_by(Fleetgroup, Season, domainCatchDis_ctr, catchCategory) %>%
  summarise(total = sum(total, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from  = catchCategory,
              values_from = total) %>%
  filter(!is.na(Lan), !is.na(Dis)) %>%
  mutate(dis_lan_ratio = Dis / Lan) %>%
  arrange(Fleetgroup, Season, desc(dis_lan_ratio))

## ###########################################################################
## Raising discards:

## ##################################################
## define group for raising discards:

make_strataCond <- function(iyear, strataCond_config) {
  
  strata_list <- list()
  
  for (fg in names(strataCond_config)) {
    season_type <- strataCond_config[[fg]]
    
    if (season_type == "year") {
      # one group foe all seasons
      Gname <- paste0(fg, "_year")
      strata_list[[Gname]] <- quo(Fleetgroup == !!fg)
      
    } else if (season_type == "season") {
      # one group per quarter (1-4), no year
      for (s in c(1, 2, 3, 4)) {
        Gname <- paste0(fg, "_Q", s)
        strata_list[[Gname]] <- quo(Fleetgroup == !!fg & Season == !!s)
      }
      
    } else if (season_type == "season_year") {
      # one group per quarter + one for year
      for (s in c(1, 2, 3, 4)) {
        Gname <- paste0(fg, "_Q", s)
        strata_list[[Gname]] <- quo(Fleetgroup == !!fg & Season == !!s)
      }
      Gname <- paste0(fg, "_", iyear)
      strata_list[[Gname]] <- quo(Fleetgroup == !!fg & Season == iyear)
    }
  }
  
  return(strata_list)
}

## this grouping strategy is based on the visualization of script_2_1_discards estimation coverage.R
strataCond_config <- list(
  "TBB<100"       = "season_year",
  "TBB>=100"      = "year",
  "OTB<100"       = "season_year",
  "OTB>=100"      = "season_year",
  "SSC_SDN<100"   = "year",
  "SSC_SDN>=100"  = "year",
  "TBB_OTB_16-31" = "season",
  "GNS_GTR"       = "year",
  "other"         = "year"
)
## a simple one
strataCond_config <- list(
  "TBB<100"       = "season_year",
  "TBB>=100"      = "year",
  "OTB<100"       = "season_year",
  "OTB>=100"      = "season_year"
)

strataCond <- make_strataCond(2025, strataCond_config)
length(strataCond)
names(strataCond)

## ##################################################
## define matched strata
## default
matchedDataCond <- strataCond

## make modifications:
# script_2_2_match_group_modification.R

## ##################################################
## Condition tests (optional automatically ran with
##   raising, but good practice):

cond_test <- check_group_conditions(catch_data = catch_data,
                                    condition_list = strataCond,
                                    logFile = NULL, append = TRUE)

cond_test2 <- check_group_conditions(catch_data = catch_data,
                                     condition_list = matchedDataCond,
                                     conditionType = "matched_data",
                                     logFile = NULL, append = TRUE)

## ##################################################
## Discards raising:

catch_data_raised <-
    catchRaising(catch_data = catch_data, # %>% select(-recordType),
                 condition_raising_st_list = strataCond,
                 condition_matched_data_list = matchedDataCond, # Optional if same as
                                        # raising strata (condition_raising_st_list)!
                 type = "discards",
                 originType = "WGEstimate",
                 variableType = "WeightLive", 
                 logFile = "Log.txt",
                 assembled_output = TRUE)

aa <- catch_data_raised[catch_data_raised$importedOrRaised == "raised",]
sum(aa$total)
data.frame(aa[aa$Country == "Denmark",])
dim(catch_data)
dim(catch_data_raised)
catch_data %>% filter(total > 0) %>% group_by(catchCategory, originType) %>% slice_sample(n = 2) %>% as.data.frame()
catch_data_raised %>% filter(total > 0) %>% group_by(catchCategory, originType, importedOrRaised) %>% slice_sample(n = 2) %>% as.data.frame()

## here i want to save the results...


## ###########################################################################
## Age allocation:

## ##################################################
## Group definitions for age allocation:
make_allocstrataCond <- function(iyear, strataCond_config, catchCategory) {
  
  cat_expr <- quo(catchCategory %in% !!catchCategory)
  
  strata_list <- list()
  
  for (fg in names(strataCond_config)) {
    season_type <- strataCond_config[[fg]]
    
    if (season_type == "year") {
      Gname <- paste0(fg, "_year")
      strata_list[[Gname]] <- quo(Fleetgroup == !!fg & !!cat_expr)
      
    } else if (season_type == "season") {
      for (s in c(1, 2, 3, 4)) {
        Gname <- paste0(fg, "_Q", s)
        strata_list[[Gname]] <- quo(Fleetgroup == !!fg & Season == !!s & !!cat_expr)
      }
      
    } else if (season_type == "season_year") {
      for (s in c(1, 2, 3, 4)) {
        Gname <- paste0(fg, "_Q", s)
        strata_list[[Gname]] <- quo(Fleetgroup == !!fg & Season == !!s & !!cat_expr)
      }
      Gname <- paste0(fg, "_", iyear)
      strata_list[[Gname]] <- quo(Fleetgroup == !!fg & Season == !!iyear & !!cat_expr)
    }
  }
  
  return(strata_list)
}


## a simple one
strataCond_config <- list(
  "TBB<100"       = "season_year",
  "TBB>=100"      = "year",
  "OTB<100"       = "season_year",
  "OTB>=100"      = "season_year"
)

strataCond_config <- list(
  "TBB<100"       = "season_year"
)

#allocStrataCond <- make_strataCondalloc(iyear = 2025, strataCond_config, catchCategory = c("Dis", "BMS"))
allocStrataCond <- make_allocstrataCond(iyear = 2025, strataCond_config, catchCategory = "Lan")

## test this group
allocStrataCond <- allocStrataCond["TBB<100_Q1"]
bioMatchedCond <- allocStrataCond

bioMatchedCond[["TBB<100_Q1"]] <- quo(Season %in% c(1,2025) & 
                                         Fleetgroup == "TBB<100" & catchCategory == "Lan"
)


## Check conditions (optional - automatically done during allocation):

cond_bio_test <- check_group_conditions(catch_data = catch_data_raised,
                                        condition_list = allocStrataCond,
                                        logFile = NULL, append = TRUE,
                                        domain = "domainBiology")

sapply(cond_bio_test, head, simplify = FALSE)

cond_bio_test2 <- check_group_conditions(catch_data = catch_data_raised,
                                         condition_list = bioMatchedCond,
                                         conditionType = "matched_data",
                                         logFile = NULL, append = TRUE,
                                         domain = "domainBiology")

sapply(cond_bio_test2, head, simplify = FALSE)

## Allocation of catch numbers and mean weights at age:
CaAallocTest <-
    catchAllocationAoL(catch_data = catch_data_raised,
                       distribution_data = distributions,
                       condition_alloc_st_list = allocStrataCond,
                       condition_matched_data_list = bioMatchedCond, # Optional if same as
                                        # allocation strata (condition_raising_st_list)!
                       originType_catch = "WGEstimate", # fraction to apply it to.
                       variableType_catch = "WeightLive",
                       distributionType = "Age",
                       variableType_mean = "WeightLive",
                       logFile = "Log.txt", ## append = TRUE,
                       assembled_output = TRUE)

## Returns a list of table -> extraction of those:
distribution_alloc <- CaAallocTest$distribution
catch_alloc <- CaAallocTest$catch

dim(distribution_alloc)

aa <- distribution_alloc[distribution_alloc$sampledOrEstimated == "estimated",]
table(aa$domainBiology)
bb <- distribution_alloc[distribution_alloc$domainBiology == "[alloc]_1_27.4.b_NA_TBB_DEF_70-99_0_0_all_Lan",]
data.frame(bb)

##here i stopped with testing
## ##################################################
## Aggregated catch numbers at age:

## Catch number at age 0 to 10 (not a plus group):
catchAtAoLAggregation(distribution_alloc,
                      catch_alloc,
                      grouping = c("catchCategory", "importedOrRaised"),
                      maxAoL = 10,
                      unit = "NE3", round = 2, # Conversion to 1000s
                      plusGroup = FALSE) %>% as.data.frame()

## Catch number at age 3 to 10+ (as used in assessment)
##   +default grouping (catchCategory and attributes):
catchAtAoLAggregation(distribution_alloc,
                      catch_alloc,
                      minAoL = 3,
                      maxAoL = 10,
                      plusGroup = TRUE,
                      round = 0) %>% as.data.frame()


## ##################################################
## Aggregated catch weights at age:

meanWoLatAoLAggregation(distribution_alloc,
                        catch_alloc,
                        minAoL = 3,
                        maxAoL = 10,
                        plusGroup = TRUE,
                        unit = "g",
                        round = 0) %>% as.data.frame()

meanWoLatAoLAggregation(distribution_alloc,
                        catch_alloc,
                        minAoL = 3,
                        maxAoL = 10,
                        plusGroup = TRUE,
                        unit = "kg",
                        round = 3) %>% as.data.frame()

## All catch:
meanWoLatAoLAggregation(distribution_alloc,
                        catch_alloc,
                        grouping = c(##"catchCategory",
                                       "attributeType",
                                       "attributeValue"),
                        minAoL = 3,
                        maxAoL = 10,
                        plusGroup = TRUE,
                        unit = "g",
                        round = 0) %>% as.data.frame()

## All ages, in kg:
meanWoLatAoLAggregation(distribution_alloc,
                        catch_alloc,
                        grouping = c("catchCategory",
                                     "attributeType",
                                     "attributeValue"),
                        minAoL = 3,
                        maxAoL = NA,
                        plusGroup = FALSE,
                        unit = "kg",
                        round = 3) %>% as.data.frame()

## 3-10+, in kg, per catch category AND season:
meanWoLatAoLAggregation(distribution_alloc,
                        catch_alloc,
                        grouping = c("catchCategory",
                                     "seasonValue", ## <-
                                     "attributeType",
                                     "attributeValue"),
                        minAoL = 3,
                        maxAoL = 10,
                        plusGroup = TRUE,
                        unit = "kg",
                        round = 3) %>% as.data.frame()

## ##################################################
## No need for fancy functions to aggregate
##  catch data, only sum per groups:

catonCateg <- catch_alloc %>%
    group_by(variableType, originType, # Recommended to keep this always!
             variableUnit,             # ...that too.
             catchCategory,
             importedOrRaised) %>%
    summarize(total = sum(total, na.rm = TRUE),
              .groups = "drop") # Ungroup to allow conversion.

print(catonCateg)

## Conversion to metric tonnes:
convert_field(data = catonCateg,
              unitField = "variableUnit",
              valueField = "total",
              to = "t")


## ####################################################################################################
## ####################################################################################################
## Examples other functions:

## ##################################################
## Unit conversion:
distribution_alloc %>%
    group_by(variableType, sampledOrEstimated) %>%
    slice_head(n = 2) %>%
    ## convert_field("value") %>%
    as.data.frame()

##  Conversion to kg and NE3:
distribution_alloc %>%
    group_by(variableType, sampledOrEstimated) %>%
    slice_head(n = 2) %>%
    convert_field(valueField = "value",
                  to = c("kg", "NE3")) %>% # ignores silently invalid conversions.
    as.data.frame()

## ###########################################################################
## Example of groups based on a simple strata (one or several fields),
## matched to same:
colnames(catch_data)

testCondGear <- fieldsToStrata(catch_data, # here for discard raising conditions;
                                        # should use catch_data_raised if groups for allocations. 
                               "gear")

## First six elements of each group:
sapply(testCondGear, head)

## Same with a combination of 2 fields
##   (use any tidy synthaxe compatible with dplyr::select()):
testCondGearA <- fieldsToStrata(catch_data,
                                c(gear, Area1))

## The returned object is a list of logical vector
##   (indicating which rows are included in which group):
head(sapply(testCondGearA, head, n = 10, simplify = FALSE))

nrow(catch_data)
sapply(testCondGearA, length)




## ###########################################################################
## Explore and compare results:

## ##################################################
## Discards:
catch_data_raised %>%
    group_by(cc = catchCategory, importedOrRaised) %>%
    slice_sample(n = 1) %>%
    as.data.frame()

## Total catch broken down by catch categories:
catch_data_raised %>%
    convert_field(valueField = "total",
                  to = c("t")) %>%
    group_by(catchCategory) %>%
    summarize(catch_t = sum(total, na.rm = TRUE))


## Total catch broken down by catch categories and imported or raised:
catch_data_raised %>%
    convert_field(valueField = "total",
                  to = c("t")) %>%
    group_by(catchCategory, importedOrRaised) %>%
    summarize(catch_t = sum(total, na.rm = TRUE))

## ## From Intercatch(-like) TAF reproduced raising:
## if (all(file.exists(file.path(dataDir,
##                               c("pok_2022_CatchAndSampleDataTables-1.csv",
##                                 "pok_2022_catonR_tot.csv")))))
## {
##     overview <- read_tsv(file = file.path(dataDir,
##                                           "pok_2022_CatchAndSampleDataTables-1.csv")) %>%
##         mutate(gear = gsub("^(([^_]+)_([^_]+))_([^_]+)_.*$", "\\2", Fleet),
##                target = gsub("^(([^_]+)_([^_]+))_([^_]+)_.*$", "\\3", Fleet),
##                gear_target = gsub("^(([^_]+)_([^_]+))_([^_]+)_.*$", "\\1", Fleet),
##                mesh = gsub("^(([^_]+)_([^_]+))_([^_]+)_.*$", "\\4", Fleet),
##                Area1 = gsub("27\\.([[:digit:]]+)(\\..*)?", "\\1", Area),
##                ## TR1 def.:
##                FleetType = case_when((gear_target %in% c("OTB_DEF", "OTT_DEF") |
##                                       gear %in% c("SDN", "SSC", "PTB")) &
##                                      mesh %in% c(">=120", ">=220", "100-119", "120-219") ~ "TR1",
##                                      TRUE ~ "Other"))

##     ## From WKRDBES_Raise&TAF outputs:
##     overview2 <- read_csv(file.path(dataDir, "pok_2022_catonR_tot.csv"))

##     head(overview, 2) %>% as.data.frame()
##     head(overview2, 2) %>% as.data.frame()
##     head(catch_data_raised, 2) %>% as.data.frame()

##     overview %>%
##         filter(CatchCategory %in% "Discards") %>%
##         group_by(CatchCategory, CATONRaisedOrImported, FleetType) %>%
##         summarize(catch_t = sum(CATON, na.rm = TRUE) * 1e-3)

##     overview2 %>%
##         filter(Catch.Cat. %in% "D") %>%
##         group_by(Catch.Cat., Discards.Imported.Or.Raised, FleetType) %>%
##         summarize(catch_t = sum(Catch..kg, na.rm = TRUE) * 1e-3) %>%
##         tail(4)

##     catch_data_raised %>%
##         filter(catchCategory %in% "Dis") %>%
##         group_by(catchCategory, importedOrRaised, FleetType) %>%
##         summarize(catch_t = sum(total, na.rm = TRUE) * 1e-3)

##     catch_data_raised %>%
##         filter(catchCategory %in% "Dis") %>%
##         group_by(catchCategory, importedOrRaised, FleetType, Season) %>%
##         summarize(catch_t = sum(total, na.rm = TRUE) * 1e-3)

##     overview %>%
##         filter(CatchCategory %in% "Discards") %>%
##         group_by(CatchCategory, CATONRaisedOrImported, FleetType, Season) %>%
##         summarize(catch_t = sum(CATON, na.rm = TRUE) * 1e-3)

##     catch_data_raised %>% group_by(Season) %>% slice_sample(n = 1) %>% as.data.frame()

##     catch_data_raised %>%
##         filter(catchCategory %in% "Dis",
##                ! Country %in% mainCo) %>%
##         group_by(catchCategory, importedOrRaised, FleetType, Season) %>%
##         summarize(catch_t = sum(total, na.rm = TRUE) * 1e-3) %>%
##         tail(4)

##     overview %>%
##         filter(CatchCategory %in% "Discards",
##                ! Country %in% mainCo) %>%
##         group_by(CatchCategory, CATONRaisedOrImported, FleetType, Season) %>%
##         summarize(catch_t = sum(CATON, na.rm = TRUE) * 1e-3) %>%
##         tail(4)

##     overview2 %>%
##         filter(Catch.Cat. %in% "D",
##                ! Country %in% mainCo) %>%
##         group_by(Catch.Cat., Discards.Imported.Or.Raised, FleetType, Season) %>%
##         summarize(catch_t = sum(Catch..kg, na.rm = TRUE) * 1e-3) %>%
##         tail(4)

##     ##
##     Comparisons_pok_2022 <- catch_data_raised %>%
##         filter(catchCategory %in% "Dis") %>%
##         group_by(catchCategory, importedOrRaised, DrGroup) %>%
##         summarize(catch_t = sum(total, na.rm = TRUE) * 1e-3) %>%
##         inner_join(overview2 %>%
##                    filter(Catch.Cat. %in% "D") %>%
##                    group_by(Catch.Cat., Discards.Imported.Or.Raised, DrGroup) %>%
##                    summarize(catch_t = sum(Catch..kg, na.rm = TRUE) * 1e-3) %>%
##                    mutate(Discards.Imported.Or.Raised = tolower(Discards.Imported.Or.Raised)),
##                    by = c("importedOrRaised" = "Discards.Imported.Or.Raised",
##                           "DrGroup" = "DrGroup"),
##                    suffix = c(".new", ".IC")) %>%
##         mutate(grN = as.numeric(sub("G", "", DrGroup)),
##                Catch.Cat. = NULL,
##                perc.change = round(100 * (catch_t.IC - catch_t.new) / catch_t.IC,
##                                    2)) %>%
##         arrange(grN) %>%
##         mutate(grN = NULL)

##     Comp_overview_pok_2022 <- catch_data_raised %>%
##         group_by(catchCategory, importedOrRaised, originType, variableType) %>%
##         summarize(catch_t = sum(total, na.rm = TRUE) * 1e-3) %>%
##         mutate(importedOrRaised = ifelse(importedOrRaised %in% c("estimated", "reported"),
##                                          "imported", importedOrRaised),
##                catchCategoryIC = sub("^(.).*$", "\\1", catchCategory),
##                catchCategoryIC = if_else(catchCategory %in% "Dis" &
##                                          originType %in% "Official",
##                                          "R", catchCategoryIC)) %>%
##         full_join(overview2 %>%
##                   group_by(Catch.Cat., Discards.Imported.Or.Raised) %>%
##                   summarize(catch_t = sum(Catch..kg, na.rm = TRUE) * 1e-3) %>%
##                   mutate(Discards.Imported.Or.Raised = tolower(Discards.Imported.Or.Raised)),
##                   by = c("importedOrRaised" = "Discards.Imported.Or.Raised",
##                          "catchCategoryIC" = "Catch.Cat."),
##                   suffix = c(".new", ".IC")) %>%
##         arrange(catchCategory) %>%
##         mutate(perc.change = round(100 * (catch_t.IC - catch_t.new) / catch_t.IC,
##                                    2)) %>%
##         select(catchCategory, originType, variableType, importedOrRaised, catchCategoryIC, catch_t.new, catch_t.IC,
##                perc.change) %>%
##         as.data.frame()

##     table(catch_data_raised$DrGroup, catch_data_raised$catchCategory)

##     print(Comp_overview_pok_2022)

##     print(Comparisons_pok_2022 %>%
##           filter(importedOrRaised %in% "raised"),
##           n = 1000)
## }

## ##################################################
## Age allocations:

## stockOverviewRaised <- read_csv(file.path("../2023_pok.27.3a46_RDBES_combined/output",
##                                           "StockOverview_raised.txt"))

## ## stockOverviewRaised %>% head(1) %>% as.data.frame()

## stockOverviewRaised %>%
##     filter(CatchCat %in% "Discards",
##            is.na(Caton)) %>%
##     group_by(AGroup, estimated = is.na(Caton)) %>%
##     dplyr::summarize(across(starts_with("N.UndeterminedAge"),
##                             ~ round(sum(.x, na.rm = TRUE)))) %>%
##     rename_with(~sub("N.UndeterminedAge", "N_Age_", .x)) %>%
##     select(1:9) %>% as.data.frame()

## catchAtAoLAggregation(distribution_alloc,
##                       catch_alloc,
##                       grouping = c("catchCategory", "allocGroup", "sampledOrEstimated"),
##                       maxAoL = 6,
##                       plusGroup = FALSE,
##                       round = 0) %>%
##     filter(catchCategory %in% "Dis",
##            sampledOrEstimated %in% "estimated") %>% as.data.frame()

## ## CANUM data:
## Canum_ICTAF <- read_csv(file.path("../2023_pok.27.3a46_RDBES_combined/output",
##                                   "canum_table.csv"))

## Canum_New <- catchAtAoLAggregation(distribution_alloc,
##                                    catch_alloc,
##                                    grouping = c("catchCategory"),
##                                    maxAoL = 10,
##                                    plusGroup = TRUE,
##                                    round = NULL) %>%
##     bind_rows(catchAtAoLAggregation(distribution_alloc,
##                                     catch_alloc,
##                                     grouping = c("attributeType", "attributeValue"),
##                                     maxAoL = 10,
##                                     plusGroup = TRUE,
##                                     round = NULL) %>%
##               mutate(catchCategory = "CAT")) %>%
##     mutate(Catch.Cat. = sub("^(.).*$", "\\1", catchCategory)) %>%
##     rename_with(~sub("N_A", "a", .x))

## ## WECA data:
## Weca_ICTAF <- read_csv(file.path("../2023_pok.27.3a46_RDBES_combined/output",
##                                   "weca_table.csv"))

## Weca_New <- meanWoLatAoLAggregation(distribution_alloc,
##                                     catch_alloc,
##                                     grouping = c("catchCategory",
##                                                  "attributeType",
##                                                  "attributeValue"),
##                                     maxAoL = 10,
##                                     plusGroup = TRUE,
##                                     round = NULL) %>%
##     bind_rows(meanWoLatAoLAggregation(distribution_alloc,
##                                       catch_alloc,
##                                       grouping = c("attributeType", "attributeValue"),
##                                       maxAoL = 10,
##                                       plusGroup = TRUE,
##                                       round = NULL) %>%
##               mutate(catchCategory = "CAT")) %>%
##     mutate(Catch.Cat. = sub("^(.).*$", "\\1", catchCategory)) %>%
##     rename_with(~sub("W_A", "a", .x))

## Weca_New

## library(scales)

## ## Deviations:
## Canum_ICTAF %>%
##     select(Catch.Cat.) %>%
##     left_join(Canum_New %>%
##               select(c(Catch.Cat., catchCategory))) %>%
##     bind_cols(Canum_ICTAF %>% select(Catch.Cat.) %>%
##               left_join(Canum_New %>%
##                         select(all_of(colnames(Canum_ICTAF)))) %>%
##               select(-Catch.Cat.) /
##               Canum_ICTAF %>% select(-Catch.Cat.)) %>%
##     mutate(across(where(is.numeric),
##                   ~percent(.x - 1, accuracy = 2))) %>%
##     as.data.frame()


## Weca_ICTAF %>%
##     select(Catch.Cat.) %>%
##     left_join(Weca_New %>%
##               select(c(Catch.Cat., catchCategory))) %>%
##     bind_cols(Weca_ICTAF %>% select(Catch.Cat.) %>%
##               left_join(Weca_New %>%
##                         select(all_of(colnames(Weca_ICTAF)))) %>%
##               select(-Catch.Cat.) /
##               Weca_ICTAF %>% select(-Catch.Cat.)) %>%
##     mutate(across(where(is.numeric),
##                   ~percent(.x - 1, accuracy = 2))) %>%
##     as.data.frame()


### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
