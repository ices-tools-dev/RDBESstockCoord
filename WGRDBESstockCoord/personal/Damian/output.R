## Output: the stock coordination files (SCF v3) - Catch Combined (CC) and Distributions Combined (DC)
##
## The catch the raising raised and the distributions the allocation estimated, added up into the two
## tables the stock coordination asks for. ?catchCombined and ?distributionsCombined say what every
## argument does. The last sections compare them with what the intercatch method (InterCatch) made of
## the same year: in total, and stratum by stratum before they are added up.

library(icesTAF)
library(dplyr)
library(tidyr)
library(rcef)

mkdir("output")

## The distributions come from the allocation (model_length_allocation.R writes them), the catch from the
## raising. A distribution and a catch row are linked by domainBiology; the strata the allocation estimated a
## distribution for have none in the raised catch, and it made one up for them ("[alloc]_<season>_<area>_
## <unit>_<fleet>_<category>"), which a catch row with no domain is given too - so every stratum, raised or not,
## finds its distribution. A catch weight with no distribution, or a distribution with fish and no catch weight,
## is warned about. It has to be the raised catch the allocation was run on - the file model_length_allocation.R
## reads - or the catch and the distributions are of two raisings.
catch_data_raised <- read.taf(file = "model/catch_data_raised.csv")
distributions_data_allocated <- read.taf(file = "model/distributions_data_allocated.csv")

## catchAllocationAoL returns a list of tables -> extraction of those:
distribution_alloc <- distributions_data_allocated
catch_alloc        <- catch_data_raised

distribution_alloc %>% group_by(variableType, catchCategory) %>%
  summarise(n = sum(value, na.rm = T))

## ###########################################################################
## The header of the files
## ###########################################################################

version <- 1
versionDescription <- "WGNSSK 2026 first run discard raising"   # a text with no commas: it is written to a csv
calendarYear <- "Y"     # "Y": the year is the calendar year. "N": a model year that starts on another date,
yearStartDate <- NA     #      given as "MM-DD" ("07-01"), the same for the whole stock


## ###########################################################################
## Catch Combined (CC)
## ###########################################################################

### The working group's catch of the stock, in tonnes, by catch category. The year, the working group
### and the stock are always reported by; `grouping` adds what else the stock assessment asks for.
CatchCombined <-
  catchCombined(catch_data = catch_data_raised,
                grouping = c("catchCategory"),
                originType_catch = "WGEstimate",
                variableType_catch = "WeightLive",
                unit = "t",
                round = 6,
                version = version,
                versionDescription = versionDescription,
                calendarYear = calendarYear,
                yearStartDate = yearStartDate)

CatchCombined

write.taf(CatchCombined, file = "SCF_CC.csv", dir = "output", na = "")

### Usually the catch is not split any further. When it is asked for - by season, area or fleet - it is
### added to `grouping` (a season and an area go with their type), and the same `grouping` goes to
### distributionsCombined(), since every distribution needs the catch it belongs to in the CC:
# catchCombined(catch_data = catch_data_raised,
#               grouping = c("catchCategory", "seasonValue"),
#               originType_catch = "WGEstimate", variableType_catch = "WeightLive", unit = "t")


## ###########################################################################
## Distributions Combined (DC)
## ###########################################################################
version <- 1
versionDescription <- "WGNSSK 2026 first run allocations"   # a text with no commas: it is written to a csv
### The numbers at length (thousands of fish, NE3) and the mean weight at length (g) of every length class
### (in mm) of every catch category: a `Number` row and a `WeightLive` row for each class. The mean weights
### are weighted by the numbers. A class no fish fall in is not reported.

#########CHECK THE NUMBERS AREN'T ADDING UP

DistributionsCombined <-
  distributionsCombined(distribution_data = distributions_data_allocated,
                        catch_data = catch_data_raised,
                        grouping = c( "attributeType", "attributeValue"),
                        weighting = "NumberAtAoL",
                        originType_catch = "WGEstimate",
                        variableType_catch = "WeightLive",
                        distributionType = "LengthTotal",
                        unit = c("NE3", "g"),
                        round = 6,
                        version = version,
                        versionDescription = versionDescription,
                        calendarYear = calendarYear,
                        yearStartDate = yearStartDate)

DistributionsCombined

write.taf(DistributionsCombined, file = "SCF_DC.csv", dir = "output", na = "")



## ###########################################################################
## Check before uploading
## ###########################################################################

### The SCF compares the catch of the CC with the sum of products of the DC - the numbers times the mean
### weights, over the classes - and rejects a total more than 20 % off. The same here, in tonnes:
SOP <-
  DistributionsCombined %>%
  select(catchCategory, distributionType, distributionClass, variableType, value) %>%
  pivot_wider(names_from = variableType, values_from = value) %>%
  group_by(catchCategory, distributionType) %>%
  summarise(SOP_t = sum(Number * 1000 * WeightLive, na.rm = TRUE) / 1e6, .groups = "drop") %>%     # NE3 x g -> t
  left_join(CatchCombined %>% filter(variableType == "WeightLive") %>% select(catchCategory, total),
            by = "catchCategory") %>%
  mutate(SOP_over_catch = SOP_t / total)

SOP

## ###########################################################################
## The same numbers with one column per class (for a look; not part of the SCF)
## ###########################################################################

### catchAtAoLAggregation() and meanWoLatAoLAggregation() add up exactly as distributionsCombined() does,
### and spread the classes over the columns: the numbers and the mean weights of the DC.
NumbersAtLength_wide <-
  catchAtAoLAggregation(distribution_data = distributions_data_allocated,
                        catch_data = catch_data_raised,
                        grouping = c("catchCategory", "year"),
                        originType_catch = "WGEstimate",
                        variableType_catch = "WeightLive",
                        distributionType = "LengthTotal",
                        unit = "NE3",
                        round = NULL)

MeanWeightAtLength_wide <-
  meanWoLatAoLAggregation(distribution_data = distributions_data_allocated,
                          catch_data = catch_data_raised,
                          grouping = c("catchCategory", "year"),
                          weighting = c("NumberAtAoL"),
                          originType_catch = "WGEstimate",
                          variableType_catch = "WeightLive",
                          distributionType = "LengthTotal",
                          unit = "g",
                          round = NULL,
                          minAoL = 0,
                          maxAoL = NA,
                          plusGroup = FALSE)


## ###########################################################################
## Compare with the intercatch method
## ###########################################################################
### The files of the intercatch method (InterCatch) for the same stock and year, all the catch categories
### together: caton, the catch in tonnes; canum, the numbers at length in fish; weca, the mean weight at
### length in grams. Seven lines of header and then one line of values. canum and weca have a value for every
### mm from the first length to the last - 0 where no class starts, or no fish are in it - and canum has a
### comma for the thousands.
read_intercatch <- function(file)
{
  x     <- readLines(file, warn = FALSE)
  range <- as.numeric(strsplit(trimws(x[5]), "\\s+")[[1]])         # the first and the last length, in mm
  list(unit   = trimws(x[7]),
       length = seq(range[1], range[2]),
       value  = as.numeric(gsub(",", "", strsplit(trimws(x[8]), "\\s+")[[1]])))
}

caton <- read_intercatch("boot/data/2025/caton.txt")
canum <- read_intercatch("boot/data/2025/canum.txt")
weca  <- read_intercatch("boot/data/2025/weca.txt")

stopifnot(caton$unit == "in tonnes", canum$unit == "in numbers", weca$unit == "in grams",
          length(canum$value) == length(canum$length), identical(canum$length, weca$length))

### Table 3 of the CatchAndSampleDataTables of InterCatch: the same catch, numbers and mean weights, before they
### are added up - a row for every stratum (country, catch category, season, area, fleet) and length. The
### numbers (CANUM) are in fish and the mean weights (WECA) in grams, as in canum and weca.
catch_and_samples <- readxl::read_excel("boot/data/2025/CatchAndSampleDataTables.xlsx", sheet = "Table 2",
                                        col_types = "text")



### caton, canum and weca against the CC and the DC above, the catch categories of those added up. Only the
### lengths with fish are compared (the 0 of canum and weca are left out). Every table has the value of the
### intercatch method, the rdbes one, their difference and their ratio (rdbes / intercatch: 1 is the same value).
stopifnot(all(CatchCombined$variableUnit == "t"), all(DistributionsCombined$variableUnit %in% c("NE3", "g")))

### The CEF has the lengths ten times those of InterCatch: 1300 to 7000 mm for a brill of 13 to 70 cm, 130 to 700
### in canum and weca. The IC files have them in mm already (300, 310, 320 ... with the unit "cm", the width of
### the class), and the conversion to the CEF multiplies the lengths in "cm" by 10. They are compared at the
### lengths of InterCatch: `length_scale` is 1 once the CEF has the lengths right.
length_scale <- 10

intercatch_length <-
  tibble(length = canum$length, canum = canum$value, weca = weca$value) %>%
  filter(canum > 0)

rdbes_length <-
  DistributionsCombined %>%
  filter(distributionType == "LengthTotal") %>%
  select(catchCategory, distributionClass, variableType, value) %>%
  pivot_wider(names_from = variableType, values_from = value) %>%
  group_by(length = distributionClass / length_scale) %>%
  summarise(canum = sum(Number) * 1000,                      # NE3 -> fish
            weca  = sum(Number * WeightLive) / sum(Number),  # g: the catch categories weighted by their numbers
            .groups = "drop")

rdbes_length_catch<-
  DistributionsCombined %>%
  filter(distributionType == "LengthTotal") %>%
  select(catchCategory, distributionClass, variableType, value) %>%
  pivot_wider(names_from = variableType, values_from = value) %>%
  group_by(catchCategory) %>%
  summarise(canum = sum(Number) * 1000,                      # NE3 -> fish
            weca  = sum(Number * WeightLive) / sum(Number),  # g: the catch categories weighted by their numbers
            .groups = "drop")



if (! setequal(intercatch_length$length, rdbes_length$length))
  warning("The lengths with fish are not the same in the two: see the tables below, and `length_scale`")

### The catch (caton against the CC), the numbers (canum) and the sum of products of the numbers and the mean
### weights (canum x weca), in total:
Compare_total <-
  tibble(quantity = c("catch (t)", "numbers (fish)", "sum of numbers x mean weights (t)"),
         intercatch = c(caton$value,
                      sum(intercatch_length$canum),
                      sum(intercatch_length$canum * intercatch_length$weca) / 1e6),
         rdbes      = c(sum(CatchCombined$total[CatchCombined$variableType == "WeightLive"]),
                      sum(rdbes_length$canum),
                      sum(rdbes_length$canum * rdbes_length$weca) / 1e6)) %>%
  mutate(difference = rdbes - intercatch,
         ratio      = rdbes / intercatch)

Compare_total

### The numbers at length (fish): canum against the Number of the DC
Compare_canum <-
  full_join(intercatch_length %>% select(length, intercatch = canum),
            rdbes_length      %>% select(length, rdbes = canum),
            by = "length") %>%
  mutate(across(c(intercatch, rdbes), \(x) replace_na(x, 0)),     # a length with no fish on one side
         difference = rdbes - intercatch,
         ratio      = rdbes / intercatch) %>%
  arrange(length)

Compare_canum

Compare_canum %>% group_by()

### The mean weights at length (g): weca against the WeightLive of the DC
Compare_weca <-
  full_join(intercatch_length %>% select(length, intercatch = weca),
            rdbes_length      %>% select(length, rdbes = weca),
            by = "length") %>%
  mutate(difference = rdbes - intercatch,
         ratio      = rdbes / intercatch) %>%
  arrange(length)

Compare_weca


## ###########################################################################
## Compare with the intercatch method, before the aggregation
## ###########################################################################

### The same, stratum by stratum and length by length: table 3 of InterCatch against the catch and the distributions
### of the allocation that the CC and the DC add up. A stratum is a row of the catch (country, catch category,
### season, area, fleet), and its distribution the one with its domainBiology. At the lengths of InterCatch again
### (`length_scale`), and the working group's own estimate of the catch.
stratum <- c("vesselFlagCountry", "catchCategory", "seasonType", "seasonValue", "areaValue", "fleetValue")

intercatch_stratum <-
  catch_and_samples %>%
  transmute(vesselFlagCountry = Country,
            catchCategory = case_when(CatchCategory == "Landings"    ~ "Lan",
                                      CatchCategory == "Discards"    ~ "Dis",
                                      CatchCategory == "BMS landing" ~ "BMS"),
            seasonType = SeasonType,
            seasonValue = Season,
            areaValue = Area,
            fleetValue = Fleet,
            sampledOrEstimated = SampledOrEstimated,    # how InterCatch got the distribution of the stratum
            raisedOrImported = CATONRaisedOrImported,   # and its catch
            length = as.numeric(AgeOrLength),
            caton_intercatch = as.numeric(CATON),         # kg
            canum_intercatch = as.numeric(CANUM),         # fish
            weca_intercatch = as.numeric(WECA))           # g

### A catch row is linked to its distribution by its domainBiology, or - the strata the allocation estimated a
### distribution for have none in the raised catch - by the one the allocation made up for it: catch_domain(),
### as the aggregations above link them.
catch_wgestimate <- catch_data_raised %>%
  filter(originType == "WGEstimate", variableType == "WeightLive")
catch_wgestimate$domainBiology <- catch_domain(catch_wgestimate)

rdbes_stratum <-
  distributions_data_allocated %>%
  filter(distributionType == "LengthTotal") %>%
  select(vesselFlagCountry, year, workingGroup, stock, speciesCode, catchCategory, domainBiology,
         distributionClass, variableType, value) %>%
  pivot_wider(names_from = variableType, values_from = value) %>%            # Number and WeightLive
  inner_join(catch_wgestimate %>%
               select(vesselFlagCountry, year, workingGroup, stock, speciesCode, catchCategory, domainBiology,
                      seasonType, seasonValue, areaValue, fleetValue, importedOrRaised, total),
             by = c("vesselFlagCountry", "year", "workingGroup", "stock", "speciesCode", "catchCategory",
                    "domainBiology")) %>%
  transmute(vesselFlagCountry, catchCategory, seasonType,
            seasonValue = as.character(seasonValue),
            areaValue, fleetValue, importedOrRaised,
            length = distributionClass / length_scale,
            caton_rdbes = total,                          # kg
            canum_rdbes = Number,                         # fish
            weca_rdbes = WeightLive)                      # g

### Every stratum and length of the two, side by side; what only one of them has is NA in the other:
Compare_strata_length <-
  full_join(intercatch_stratum, rdbes_stratum, by = c(stratum, "length")) %>%
  mutate(canum_difference = canum_rdbes - canum_intercatch,
         canum_ratio      = canum_rdbes / canum_intercatch,
         weca_difference  = weca_rdbes - weca_intercatch,
         weca_ratio       = weca_rdbes / weca_intercatch) %>%
  arrange(vesselFlagCountry, catchCategory, seasonValue, areaValue, fleetValue, length)

### Added up over the lengths, a row for each stratum: the catch (kg), the numbers (fish) and the sum of the
### numbers x the mean weights (kg), of both. The SCF compares that sum with the catch (sop_over_caton).
Compare_strata <-
  Compare_strata_length %>%
  group_by(across(all_of(stratum))) %>%
  summarise(sampledOrEstimated = first(na.omit(sampledOrEstimated)),
            raisedOrImported   = first(na.omit(raisedOrImported)),
            importedOrRaised   = first(na.omit(importedOrRaised)),
            caton_intercatch     = first(na.omit(caton_intercatch)),
            caton_rdbes          = first(na.omit(caton_rdbes)),
            numbers_intercatch   = sum(canum_intercatch, na.rm = TRUE),
            numbers_rdbes        = sum(canum_rdbes, na.rm = TRUE),
            sop_intercatch       = sum(canum_intercatch * weca_intercatch, na.rm = TRUE) / 1000,
            sop_rdbes            = sum(canum_rdbes * weca_rdbes, na.rm = TRUE) / 1000,
            .groups = "drop") %>%
  mutate(caton_difference         = caton_rdbes - caton_intercatch,
         numbers_difference       = numbers_rdbes - numbers_intercatch,
         numbers_ratio            = numbers_rdbes / numbers_intercatch,
         sop_over_caton_intercatch  = sop_intercatch / caton_intercatch,
         sop_over_caton_rdbes       = sop_rdbes / caton_rdbes)

### By how InterCatch got the distribution and the catch of the strata (NA: strata only the rdbes one has):
### the strata, how many have numbers more than one fish apart, the catch and the numbers of both.
Compare_summary <-
  Compare_strata %>%
  group_by(sampledOrEstimated, raisedOrImported, catchCategory) %>%
  summarise(strata           = n(),
            strata_differ    = sum(abs(numbers_difference) > 1),
            caton_intercatch   = sum(caton_intercatch, na.rm = TRUE),
            caton_rdbes        = sum(caton_rdbes, na.rm = TRUE),
            numbers_intercatch = sum(numbers_intercatch),
            numbers_rdbes      = sum(numbers_rdbes),
            .groups = "drop") %>%
  mutate(numbers_ratio = numbers_rdbes / numbers_intercatch)

Compare_summary
