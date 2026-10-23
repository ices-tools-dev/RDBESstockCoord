## Output: the stock coordination files (SCF v3) - Catch Combined (CC) and Distributions Combined (DC)
##
## The catch the raising raised and the distributions the allocation estimated, added up into the two
## tables the stock coordination asks for. ?catchCombined and ?distributionsCombined say what every
## argument does. The last sections compare them with what the intercatch method (InterCatch) made of
## the same year: in total, and stratum by stratum before they are added up. Saved in output/4_CEF_flow_vs_intercatch_final/, the figures in output/figures/4_CEF_flow_vs_intercatch_final/.

library(icesTAF)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rcef)
library(here)
library(icesRDBES)

## The project folder, wherever the repo (RDBESstockCoord) is cloned: every path below is relative to it
here::i_am("WGRDBESstockCoord/personal/Lennert/2025/output.R")
setwd(here("WGRDBESstockCoord", "personal", "Lennert", "2025"))

mkdir("output")

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
versionDescription <- "RDBESstockCoord test"   # a text with no commas: it is written to a csv
calendarYear <- "Y"     # "Y": the year is the calendar year. "N": a model year that starts on another date,
yearStartDate <- NA     #      given as "MM-DD" ("07-01"), the same for the whole stock
catchAllocationSchemeName <- "Manual groups"

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
                workingGroupYear = 2026,
                calendarYear = calendarYear)#,
                #yearStartDate = yearStartDate)

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
versionDescription <- "RDBESstockCoord test"   # a text with no commas: it is written to a csv
### The numbers at age (thousands of fish, NE3) and the mean weight at age (g) of every age class
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
                        distributionType = "Age",
                        unit = c("NE3", "g"),
                        round = 6,
                        version = version,
                        versionDescription = versionDescription,
                        workingGroupYear = 2026,
                        calendarYear = calendarYear)#,
                        #yearStartDate = yearStartDate)

DistributionsCombined

write.taf(DistributionsCombined, file = "SCF_DC.csv", dir = "output", na = "")

## ###########################################################################
## Catch Allocation Results (CAR) and Settings (CAS)
## ###########################################################################
### How the discards were raised, stratum by stratum, in the allocation setup of the format. The CAR has every stratum of the raised
### catch - the landings, the discards that were reported and the discards the raising made - with the group each discard was raised
### for (Target) and the groups each donor stratum feeds (Source); the CAS has the donor strata behind each group's discard ratio:
### the ratio of each (the discards over the landings) and the landings that weight it. The header is the one of the CC and the DC.
### The groups are the ones the discards were raised with: model_discard_raising.R saves them next to the raised catch.
stopifnot("model_discard_raising.R saves the groups the CAR and the CAS need: run it again" = file.exists("model/discard_raising_groups.rds"))
groups <- readRDS("model/discard_raising_groups.rds")

CatchAllocation <-
  catchAllocation(catch_data = catch_data_raised,
                  condition_raising_st_list = groups$strataCond,
                  condition_matched_data_list = groups$matchedDataCond,
                  originType = "WGEstimate",
                  variableType = "WeightLive",
                  catchAllocationSchemeName = catchAllocationSchemeName,
                  round = 3,
                  workingGroupYear = 2026,
                  version = version,
                  versionDescription = versionDescription,
                  calendarYear = calendarYear,
                  startDateYear = NA)

CatchAllocationResults <- CatchAllocation$CAR
CatchAllocationSettings <- CatchAllocation$CAS

CatchAllocationResults
CatchAllocationSettings

write.taf(CatchAllocationResults, file = "SCF_CAR.csv", dir = "output", na = "")
write.taf(CatchAllocationSettings, file = "SCF_CAS.csv", dir = "output", na = "")

### The CAR adds up to the CC (the CAR is in kg, the CC in tonnes), and the donors of a group give the ratio its discards were raised with:
CatchAllocationResults %>%
  group_by(catchCategory) %>%
  summarise(CAR_t = sum(total) / 1000) %>%
  left_join(CatchCombined %>% filter(variableType == "WeightLive") %>% select(catchCategory, CC_t = total), by = "catchCategory")

CatchAllocationSettings %>%
  group_by(catchAllocationGroupName) %>%
  summarise(donors = n(), landings_kg = sum(weightingTotal), ratio = sum(rate * weightingTotal) / sum(weightingTotal))

## CC and DC one after the other in one file, no header
write.table(CatchCombined, "output/HCI.csv", sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE, na = "")
write.table(DistributionsCombined, "output/HCI.csv", sep = ",", row.names = FALSE, col.names = FALSE, quote = FALSE, na = "", append = TRUE)




try(rdbes_upload_data(
  file_path = "output/HCI.csv",
  hierarchy = "HCI"))



## ###########################################################################
## Check before uploading
## ###########################################################################

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
## The same numbers with one column per class (to be used as model input.
## ###########################################################################

### catchAtAoLAggregation() and meanWoLatAoLAggregation() add up exactly as distributionsCombined() does,
### and spread the classes over the columns: the numbers and the mean weights of the DC.
NumbersAtAge_wide <-
  catchAtAoLAggregation(distribution_data = distributions_data_allocated,
                        catch_data = catch_data_raised,
                        grouping = c("catchCategory", "year"),
                        originType_catch = "WGEstimate",
                        variableType_catch = "WeightLive",
                        distributionType = "Age",
                        unit = "NE3",
                        round = NULL,
                        maxAoL = 8,
                        plusGroup = T)

MeanWeightAtAge_wide <-
  meanWoLatAoLAggregation(distribution_data = distributions_data_allocated,
                          catch_data = catch_data_raised,
                          grouping = c("catchCategory", "year"),
                          weighting = c("NumberAtAoL"),
                          originType_catch = "WGEstimate",
                          variableType_catch = "WeightLive",
                          distributionType = "Age",
                          unit = "g",
                          round = NULL,
                          minAoL = 0,
                          maxAoL = 8,
                          plusGroup = T)

## ###########################################################################
## Make CAR
## ###########################################################################

rcef::scf_table(d = , record = "CAR", header = ,fn = )

## ###########################################################################
## The CEF flow against the intercatch output
## ###########################################################################
### The files of the intercatch method (InterCatch) for the same stock and year, all the catch categories
### together: caton, the catch in tonnes; canum, the numbers at age in fish; weca, the mean weight at
### age in grams. Seven lines of header and then one line of values. canum and weca have a value for every
### mm from the first age to the last - 0 where no class starts, or no fish are in it - and canum has a
### comma for the thousands.
read_intercatch <- function(file)
{
  x     <- readLines(file, warn = FALSE)
  range <- as.numeric(strsplit(trimws(x[5]), "\\s+")[[1]])         # the first and the last age
  list(unit   = trimws(x[7]),
       age = seq(range[1], range[2]),
       value  = as.numeric(gsub(",", "", strsplit(trimws(x[8]), "\\s+")[[1]])))
}

caton <- read_intercatch("boot/data/2025/caton.txt") # total catch
canum <- read_intercatch("boot/data/2025/lanum.txt")
weca  <- read_intercatch("boot/data/2025/wela.txt")

stopifnot(caton$unit == "in tonnes", canum$unit == "in numbers", weca$unit == "in grams",
          length(canum$value) == length(canum$age), identical(canum$age, weca$age))

### Table 3 of the CatchAndSampleDataTables of InterCatch: the same catch, numbers and mean weights, before they
### are added up - a row for every stratum (country, catch category, season, area, fleet) and length. The
### numbers (CANUM) are in fish and the mean weights (WECA) in grams, as in canum and weca.
catch_and_samples <- readxl::read_excel("boot/data/2025/CatchAndSampleDataTables.xlsx", sheet = "Table2",
                                        col_types = "text")


### caton, canum and weca against the CC and the DC above, the catch categories of those added up. Only the
### ages with fish are compared (the 0 of canum and weca are left out). Every table has the value of the
### intercatch method, the CEF flow one, their difference and their ratio (cef_flow / intercatch: 1 is the same value).
stopifnot(all(CatchCombined$variableUnit == "t"), all(DistributionsCombined$variableUnit %in% c("NE3", "g")))

### The CEF has the lengths ten times those of InterCatch: 1300 to 7000 mm for a brill of 13 to 70 cm, 130 to 700
### in canum and weca. The IC files have them in mm already (300, 310, 320 ... with the unit "cm", the width of
### the class), and the conversion to the CEF multiplies the lengths in "cm" by 10. They are compared at the
### lengths of InterCatch: `length_scale` is 1 once the CEF has the lengths right.
#length_scale <- 1

intercatch_age <-
  tibble(age = canum$age, canum = canum$value, weca = weca$value) %>%
  filter(canum > 0)

cef_flow_age <-
  DistributionsCombined %>%
  filter(distributionType == "Age") %>%
  select(catchCategory, distributionClass, variableType, value) %>%
  ### LVDP: For tur.27.4, only landings
  filter(catchCategory == "Lan") %>%
  pivot_wider(names_from = variableType, values_from = value) %>%
  ### LVDP: Plus group
  mutate(distributionClass = ifelse(distributionClass > 8, 8, distributionClass)) %>%
  group_by(age = distributionClass ) %>% # was / length_scale
  summarise(canum = sum(Number) * 1000,                      # NE3 -> fish
            weca  = sum(Number * WeightLive) / sum(Number),  # g: the catch categories weighted by their numbers
            .groups = "drop")

cef_flow_age_catch<-
  DistributionsCombined %>%
  filter(distributionType == "Age") %>%
  select(catchCategory, distributionClass, variableType, value) %>%
  pivot_wider(names_from = variableType, values_from = value) %>%
  group_by(catchCategory) %>%
  summarise(canum = sum(Number) * 1000,                      # NE3 -> fish
            weca  = sum(Number * WeightLive) / sum(Number),  # g: the catch categories weighted by their numbers
            .groups = "drop")



if (! setequal(intercatch_age$age, cef_flow_age$age))
  warning("The ages with fish are not the same in the two: see the tables below, and `length_scale`")

### The catch (caton against the CC), the numbers (canum) and the sum of products of the numbers and the mean
### weights (canum x weca), in total:
Compare_total <-
  tibble(quantity = c("catch (t)", "numbers (fish)", "sum of numbers x mean weights (t)"),
         intercatch = c(caton$value,
                      sum(intercatch_age$canum),
                      sum(intercatch_age$canum * intercatch_age$weca) / 1e6),
         cef_flow   = c(sum(CatchCombined$total[CatchCombined$variableType == "WeightLive"]),
                      sum(cef_flow_age$canum),
                      sum(cef_flow_age$canum * cef_flow_age$weca) / 1e6)) %>%
  mutate(difference = cef_flow - intercatch,
         ratio      = cef_flow / intercatch)

Compare_total

### The numbers at agefish): canum against the Number of the DC
Compare_canum <-
  full_join(intercatch_age %>% select(age, intercatch = canum),
            cef_flow_age   %>% select(age, cef_flow = canum),
            by = "age") %>%
  mutate(across(c(intercatch, cef_flow), \(x) replace_na(x, 0)),     # a age with no fish on one side
         difference = cef_flow - intercatch,
         ratio      = cef_flow / intercatch) %>%
  arrange(age)

Compare_canum

Compare_canum %>% group_by()

### The mean weights at age (g): weca against the WeightLive of the DC
Compare_weca <-
  full_join(intercatch_age %>% select(age, intercatch = weca),
            cef_flow_age   %>% select(age, cef_flow = weca),
            by = "age") %>%
  mutate(difference = cef_flow - intercatch,
         ratio      = cef_flow / intercatch) %>%
  arrange(age)

Compare_weca


## ###########################################################################
## The CEF flow against the intercatch output, before the aggregation
## ###########################################################################

### The same, stratum by stratum and age by age: table 3 of InterCatch against the catch and the distributions
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
            age = as.numeric(AgeOrLength),
            caton_intercatch = as.numeric(CATON),         # kg
            canum_intercatch = as.numeric(CANUM),         # fish
            weca_intercatch = as.numeric(WECA))           # g

### A catch row is linked to its distribution by its domainBiology, or - the strata the allocation estimated a
### distribution for have none in the raised catch - by the one the allocation made up for it: catch_domain(),
### as the aggregations above link them.
catch_wgestimate <- catch_data_raised %>%
  filter(originType == "WGEstimate", variableType == "WeightLive")
catch_wgestimate$domainBiology <- catch_domain(catch_wgestimate)

cef_flow_stratum <-
  distributions_data_allocated %>%
  filter(distributionType == "Age") %>%
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
            age = distributionClass, # Was /length_scale
            caton_cef_flow = total,                          # kg
            canum_cef_flow = Number,                         # fish
            weca_cef_flow = WeightLive)                      # g

### Every stratum and age of the two, side by side; what only one of them has is NA in the other:
Compare_strata_age <-
  full_join(intercatch_stratum, cef_flow_stratum, by = c(stratum, "age")) %>%
  mutate(canum_difference = canum_cef_flow - canum_intercatch,
         canum_ratio      = canum_cef_flow / canum_intercatch,
         weca_difference  = weca_cef_flow - weca_intercatch,
         weca_ratio       = weca_cef_flow / weca_intercatch) %>%
  arrange(vesselFlagCountry, catchCategory, seasonValue, areaValue, fleetValue, age)

### Added up over the ages, a row for each stratum: the catch (kg), the numbers (fish) and the sum of the
### numbers x the mean weights (kg), of both. The SCF compares that sum with the catch (sop_over_caton).
Compare_strata <-
  Compare_strata_age %>%
  group_by(across(all_of(stratum))) %>%
  summarise(sampledOrEstimated = first(na.omit(sampledOrEstimated)),
            raisedOrImported   = first(na.omit(raisedOrImported)),
            importedOrRaised   = first(na.omit(importedOrRaised)),
            caton_intercatch     = first(na.omit(caton_intercatch)),
            caton_cef_flow       = first(na.omit(caton_cef_flow)),
            numbers_intercatch   = sum(canum_intercatch, na.rm = TRUE),
            numbers_cef_flow     = sum(canum_cef_flow, na.rm = TRUE),
            sop_intercatch       = sum(canum_intercatch * weca_intercatch, na.rm = TRUE) / 1000,
            sop_cef_flow         = sum(canum_cef_flow * weca_cef_flow, na.rm = TRUE) / 1000,
            .groups = "drop") %>%
  mutate(caton_difference         = caton_cef_flow - caton_intercatch,
         numbers_difference       = numbers_cef_flow - numbers_intercatch,
         numbers_ratio            = numbers_cef_flow / numbers_intercatch,
         sop_over_caton_intercatch  = sop_intercatch / caton_intercatch,
         sop_over_caton_cef_flow    = sop_cef_flow / caton_cef_flow)

### By how InterCatch got the distribution and the catch of the strata (NA: strata only the CEF flow has):
### the strata, how many have numbers more than one fish apart, the catch and the numbers of both.
Compare_summary <-
  Compare_strata %>%
  group_by(sampledOrEstimated, raisedOrImported, catchCategory) %>%
  summarise(strata           = n(),
            strata_differ    = sum(abs(numbers_difference) > 1),
            caton_intercatch   = sum(caton_intercatch, na.rm = TRUE),
            caton_cef_flow     = sum(caton_cef_flow, na.rm = TRUE),
            numbers_intercatch = sum(numbers_intercatch),
            numbers_cef_flow   = sum(numbers_cef_flow),
            .groups = "drop") %>%
  mutate(numbers_ratio = numbers_cef_flow / numbers_intercatch)

Compare_summary

## ###########################################################################
## The comparison, saved
## ###########################################################################
### The tables of the two sections above in a folder named for what they compare: each as a csv in tables/, and all of
### them in one Excel workbook (a sheet each). The other comparison - the CEF converted from InterCatch against the CEF
### downloaded from the RDBES - is in data.R: output/1_IC_conversion_vs_CEF_download/.
compare_dir <- file.path("output", "4_CEF_flow_vs_intercatch_final")
unlink(c(compare_dir, file.path("output", c("CEF_flow_vs_intercatch_final", "CEF-flow_vs_IC-output"))), recursive = TRUE)   # nothing stale next to the new files, nor under the names this had before
mkdir(file.path(compare_dir, "tables"))

compare_tables <- list(total = Compare_total, numbers_at_age = Compare_canum, mean_weight_at_age = Compare_weca,
                       strata_by_age = Compare_strata_age, strata = Compare_strata, summary = Compare_summary)

for (nm in names(compare_tables))
  tryCatch(write.taf(compare_tables[[nm]], file = paste0(nm, ".csv"), dir = file.path(compare_dir, "tables"), quote = TRUE),
           error = function(e) warning("could not write ", nm, ": ", conditionMessage(e), " (is it open in Excel?)", call. = FALSE))
if (requireNamespace("openxlsx", quietly = TRUE)) {
  wb <- openxlsx::createWorkbook()
  for (nm in names(compare_tables)) {
    openxlsx::addWorksheet(wb, nm)
    openxlsx::writeData(wb, nm, as.data.frame(compare_tables[[nm]]), withFilter = TRUE)
    openxlsx::freezePane(wb, nm, firstRow = TRUE)
    openxlsx::setColWidths(wb, nm, cols = seq_along(compare_tables[[nm]]), widths = "auto")
  }
  tryCatch(openxlsx::saveWorkbook(wb, file.path(compare_dir, "CEF_flow_vs_intercatch_final.xlsx"), overwrite = TRUE),
           error = function(e) warning("could not write the workbook: ", conditionMessage(e), " (is it open in Excel?)", call. = FALSE))
} else message("install the package openxlsx to get the tables as one Excel workbook")

## ###########################################################################
## The figures of the comparison
## ###########################################################################
### The comparison in figures, in output/figures/4_CEF_flow_vs_intercatch_final (the folders of output/figures are numbered
### in the order the scripts make them). Two sides in every figure: IC final, the files of the intercatch method (caton,
### canum, weca and its table 3), and the CEF flow, the raising and the allocation as the CC and the DC add them up.
### The tables of the figures are in output/4_CEF_flow_vs_intercatch_final/tables.
fig_dir <- file.path("output", "figures", "4_CEF_flow_vs_intercatch_final")
unlink(fig_dir, recursive = TRUE)                           # nothing stale next to the new files
mkdir(fig_dir)

## IC keeps the colour it has in the comparison of data.R (the IC conversion against the CEF download), the CEF flow has one of
## its own - not the orange of the RDBES download there
invisible(plot_colours(c("IC", "RDBES"), "source"))
src_col <- plot_colours(c("IC", "RDBES", "CEF flow"), "source")[c("IC", "CEF flow")]
src_lab <- c(IC = "IC final", `CEF flow` = "CEF flow")
category_name <- c(Lan = "Landings", Dis = "Discards", BMS = "BMS landings")
cmp_caption <- paste(strwrap(paste0(
  "IC final: the files of the intercatch method (caton, canum, weca) and its table 3 for the strata. CEF flow: the raising and ",
  "the allocation, added up as the CC and the DC"), 120), collapse = "\n")
save_cmp <- function(p, name, width = 9, height = 5.5)
  ggsave(file.path(fig_dir, paste0(name, ".png")), p, width = width, height = height, dpi = 150)

### 1. The totals: the catch, the numbers, and the sum of products of the numbers and the mean weights
cmp_total <- Compare_total %>%
  mutate(scale = c(1, 1000, 1),
         panel = factor(sprintf("%s\nCEF flow / IC final = %.4f",
                                c("Catch (t)", "Numbers (thousand fish)", "Sum of numbers x mean weights (t)"), ratio))) %>%
  pivot_longer(c(intercatch, cef_flow), names_to = "source", values_to = "value") %>%
  mutate(value = value / scale, source = factor(recode(source, intercatch = "IC", cef_flow = "CEF flow"), c("IC", "CEF flow")))
p <- ggplot(cmp_total, aes(source, value, fill = source)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = format(round(value, 1), big.mark = ",")), vjust = -0.5, size = 3.4) +
  facet_wrap(vars(panel), scales = "free_y") +
  scale_fill_manual(values = src_col) + scale_x_discrete(labels = src_lab) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12)), labels = scales::label_comma()) +
  labs(title = "The totals: the CEF flow against the intercatch final output", x = NULL, y = NULL, caption = cmp_caption) +
  app_theme() + theme(legend.position = "none")
save_cmp(p, "01_totals", width = 10, height = 4.8)

### 2. and 3. The numbers and the mean weights at age (IC final wide and light, the CEF flow on it), and their ratio
at_age <- function(tab, scale, what, unit) {
  top <- paste0(what, " (", unit, ")")
  d <- bind_rows(tibble(age = tab$age, panel = top, source = "IC", y = tab$intercatch / scale),
                 tibble(age = tab$age, panel = top, source = "CEF flow", y = tab$cef_flow / scale),
                 tibble(age = tab$age, panel = "Ratio CEF flow / IC final", source = "CEF flow", y = tab$ratio))
  d$panel  <- factor(d$panel, c(top, "Ratio CEF flow / IC final"))
  d$source <- factor(d$source, c("IC", "CEF flow"))
  ggplot(d, aes(age, y, colour = source)) +
    geom_line(data = filter(d, source == "IC", panel == top), linewidth = 2.4, alpha = 0.5) +
    geom_line(data = filter(d, source == "CEF flow"), linewidth = 0.5) +
    geom_point(data = filter(d, source == "CEF flow"), size = 1.3) +
    geom_hline(data = tibble(panel = factor("Ratio CEF flow / IC final", levels(d$panel)), y = 1), aes(yintercept = y),
               linetype = "dashed", colour = "grey35") +
    facet_wrap(vars(panel), ncol = 1, scales = "free_y") +
    scale_colour_manual(values = src_col, labels = src_lab, name = NULL) +
    scale_y_continuous(labels = scales::label_comma(), expand = expansion(mult = 0.08)) +
    labs(x = "Age", y = NULL, caption = cmp_caption) +
    app_theme() + theme(legend.position = "top")
}
save_cmp(at_age(Compare_canum, 1000, "Numbers at age", "thousand fish") + labs(title = "The numbers at age"),
         "02_numbers_at_age", height = 6.2)
save_cmp(at_age(Compare_weca, 1, "Mean weight at age", "g") + labs(title = "The mean weight at age"),
         "03_mean_weight_at_age", height = 6.2)

### 4. The numbers at age of each catch category: the landings and the discards have a age distribution of their own
cmp_category <- Compare_strata_age %>%
  group_by(catchCategory, age) %>%
  summarise(IC = sum(canum_intercatch, na.rm = TRUE) / 1000, `CEF flow` = sum(canum_cef_flow, na.rm = TRUE) / 1000, .groups = "drop") %>%
  group_by(catchCategory) %>% filter(sum(IC) + sum(`CEF flow`) > 0) %>% ungroup() %>%
  pivot_longer(c(IC, `CEF flow`), names_to = "source", values_to = "fish") %>%
  mutate(source = factor(source, c("IC", "CEF flow")), catchCategory = factor(category_name[catchCategory], category_name))
p <- ggplot(cmp_category, aes(age, fish, colour = source)) +
  geom_line(data = filter(cmp_category, source == "IC"), linewidth = 2.4, alpha = 0.5) +
  geom_line(data = filter(cmp_category, source == "CEF flow"), linewidth = 0.5) +
  geom_point(data = filter(cmp_category, source == "CEF flow"), size = 1.3) +
  facet_wrap(vars(catchCategory), ncol = 1, scales = "free_y") +
  scale_colour_manual(values = src_col, labels = src_lab, name = NULL) +
  scale_y_continuous(labels = scales::label_comma(), expand = expansion(mult = c(0.02, 0.08))) +
  labs(title = "The numbers at age of each catch category", x = "Age", y = "Numbers (thousand fish)", caption = cmp_caption) +
  app_theme() + theme(legend.position = "top")
save_cmp(p, "04_numbers_at_age_by_category", height = 6.5)

### 5. Stratum by stratum, by the size of the stratum. A stratum is a row of the catch (country, catch category, season, area,
### fleet); the colour is how InterCatch got its distribution and its catch.
how_ic <- c("Sampled_Distribution Imported_Data" = "sampled, imported", "Estimated_Distribution Imported_Data" = "estimated, imported",
            "Estimated_Distribution Raised_Discards" = "estimated, raised")
how_col <- plot_colours(unname(how_ic), "how InterCatch got the stratum", palette = "colorblind")
cmp_strata <- Compare_strata %>%
  mutate(label = paste(vesselFlagCountry, catchCategory, seasonValue, areaValue, fleetValue),
         how = factor(how_ic[paste(sampledOrEstimated, raisedOrImported)], how_ic))
by_size <- bind_rows(
  cmp_strata %>% filter(caton_intercatch > 0, caton_cef_flow > 0) %>%
    transmute(how, panel = "Catch: CEF flow minus IC final (kg), by the catch of the stratum in IC final (kg)",
              size = caton_intercatch, y = caton_difference),
  cmp_strata %>% filter(numbers_intercatch > 0, numbers_cef_flow > 0) %>%
    transmute(how, panel = "Numbers: CEF flow against IC final (%), by the numbers of the stratum in IC final (fish)",
              size = numbers_intercatch, y = 100 * (numbers_cef_flow / numbers_intercatch - 1))) %>%
  filter(!is.na(how)) %>% mutate(panel = factor(panel, unique(panel)))
p <- ggplot(by_size, aes(size, y, colour = how)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey35") +
  geom_point(alpha = 0.7, size = 1.6) +
  facet_wrap(vars(panel), scales = "free", ncol = 1) +
  scale_x_log10(labels = scales::label_comma()) +
  scale_colour_manual(values = how_col, name = "How InterCatch got the stratum") +
  labs(title = "Stratum by stratum: the CEF flow against the intercatch final output",
       x = "Size of the stratum in IC final (log scale)", y = NULL, caption = cmp_caption) +
  app_theme() + theme(legend.position = "top")
save_cmp(p, "05_by_stratum", width = 9, height = 7.5)

### 6. The strata that differ most, in the catch and in the numbers
largest <- bind_rows(
  cmp_strata %>% slice_max(abs(caton_difference), n = 10, with_ties = FALSE) %>%
    transmute(panel = "Catch (kg)", stratum = label, diff = caton_difference, share = 100 * caton_difference / caton_intercatch),
  cmp_strata %>% slice_max(abs(numbers_difference), n = 10, with_ties = FALSE) %>%
    transmute(panel = "Numbers (fish)", stratum = label, diff = numbers_difference, share = 100 * numbers_difference / numbers_intercatch)) %>%
  mutate(panel = factor(panel, unique(panel)), key = paste(panel, stratum, sep = "||")) %>%
  group_by(panel) %>% arrange(abs(diff), .by_group = TRUE) %>% ungroup()
largest$key <- factor(largest$key, largest$key)
p <- ggplot(largest, aes(diff, key)) +
  geom_col(fill = src_col[["CEF flow"]], width = 0.7) +
  geom_text(aes(label = sprintf("%+.2f (%+.1f%%)", diff, share), hjust = ifelse(diff >= 0, -0.1, 1.1)), size = 3) +
  facet_wrap(vars(panel), scales = "free", ncol = 1) +
  scale_y_discrete(labels = function(k) sub("^.*\\|\\|", "", k)) +
  scale_x_continuous(expand = expansion(mult = 0.22)) +
  labs(title = "The strata that differ most: CEF flow minus IC final",
       x = "CEF flow minus IC final (its share of the IC final value in brackets)", y = NULL, caption = cmp_caption) +
  app_theme()
save_cmp(p, "06_strata_that_differ_most", width = 10, height = 8)

### 7. By how InterCatch got the distribution and the catch of the strata: sampled or estimated, imported or raised
rank_of <- function(x, first) ifelse(x == first, 1L, 2L)
how_got <- Compare_summary %>%
  filter(caton_intercatch > 0 | numbers_intercatch > 0) %>%
  mutate(group = sprintf("%s: %s, %s\n%d strata, %d differ", category_name[catchCategory],
                         ifelse(sampledOrEstimated == "Sampled_Distribution", "sampled", "estimated"),
                         ifelse(raisedOrImported == "Raised_Discards", "raised", "imported"), strata, strata_differ),
         order = match(catchCategory, names(category_name)) * 100 + rank_of(sampledOrEstimated, "Sampled_Distribution") * 10 +
                 rank_of(raisedOrImported, "Imported_Data"))
how_got <- how_got %>%
  pivot_longer(c(caton_intercatch, caton_cef_flow, numbers_intercatch, numbers_cef_flow),
               names_to = c("what", "source"), names_pattern = "(caton|numbers)_(.*)") %>%
  mutate(source = factor(ifelse(source == "intercatch", "IC", "CEF flow"), c("IC", "CEF flow")),
         panel = factor(ifelse(what == "caton", "Catch (t)", "Numbers (thousand fish)"), c("Catch (t)", "Numbers (thousand fish)")),
         value = value / 1000) %>%
  group_by(group, panel) %>% mutate(share = 100 * (value[source == "CEF flow"] / value[source == "IC"] - 1)) %>% ungroup()
how_got$group <- factor(how_got$group, rev(unique(how_got$group[order(how_got$order)])))        # the first at the top
p <- ggplot(how_got, aes(value, group, fill = source)) +
  geom_col(position = position_dodge(width = 0.8, reverse = TRUE), width = 0.75) +
  geom_text(aes(label = ifelse(source == "CEF flow", sprintf("%s  (%+.2f%%)", format(round(value, 1), big.mark = ","), share), "")),
            position = position_dodge(width = 0.8, reverse = TRUE), hjust = -0.08, size = 3) +
  facet_wrap(vars(panel), scales = "free_x") +
  scale_fill_manual(values = src_col, labels = src_lab, name = NULL) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.35)), labels = scales::label_comma()) +
  labs(title = "By how InterCatch got the distribution and the catch of the strata", x = NULL, y = NULL, caption = cmp_caption) +
  app_theme() + theme(legend.position = "top")
save_cmp(p, "07_how_intercatch_got_it", width = 11, height = 5.5)

### 8. The check of the SCF, stratum by stratum: the sum of products of the numbers and the mean weights over the catch
### (the SCF rejects a total more than 20 % off)
sop_by_stratum <- bind_rows(
  cmp_strata %>% filter(caton_intercatch > 0) %>% transmute(catchCategory, source = "IC", ratio = sop_over_caton_intercatch),
  cmp_strata %>% filter(caton_cef_flow > 0) %>% transmute(catchCategory, source = "CEF flow", ratio = sop_over_caton_cef_flow)) %>%
  filter(catchCategory %in% c("Lan", "Dis")) %>%
  mutate(source = factor(source, c("IC", "CEF flow")), catchCategory = factor(category_name[catchCategory], category_name[c("Lan", "Dis")]),
         outside = ratio < 0.8 | ratio > 1.2)
n_outside <- sop_by_stratum %>% group_by(catchCategory, source) %>%
  summarise(label = sprintf("%d of %d strata\noutside the 20%% band", sum(outside), n()), .groups = "drop")
p <- ggplot(sop_by_stratum, aes(source, ratio, colour = source)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.8, ymax = 1.2, fill = "grey85", alpha = 0.5) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey35") +
  geom_jitter(width = 0.25, height = 0, alpha = 0.6, size = 1.4) +
  geom_text(data = n_outside, aes(y = 1.28, label = label), colour = "grey20", size = 3, lineheight = 0.9, vjust = 0) +
  facet_wrap(vars(catchCategory)) +
  scale_colour_manual(values = src_col) + scale_x_discrete(labels = src_lab) +
  scale_y_continuous(expand = expansion(mult = c(0.03, 0.12))) +
  labs(title = "The check of the SCF, stratum by stratum", x = NULL, y = "Sum of numbers x mean weights / catch",
       caption = paste0(cmp_caption, "\nThe grey band is the 20 % the SCF accepts; a stratum with catch and no numbers is at 0.")) +
  app_theme() + theme(legend.position = "none")
save_cmp(p, "08_sop_over_catch", width = 9, height = 5.5)

message(length(list.files(fig_dir)), " figures in ", fig_dir)
