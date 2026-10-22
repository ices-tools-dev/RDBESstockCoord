## Preprocess data, write TAF data tables
## Before:
## After:

library(icesTAF)
library(dplyr)
library(tidyr)
library(grid) # for unit()
library(RColorBrewer)
library(readr)
library(ggplot2)

library(icesRDBES)
library(remotes)
## To install (or update) rcef run the next line in the console, not with the script: when it is done it asks to restart R, which a new version needs
# remotes::install_github("DVillagra26/rcef", dependencies = TRUE, force = TRUE); if (interactive() && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable() && isTRUE(utils::askYesNo("rcef is installed: restart R now?"))) rstudioapi::restartSession()
library(rcef)
library(here)
# taf.bootstrap()

############### USING INTERCATCH DATA #################
## The project folder, wherever the repo (RDBESstockCoord) is cloned: here() finds the repo from the place of this script in it
here::i_am("WGRDBESstockCoord/personal/Damian/2024/data.R")
base_path <- file.path(here(),"WGRDBESstockCoord/personal/Damian/2024")
setwd(base_path)   # the paths that are relative below (data/, output/) are then inside the project folder too

## The catch year of this folder: the InterCatch files are in boot/data/<catch_year> and it is the year of the RDBES download below.
## For another year: copy the folder, change this year and the folder in the here() lines of the scripts.
catch_year <- 2024
boot_path <- file.path(base_path, "boot/data")
data_path <- file.path(base_path, "data")
output_path <- file.path(base_path, "output")
report_path <- file.path(base_path, "report")

mkdir(data_path)
mkdir(report_path)

## To files:
funICoutCEF(dat_path = boot_path,
            years = catch_year,
            metier6 = "fleet",
            output_format = "to_file",
            out_path = data_path,
            keep_temp_file = TRUE,
            file_prefix = paste0("bll_", catch_year, "_"))
rm(list = intersect("stock_relation", ls()))     # funICoutCEF leaves its stock lookup in the session: nothing reads it


## An empty field of the CEF files is missing, so it is read as NA. read.taf() is read.csv(), which reads it as the text "":
## the raising and the allocation then take "" for a domain of its own that every record without one shares.
catch_data    <- read.taf(file.path(data_path, paste0("bll_", catch_year, "_catches_CEF_v17.1.csv")), na.strings = c("NA", ""))
distributions_data <- read.taf(file.path(data_path, paste0("bll_", catch_year, "_distributions_CEF_v17.1.csv")), na.strings = c("NA", ""))


catch_data <- derive_fields(catch_data)
distributions_data <- derive_fields(catch_data, distributions_data, what = "distribution")

write.taf(catch_data, file = "catch_data_CEF.csv", dir = "data")
write.taf(distributions_data, file = "distributions_data_CEF.csv", dir = "data")



############### USING CEF DOWLOAD DATA #################

my_payload_cef <- list(
  Hierarchies= list("HNI"),
  DataType= "CEF",
  Format= "CsvFilePerTable",
  CEFFilters= list(
    CNVesselFlagCountry= list("All"),
    CNstock= list("bll.27.3a47de"),
    CNyear=  list(as.character(catch_year)),
    SNstock= list("bll.27.3a47de"),
    SNyear=  list(as.character(catch_year)),
    ENyear=  list(as.character(catch_year)),
    ENStockForDeterminingAreas= list("bll.27.3a47de")))

zipfile <- rdbes_download_data(payload = my_payload_cef, dest_dir = data_path)


## ###########################################################################
## The same data? The InterCatch conversion against the RDBES download
## ###########################################################################
##
## The catch (CatchNatEst) and the distributions (DistributionNatEst) of the RDBES download against what the
## conversion of InterCatch made (funICoutCEF above: catch_data and distributions_data, "IC" below).
## The other comparison - the CEF flow against the output of InterCatch - is in output.R: output/4_CEF_flow_vs_intercatch_final/ (tables) and output/figures/4_CEF_flow_vs_intercatch_final/ (figures).
##
## WHAT TO OPEN
##   catch_compare       the catch, stratum by stratum (country, catch category, season, area, fleet): the total of IC and of
##                       the RDBES side by side, both in tonnes, with the origin of the data of each, the tonnes of the
##                       stratum that have a length or an age distribution, and the numbers of fish of those distributions
##   dist_compare        the distributions, stratum by stratum: fish, fish measured, classes, of IC and of the RDBES
##   dist_class_compare  the same class by class: fish and mean weight
##   rdbes_gap           catch_compare added up by country, area, season and metier4 (a list of tables: catch_*, length_*, age_*
##                       for the tonnes, NaL_* and NaA_* for the numbers)
## They are also written to output/1_IC_conversion_vs_CEF_download/, named for what it compares: the tables as csv in tables/
## and all of them in IC_conversion_vs_CEF_download.xlsx (a sheet each, catch_compare first). The figures are in
## output/figures/1_IC_conversion_vs_CEF_download/, a folder for each of five amounts:
##   catch              the catch (t)                               catch_with_length  the catch with a length distribution (t)
##   catch_with_age     the catch with an age distribution (t)      numbers_at_length  the sum of the numbers at length (thousand fish)
##   numbers_at_age     the sum of the numbers at age (thousand fish)
## each for all countries together, by country, by country and area, by country, area and season, and by country, area, season
## and metier4 (a figure for each country). In every figure each stratum has two bars, IC and the RDBES, and each bar is
## stacked by the ORIGIN of the catch records in it: WGEstimate (dark) or Official (light) - Germany, for instance, is
## WGEstimate in IC and Official in the RDBES. Every stratum of the catch is in every figure of its level: one with none of the amount in
## either source (no length distribution, say) is a row without a bar, never left out.
##
## `status` says whether the two agree: "same", "differs", "only IC" (the conversion has it, the RDBES has none: missing
## in the RDBES) or "only RDBES" (missing in the conversion).

## 0. Settings

tol_t   <- 0                 # two amounts are the same when they differ by less than this (a kg) ...
tol_pct <- 0                     # ... or by less than this share (%) of the larger



## ###########################################################################
## 1. The download
## ###########################################################################

### A zip with the catch and the distributions as they are in the RDBES. Its columns are named after their
### table (CNyear, DNyear ...): the prefix goes, and the fields are derived as they were for the conversion.
if (!exists("zipfile") || !file.exists(zipfile))            # the newest download, when the line above was not run
  zipfile <- local({ zips <- list.files("data", "^export_.*[.]zip$", full.names = TRUE); zips[which.max(file.mtime(zips))] })
message("RDBES download: ", zipfile, " (", paste(utils::unzip(zipfile, list = TRUE)$Name, collapse = ", "), ")")
if (!any(grepl("Effort", utils::unzip(zipfile, list = TRUE)$Name))) message("it has no effort table: the conversion made bll_", catch_year, "_effort_CEF_v17.1.csv, the download has none to compare it with")

rdbes_catch <- read_csv(unz(zipfile, "CatchNatEst.csv"), show_col_types = FALSE)
rdbes_dist  <- read_csv(unz(zipfile, "DistributionNatEst.csv"), show_col_types = FALSE)
names(rdbes_catch) <- sub("^CN", "", names(rdbes_catch))
names(rdbes_dist)  <- sub("^DN", "", names(rdbes_dist))

rdbes_catch <- derive_fields(rdbes_catch)
rdbes_dist  <- derive_fields(rdbes_catch, rdbes_dist, what = "distribution")


### The comparison is made inside local(): its helpers and its intermediate tables stay out of the session, and the four tables to open come out.
invisible(list2env(local({
  ## The folders of this comparison, named for what it compares: its tables in the output folder, its figures in output/figures
  compare_dir <- file.path(output_path, "1_IC_conversion_vs_CEF_download")
  tables_dir  <- file.path(compare_dir, "tables")
  fig_dir     <- file.path(output_path, "figures", "1_IC_conversion_vs_CEF_download")   # short: the path of a figure must stay under the 260 characters of Windows

  ## what an earlier run (or an earlier version of this section) wrote goes, so that nothing stale is left next to the new files:
  ## these folders, and the ones they had before they were named for what they compare
  unlink(c(compare_dir, fig_dir,
           file.path(output_path, c("IC_conversion_vs_CEF_download", "IC-conversion_vs_RDBES-download", "rdbes_vs_intercatch")),
           file.path(output_path, "figures", "IC_conversion_vs_CEF_download"), file.path(report_path, "figures", "rdbes_vs_ic"),
           list.files(output_path, "^rdbes_vs_intercatch_.*[.]csv$", full.names = TRUE),
           list.files(file.path(output_path, "figures"), "^rdbes_vs_intercatch_[0-9]+[.]png$", full.names = TRUE)), recursive = TRUE)
  if (dir.exists(file.path(report_path, "figures")) && !length(list.files(file.path(report_path, "figures"), all.files = TRUE, no.. = TRUE)))
    unlink(file.path(report_path, "figures"), recursive = TRUE)     # the folder that held them, when nothing else is in it
  mkdir(tables_dir)


  ## ###########################################################################
  ## 2. One footing: the same columns, the same units, the same spelling
  ## ###########################################################################

  ### The same column can be of another type in the two tables (ICESSubArea is the number 27.4 once read.taf has read it, and
  ### text in the download): what names something is text and what counts is a number, before they are put together.
  text_cols   <- c("vesselFlagCountry", "catchCategory", "originType", "variableType", "variableUnit", "seasonType", "areaValue",
                   "fleetValue", "metier6", "metier4", "ICESSubArea", "domainBiology", "distributionType")
  number_cols <- c("total", "seasonValue", "distributionClass", "value", "numMeasurements")
  same_types  <- function(x, keep) x[keep] %>% mutate(across(any_of(text_cols), as.character), across(any_of(number_cols), as.numeric))

  ### What is only spelled differently, so that it is not a difference in the data: the country (the conversion writes its
  ### name, the RDBES its code), the discards of some countries (DisBMS in the RDBES, Dis in the conversion), the fleet
  ### (All / all). The weights are all in tonnes (the kilos are divided by 1000).
  country_code <- c(Belgium = "BE", Denmark = "DK", Estonia = "EE", Finland = "FI", France = "FR", Germany = "DE",
                    Ireland = "IE", Latvia = "LV", Lithuania = "LT", Netherlands = "NL", Norway = "NO", Poland = "PL",
                    Portugal = "PT", Spain = "ES", Sweden = "SE", "UK (England)" = "GB-ENG", "UK(England)" = "GB-ENG",
                    "UK(Northern Ireland)" = "GB-NIR", "UK(Scotland)" = "GB-SCT", "UK(Wales)" = "GB-WLS")
  as_iso <- function(x) unname(ifelse(x %in% names(country_code), country_code[x], x))
  not_coded <- setdiff(unique(catch_data$vesselFlagCountry), c(names(country_code), rdbes_catch$vesselFlagCountry))
  if (length(not_coded)) warning("no code for the country: ", paste(not_coded, collapse = ", "), " (add it to country_code)")
  season_of <- function(type, value) ifelse(type == "Year", "Year", paste0(substr(type, 1, 1), value))

  ## the distributions first, since the catch records say which of them have one. The length classes are in mm in both.
  ## Numbers are in thousands of fish, mean weights in grams.
  dist_keep <- c("vesselFlagCountry", "catchCategory", "domainBiology", "distributionType", "distributionClass", "variableType",
                 "variableUnit", "value", "numMeasurements", "seasonValue", "areaValue", "fleetValue", "metier4", "ICESSubArea")
  dist_all <- bind_rows(IC = same_types(distributions_data, dist_keep), RDBES = same_types(rdbes_dist, dist_keep), .id = "source")
  n_rows   <- nrow(dist_all)
  dist_all <- distinct(dist_all)                                  # a row that repeats another is not a second sample
  if (nrow(dist_all) < n_rows) message(n_rows - nrow(dist_all), " distribution rows repeat another row exactly and are left out")
  dist_all <- dist_all %>%
    mutate(fish_e3 = case_when(variableType == "Number" & variableUnit == "NE3" ~ value,         # the fish of a class, in thousands
                               variableType == "Number" & variableUnit == "N"   ~ value / 1000))
  stopifnot(!anyNA(dist_all$fish_e3[dist_all$variableType == "Number"]))    # a unit that is neither N nor NE3 has no fish

  ## ... and in the same unit. The mean length (weighted by the fish) of the two has to be about the same: a conversion whose classes are not
  ## those of the download - the rcef before 21 October 2026 made them 10 times too long - compares nothing, and is stopped here.
  mean_mm <- dist_all %>%
    filter(distributionType == "LengthTotal", variableType == "Number") %>%
    group_by(source) %>%
    summarise(mean_mm = sum(distributionClass * fish_e3) / sum(fish_e3))
  if (nrow(mean_mm) == 2 && max(mean_mm$mean_mm) / min(mean_mm$mean_mm) > 1.25)
    stop("The mean length of the conversion (", round(mean_mm$mean_mm[mean_mm$source == "IC"]), " mm) and of the download (",
         round(mean_mm$mean_mm[mean_mm$source == "RDBES"]), " mm) are not about the same: the lengths are not in the same unit. If those of the conversion ",
         "are 10 times too long, it was made by an older rcef: install the current rcef and run this script again (the data/*_CEF.csv written above are not to be used).", call. = FALSE)

  ## the catch: one row for each record of each source, with the tonnes of it that have a length or an age distribution (its
  ## domainBiology is the one of a distribution of the same country and catch category)
  catch_keep <- c("vesselFlagCountry", "catchCategory", "originType", "variableType", "variableUnit", "total", "seasonType",
                  "seasonValue", "areaValue", "fleetValue", "metier6", "metier4", "ICESSubArea", "domainBiology")
  catch_raw <- bind_rows(IC = same_types(catch_data, catch_keep), RDBES = same_types(rdbes_catch, catch_keep), .id = "source") %>%
    filter(variableType == "WeightLive") %>%
    mutate(country  = as_iso(vesselFlagCountry),
           category = ifelse(catchCategory == "DisBMS", "Dis", catchCategory),
           origin   = originType,
           season   = season_of(seasonType, seasonValue),
           total_t  = total * c(kg = 0.001, t = 1)[variableUnit])
  stopifnot(!anyNA(catch_raw$total_t))    # a unit that is neither kg nor t has no tonnes
  message("units of the catch records, all in tonnes from here on:")
  print(table(source = catch_raw$source, unit = catch_raw$variableUnit))

  linked <- function(type) {
    d <- filter(dist_all, distributionType == type)
    catch_raw$source %in% d$source &
      paste(catch_raw$source, catch_raw$vesselFlagCountry, catch_raw$category, catch_raw$domainBiology) %in%
      paste(d$source, d$vesselFlagCountry, ifelse(d$catchCategory == "DisBMS", "Dis", d$catchCategory), d$domainBiology)
  }
  catch_raw <- catch_raw %>% mutate(length_t = ifelse(linked("LengthTotal"), total_t, 0), age_t = ifelse(linked("Age"), total_t, 0))

  ## ... and the fish of those distributions, summed over their classes (thousands): NaL the numbers at length, NaA the numbers at age.
  ## A distribution that several catch records use is shared among them in proportion to their catch weight (in IC each
  ## distribution has one record; in the RDBES the Belgian landings of TBB_DEF_70-99 are one distribution for 14 records, in four
  ## areas and four quarters)
  numbers_of <- function(type) {
    d <- dist_all %>% filter(distributionType == type, variableType == "Number") %>%
      group_by(source, vesselFlagCountry, category = ifelse(catchCategory == "DisBMS", "Dis", catchCategory), domainBiology) %>%
      summarise(fish_e3 = sum(fish_e3), .groups = "drop")
    catch_raw %>%
      mutate(row_d = match(paste(source, vesselFlagCountry, category, domainBiology), paste(d$source, d$vesselFlagCountry, d$category, d$domainBiology))) %>%
      group_by(row_d) %>% mutate(share = if (all(total_t == 0)) 1 / n() else total_t / sum(total_t)) %>% ungroup() %>%
      mutate(fish = ifelse(is.na(row_d), 0, d$fish_e3[row_d] * share)) %>% pull(fish)
  }
  catch_raw$nal_e3 <- numbers_of("LengthTotal")
  catch_raw$naa_e3 <- numbers_of("Age")

  ## the five amounts of the tables and the figures: the catch, the catch with a length and the catch with an age distribution (tonnes),
  ## the numbers at length and the numbers at age (thousand fish)
  variables <- c(catch = "total_t", length = "length_t", age = "age_t", NaL = "nal_e3", NaA = "naa_e3")

  ### A record that lists several areas ("27.4.b;27.4.c;27.7.d;27.7.e") or several fleets in one field cannot be placed in
  ### any one of them: it is compared with what the other source reports for one of them. The Belgian discards of
  ### TBB_DEF_70-99 are one record for four areas in the RDBES and were reported for 27.7.d in IC: the two are one
  ### stratum, under the list. A list of areas is matched on the country, catch category, season and fleet, a list of fleets on the
  ### country, catch category, season and area. `area`, `fleet`, `gear` (the metier4) and `subarea` (the ICES area) are what
  ### the strata are made of; `listed` marks a record that is part of a list.
  area_lists  <- catch_raw %>% filter(grepl(";", areaValue)) %>%
    distinct(country, category, season, fleet0 = tolower(fleetValue), area_list = areaValue) %>%
    mutate(areaValue = strsplit(area_list, ";")) %>% unnest(areaValue) %>%
    distinct(country, category, season, fleet0, areaValue, .keep_all = TRUE)
  fleet_lists <- catch_raw %>% filter(grepl(";", fleetValue)) %>%
    distinct(country, category, season, areaValue, fleet_list = tolower(fleetValue)) %>%
    mutate(fleet0 = strsplit(fleet_list, ";")) %>% unnest(fleet0) %>%
    distinct(country, category, season, areaValue, fleet0, .keep_all = TRUE)
  place_lists <- function(x)
    x %>% mutate(fleet0 = tolower(fleetValue)) %>%
      left_join(area_lists, by = c("country", "category", "season", "fleet0", "areaValue"), relationship = "many-to-one") %>%
      left_join(fleet_lists, by = c("country", "category", "season", "areaValue", "fleet0"), relationship = "many-to-one") %>%
      mutate(area    = coalesce(area_list, areaValue),
             fleet   = coalesce(fleet_list, fleet0),
             subarea = ifelse(grepl(";", area), "several areas", coalesce(ICESSubArea, "unknown")),
             gear    = ifelse(grepl(";", fleet), vapply(strsplit(fleet, ";"), function(f) paste(sort(unique(toupper(sub("_.*$", "", f)))), collapse = ";"), ""),
                              coalesce(metier4, "unknown")),
             listed  = !is.na(area_list) | !is.na(fleet_list)) %>%
      select(-fleet0, -area_list, -fleet_list)
  catch_all <- place_lists(catch_raw)
  for (v in names(variables))                      # the origin of what each amount counts: of the records that have some of it
    catch_all[[paste0("origin_", v)]] <- ifelse(catch_all[[variables[[v]]]] > 0, catch_all$origin, NA)
  catch_all <- catch_all %>% group_by(source, vesselFlagCountry, category, domainBiology) %>%      # the records that use a distribution
    mutate(numbers_split = nal_e3 + naa_e3 > 0 &                                                  # together with records of another
             n_distinct(paste(area, season, fleet)[nal_e3 + naa_e3 > 0]) > 1) %>% ungroup()       # area, season or fleet
  twice <-catch_all %>% filter(total_t > 0) %>% group_by(source, country, category, season, area, fleet) %>%
    summarise(origins = n_distinct(origin), .groups = "drop") %>% filter(origins > 1)
  if (nrow(twice)) warning(nrow(twice), " strata have catch of more than one origin in a source and they are added up: check that it is not the same catch twice")

  ## the distributions: one row for each class of each distribution (a distribution is a domainBiology of a country and a catch
  ## category), and one row for each distribution
  dist_cmp <- dist_all %>%
    mutate(country  = as_iso(vesselFlagCountry),
           category = ifelse(catchCategory == "DisBMS", "Dis", catchCategory),
           season   = ifelse(is.na(seasonValue), NA_character_, ifelse(seasonValue > 4, "Year", paste0("Q", seasonValue))),
           class    = distributionClass,
           weight_g = case_when(variableType == "WeightLive" & variableUnit == "g"  ~ value,
                                variableType == "WeightLive" & variableUnit == "kg" ~ value * 1000)) %>%
    place_lists() %>%
    mutate(fleet   = ifelse(is.na(fleetValue), "several fleets", fleet),            # a domain that several strata use has no
           subarea = ifelse(is.na(areaValue), "several areas", subarea))            # single fleet or area
  stopifnot(!anyNA(dist_cmp$weight_g[dist_cmp$variableType == "WeightLive"]))     # a unit that is neither g nor kg has no weight
  dist_class <- dist_cmp %>%
    filter(variableType %in% c("Number", "WeightLive")) %>%
    group_by(source, country, category, distributionType, domainBiology, fleet, subarea, class) %>%
    summarise(fish_e3  = sum(fish_e3, na.rm = TRUE),
              weight_g = if (all(is.na(weight_g))) NA_real_ else mean(weight_g, na.rm = TRUE),
              measured = if (all(is.na(numMeasurements))) 0 else max(numMeasurements, na.rm = TRUE), .groups = "drop")
  dist_dom <- dist_class %>%
    group_by(source, country, category, distributionType, domainBiology, fleet, subarea) %>%
    summarise(fish_e3 = sum(fish_e3), measured = max(measured), classes = n(), weighted = sum(!is.na(weight_g)), .groups = "drop")


  ## ###########################################################################
  ## 3. The join: IC and the RDBES side by side
  ## ###########################################################################

  ### Three small tools, used for every table below.
  ## status_of: do two amounts agree? "same" also when neither has anything; "only IC" / "only RDBES" when just one has.
  status_of <- function(ic, rdbes)
    case_when(ic <= tol_t & rdbes <= tol_t ~ "same",
              rdbes <= tol_t ~ "only IC",
              ic <= tol_t ~ "only RDBES",
              abs(rdbes - ic) <= pmax(tol_t, tol_pct / 100 * pmax(ic, rdbes)) ~ "same",
              TRUE ~ "differs")

  ## join_sources: the records of `data` (one row per record, a column `source`) added up by the columns of `key`, once for
  ## IC and once for the RDBES, and the two joined by `key`: `<amount>_IC` and `<amount>_RDBES` next to each other, and
  ## `rows_IC` / `rows_RDBES`, how many records there are. A stratum of only one source has 0 in the other.
  join_sources <- function(data, key, amounts, texts = character()) {
    one <- function(s)
      data %>% filter(source == s) %>% group_by(across(all_of(key))) %>%
        summarise(across(all_of(amounts), sum), rows = n(),
                  across(all_of(texts), ~ paste(sort(unique(.x)), collapse = "; ")), .groups = "drop")
    full_join(one("IC"), one("RDBES"), by = key, suffix = c("_IC", "_RDBES")) %>%
      mutate(across((ends_with("_IC") | ends_with("_RDBES")) & where(is.numeric), ~ coalesce(.x, 0)))
  }

  ## gap: a joined table added up by `by` (and the catch category) for one of the amounts of `variables`, `v`: IC, RDBES, the
  ## difference and the status - and the origins of the catch records that have some of that amount, when the table has them
  origins_of <- function(x) paste(sort(unique(unlist(strsplit(x[nzchar(x)], "; ", fixed = TRUE)))), collapse = "; ")
  gap <- function(wide, by, v = "catch") {
    amount <- variables[[v]]
    origin_cols <- paste0("origin_", v, c("_IC", "_RDBES"))
    wide %>%
      group_by(across(all_of(c(by, "category")))) %>%
      summarise(IC = sum(.data[[paste0(amount, "_IC")]]), RDBES = sum(.data[[paste0(amount, "_RDBES")]]),
                rows_IC = sum(rows_IC), rows_RDBES = sum(rows_RDBES),
                across(any_of(origin_cols), origins_of), .groups = "drop") %>%
      rename(any_of(c(origin_IC = origin_cols[[1]], origin_RDBES = origin_cols[[2]]))) %>%
      mutate(diff = round(RDBES - IC, 3), diff_pct = ifelse(IC > 0, round(100 * (RDBES - IC) / IC, 1), NA),
             status = status_of(IC, RDBES), IC = round(IC, 3), RDBES = round(RDBES, 3)) %>%
      select(all_of(c(by, "category")), IC, RDBES, diff, diff_pct, status, any_of(c("origin_IC", "origin_RDBES")), rows_IC, rows_RDBES) %>%
      arrange(category, desc(abs(diff)))
  }

  ### The catch, stratum by stratum: a stratum is a country, a catch category, a season, an area and a fleet.
  ###   total_t_IC, total_t_RDBES  the total of the stratum in tonnes: the records of kg and of t are all converted (unit_IC and
  ###                              unit_RDBES are the units they were in); the RDBES has the metier6 of each record where the
  ###                              conversion has none, so its records are added up over them (metier6_RDBES says which)
  ###   diff_t, diff_pct, status   RDBES - IC, and whether the two agree
  ###   status_ices_area           the status of the same stratum with the season and the area detail left out (country, fleet,
  ###                              ICES area): "same" there and "only IC" here is the catch being split another way over the
  ###                              seasons and areas; "only IC" in both is catch the RDBES has nowhere
  ###   origin_IC, origin_RDBES    the origin of the catch records of the stratum (those with catch): WGEstimate or Official
  ###   rows_IC, rows_RDBES        how many records the stratum has in each source
  ###   length_t_*, age_t_*        the tonnes of the stratum that have a length or an age distribution
  ###   nal_e3_*, naa_e3_*         the fish of those distributions summed over the classes (thousands): the numbers at length and at
  ###                              age. A distribution that records of several strata use is shared among them in proportion to
  ###                              their catch weight
  ### subarea (the ICES area: 27.4 for 27.4.a) and gear (the first part of the fleet) are there to filter and group by.
  stratum    <- c("country", "category", "season", "area", "fleet", "subarea", "gear")
  catch_wide <- join_sources(catch_all, stratum, unname(variables),
                             texts = c(paste0("origin_", names(variables)), "fleetValue", "metier6", "variableUnit")) %>%
    group_by(country, category, fleet, subarea) %>%
    mutate(status_ices_area = status_of(sum(total_t_IC), sum(total_t_RDBES))) %>% ungroup()
  season_levels <- c("Q1", "Q2", "Q3", "Q4", "Year")
  catch_compare <- catch_wide %>%
    mutate(diff_t = round(total_t_RDBES - total_t_IC, 3),
           diff_pct = ifelse(total_t_IC > 0, round(100 * (total_t_RDBES - total_t_IC) / total_t_IC, 1), NA),
           status = status_of(total_t_IC, total_t_RDBES),
           across(all_of(c(paste0(unname(variables), "_IC"), paste0(unname(variables), "_RDBES"))), ~ round(.x, 3))) %>%
    select(all_of(stratum), total_t_IC, total_t_RDBES, diff_t, diff_pct, status, status_ices_area,
           origin_IC = origin_catch_IC, origin_RDBES = origin_catch_RDBES, rows_IC, rows_RDBES,
           unit_IC = variableUnit_IC, unit_RDBES = variableUnit_RDBES, fleetValue_IC, fleetValue_RDBES, metier6_RDBES,
           length_t_IC, length_t_RDBES, age_t_IC, age_t_RDBES, nal_e3_IC, nal_e3_RDBES, naa_e3_IC, naa_e3_RDBES) %>%
    arrange(match(category, c("Lan", "Dis", "BMS")), country, area, match(season, season_levels), fleet)


  ## ###########################################################################
  ## 4. The catch added up: by country, area, season and metier4
  ## ###########################################################################

  ### The five levels of the figures, for each of the five amounts of `variables` (the catch, the catch with a length distribution,
  ### the catch with an age distribution, the numbers at length and the numbers at age): every table is catch_compare added up,
  ### with the origins of the records of each source that have some of the amount
  levels    <- list(total = character(), country = "country", country_area = c("country", "area"),
                    country_area_season = c("country", "area", "season"),
                    country_area_season_metier4 = c("country", "area", "season", "gear"))
  rdbes_gap <- list()
  for (v in names(variables)) for (l in names(levels))
    rdbes_gap[[paste(v, l, sep = "_")]] <- gap(catch_wide, levels[[l]], v)
  rdbes_gap$origin <- gap(join_sources(catch_all, c("country", "category", "origin"), "total_t"), c("country", "origin"))

  ### What was only spelled differently or put together, and what has another origin: how many records and tonnes of each
  flag <- function(data, item) data %>% group_by(source) %>%
    summarise(item = item, rows = n(), total_t = round(sum(total_t), 3), countries = paste(sort(unique(country)), collapse = " "), .groups = "drop")
  rdbes_gap$flags <- bind_rows(
    flag(filter(catch_all, catchCategory == "DisBMS"), "discards reported as DisBMS (compared as Dis)"),
    flag(filter(catch_all, grepl(";", areaValue)), "several areas in one record"),
    flag(filter(catch_all, grepl(";", fleetValue)), "several fleets in one record"),
    flag(filter(catch_all, listed, !grepl(";", areaValue), !grepl(";", fleetValue)), "records compared with a record that lists several areas or fleets"),
    flag(filter(catch_all, origin != "WGEstimate", total_t > 0), "catch of another origin than WGEstimate"),
    flag(filter(catch_all, numbers_split), "records that use a distribution together with records of another area, season or fleet (its numbers are shared among them by catch weight)"))


  ## ###########################################################################
  ## 5. The distributions, stratum by stratum and class by class
  ## ###########################################################################

  ### Stratum by stratum: a stratum is what the catch is by (country, catch category, fleet, ICES area), and its distributions
  ### are added up: the fish (thousands), the fish measured, how many length or age classes there are and how many of them have
  ### a mean weight (summed over the distributions of the stratum).
  dist_wide <- join_sources(dist_dom, c("country", "category", "distributionType", "fleet", "subarea"),
                            c("fish_e3", "measured", "classes", "weighted"))
  dist_compare <- dist_wide %>%
    mutate(diff_fish_e3 = round(fish_e3_RDBES - fish_e3_IC, 3), status = status_of(fish_e3_IC, fish_e3_RDBES)) %>%
    mutate(across(starts_with("fish_e3_"), ~ round(.x, 3))) %>%
    select(country, category, distributionType, fleet, subarea, fish_e3_IC, fish_e3_RDBES, diff_fish_e3, status,
           measured_IC, measured_RDBES, classes_IC, classes_RDBES, weighted_IC, weighted_RDBES, distributions_IC = rows_IC,
           distributions_RDBES = rows_RDBES) %>%
    arrange(match(category, c("Lan", "Dis", "BMS")), distributionType, country, fleet, subarea)

  ### Class by class: the fish and the mean weight of each class of each stratum (the weights weighted by the fish)
  class_wide <- dist_class %>%
    mutate(grams = ifelse(is.na(weight_g), 0, fish_e3 * weight_g), fish_w = ifelse(is.na(weight_g), 0, fish_e3)) %>%
    join_sources(c("country", "category", "distributionType", "fleet", "subarea", "class"), c("fish_e3", "grams", "fish_w"))
  dist_class_compare <- class_wide %>%
    mutate(weight_g_IC    = round(ifelse(fish_w_IC > 0, grams_IC / fish_w_IC, NA), 1),
           weight_g_RDBES = round(ifelse(fish_w_RDBES > 0, grams_RDBES / fish_w_RDBES, NA), 1),
           diff_fish_e3   = round(fish_e3_RDBES - fish_e3_IC, 3), status = status_of(fish_e3_IC, fish_e3_RDBES),
           fish_e3_IC = round(fish_e3_IC, 3), fish_e3_RDBES = round(fish_e3_RDBES, 3)) %>%
    select(country, category, distributionType, fleet, subarea, class, fish_e3_IC, fish_e3_RDBES, diff_fish_e3, status,
           weight_g_IC, weight_g_RDBES) %>%
    arrange(match(category, c("Lan", "Dis", "BMS")), distributionType, country, fleet, subarea, class)


  ## ###########################################################################
  ## 6. The tables, written, and in short
  ## ###########################################################################

  ### Every table as a csv, and all of them in one Excel workbook (a sheet each, a filter on the first row): open catch_compare first
  sheets <- c(list(catch_compare = catch_compare, dist_compare = dist_compare, dist_class_compare = dist_class_compare), rdbes_gap)
  stopifnot(!anyDuplicated(substr(names(sheets), 1, 31)))          # the name of an Excel sheet has at most 31 characters
  for (nm in names(sheets))
    tryCatch(write.taf(sheets[[nm]], file = paste0(ifelse(nm %in% names(rdbes_gap), "gap_", ""), nm, ".csv"), dir = tables_dir, quote = TRUE),
             error = function(e) warning("could not write ", nm, ": ", conditionMessage(e), " (is it open in Excel?)", call. = FALSE))
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    wb <- openxlsx::createWorkbook()
    for (nm in names(sheets)) {
      openxlsx::addWorksheet(wb, substr(nm, 1, 31))
      openxlsx::writeData(wb, substr(nm, 1, 31), as.data.frame(sheets[[nm]]), withFilter = TRUE)
      openxlsx::freezePane(wb, substr(nm, 1, 31), firstRow = TRUE)
      openxlsx::setColWidths(wb, substr(nm, 1, 31), cols = seq_along(sheets[[nm]]), widths = "auto")
    }
    tryCatch(openxlsx::saveWorkbook(wb, file.path(compare_dir, "IC_conversion_vs_CEF_download.xlsx"), overwrite = TRUE),
             error = function(e) warning("could not write the workbook: ", conditionMessage(e), " (is it open in Excel?)", call. = FALSE))
  } else message("install the package openxlsx to get the tables as one Excel workbook")

  ### In short: the catch of each source by country and the origin of its records; what was spelled differently or put together;
  ### the strata that are the same, differ or are in one source only, and for each whether the other source has the catch of
  ### the stratum at all once the season and the area detail are left out (status_ices_area).
  print(as.data.frame(rdbes_gap$catch_country))
  print(as.data.frame(rdbes_gap$flags))
  print(catch_compare %>% filter(category %in% c("Lan", "Dis")) %>% group_by(category, status, status_ices_area) %>%
          summarise(strata = n(), total_t_IC = round(sum(total_t_IC), 1), total_t_RDBES = round(sum(total_t_RDBES), 1), .groups = "drop") %>%
          filter(total_t_IC + total_t_RDBES > 0) %>% as.data.frame())


  ## ###########################################################################
  ## 7. The figures
  ## ###########################################################################

  ### One figure for each level (the five of `levels`) of each amount of `variables` (the catch, the catch with a length distribution,
  ### the catch with an age distribution, the numbers at length and at age). Every stratum has two bars, IC and the RDBES, and
  ### each bar is stacked by the origin of its catch records: the colour of the source, dark for WGEstimate and lighter for the
  ### other origins.
  source_col <- plot_colours(c("IC", "RDBES"), "source")      # the two sources keep their colour in every figure
  origin_col <- function(origins)                              # the colour of each source and origin
    unlist(lapply(names(source_col), function(s)
      setNames(vapply(seq_along(origins), function(i) scales::col_mix(source_col[[s]], "white", amount = c(0, 0.55, 0.8)[min(i, 3)]), ""),
               paste(s, origins))))

  plot_levels <- function(data, by, value, what, unit = "Tonnes", all_categories = FALSE, by_country = FALSE, horizontal = FALSE,
                          label_values = FALSE, label_origin = FALSE, ncol = 4, note = NULL) {
    d <- data %>% filter(category %in% c("Lan", "Dis"))
    if (all_categories) d <- bind_rows(d, mutate(d, category = "All"))
    d <- d %>% group_by(across(all_of(c(by, "category", "source", "origin")))) %>%
      summarise(y = sum(.data[[value]]), .groups = "drop")
    inner <- setdiff(by, "country")
    d$stratum <- if (length(inner)) do.call(paste, c(d[inner], sep = " ")) else if ("country" %in% by) d$country else "all countries"
    d$panel   <- if (by_country) paste(d$country, d$category, sep = " - ") else as.character(d$category)
    ## every stratum is drawn, also one with none of this amount in either source (a row without a bar): a stratum that is not in a figure
    ## looks like data that is missing. Only the strata that have some of it say which origins colour the bars
    some <- d %>% group_by(panel, stratum) %>% filter(sum(y) > 0) %>% ungroup()
    ## the order of the strata: area, then season, then metier4; of the panels: country, then Lan and Dis
    keys <- lapply(inner, function(k) if (k == "season") match(d[[k]], season_levels) else d[[k]])
    stratum_levels <- unique(d$stratum[do.call(order, c(keys, list(d$stratum)))])
    panel_levels   <- unique(d$panel[order(if (by_country) d$country else rep("", nrow(d)), match(d$category, c("Lan", "Dis", "All")))])
    if (!nrow(some)) return(NULL)                       # none of this anywhere: nothing to draw
    if (nrow(distinct(d, panel, stratum)) > nrow(distinct(some, panel, stratum)))
      note <- c(note, "Every stratum is drawn; one with none of this in either source has no bar.")
    ## both sources in every stratum, so that IC is always the first bar and the RDBES the second; the origins stacked
    origin_levels <- c(intersect("WGEstimate", some$origin), setdiff(sort(unique(some$origin)), "WGEstimate"))
    d <- select(d, panel, stratum, source, origin, y) %>% mutate(origin = ifelse(y > 0, origin, NA))      # no amount, no origin
    d <- distinct(d, panel, stratum) %>% tidyr::crossing(source = c("IC", "RDBES")) %>%
      left_join(d, by = c("panel", "stratum", "source")) %>%
      mutate(y = coalesce(y, 0), origin = factor(coalesce(origin, origin_levels[[1]]), origin_levels)) %>%
      arrange(panel, stratum, source, origin) %>% group_by(panel, stratum, source) %>% mutate(cum = cumsum(y)) %>%
      filter(y > 0 | (!any(y > 0) & row_number() == 1)) %>% ungroup() %>%                  # an origin with no catch is not stacked
      mutate(fill_id = paste(source, origin), source = factor(source, if (horizontal) c("RDBES", "IC") else c("IC", "RDBES")),
             stratum = factor(stratum, if (horizontal) rev(stratum_levels) else stratum_levels),
             panel = factor(panel, panel_levels)) %>%
      arrange(desc(cum))                                     # the tallest first: the shorter ones are drawn over it
    n_strata <- length(unique(d$stratum)); n_panels <- length(unique(d$panel))
    size <- if (horizontal) c(width = 11, height = max(4, 1.8 + 0.3 * n_strata))
            else c(width = min(20, 3.2 * min(n_panels, ncol) + 1), height = 2.4 + 3.6 * ceiling(n_panels / ncol) + if (n_strata > 8) 1.2 else 0)
    fills <- origin_col(origin_levels)
    present <- unique(d$fill_id[d$y > 0])
    dodge <- position_dodge(width = 0.8)
    p <- (if (horizontal) ggplot(d, aes(x = cum, y = stratum, fill = fill_id, group = source)) + geom_col(orientation = "y", position = dodge, width = 0.8)
          else ggplot(d, aes(x = stratum, y = cum, fill = fill_id, group = source)) + geom_col(position = dodge, width = 0.8))
    ## the total at the top of each bar, and the origin where it is not WGEstimate. Every bar has a label, empty where there is
    ## nothing to say, so that the labels dodge like the bars
    tilted <- !horizontal && n_strata > 4
    tops <- d %>% group_by(panel, stratum, source) %>% slice_max(cum, n = 1, with_ties = FALSE) %>% ungroup() %>%
      mutate(label = ifelse(cum > 0, format(round(cum, 1), big.mark = ","), ""))
    if (label_values) p <- p + geom_text(data = tops, aes(label = label), position = dodge, size = 2.8,
                                         angle = if (tilted) 90 else 0, vjust = if (horizontal || tilted) 0.5 else -0.4,
                                         hjust = if (horizontal || tilted) -0.1 else 0.5)
    ## the origin is written in the part of a bar that is not the first origin, when the part is tall enough to hold the word (the legend
    ## names the colours in any case)
    others <- d %>% group_by(panel) %>%
      mutate(label = ifelse(origin != origin_levels[[1]] & y >= 0.16 * max(cum), as.character(origin), "")) %>% ungroup()
    if (label_origin && any(nzchar(others$label))) p <- p + geom_text(data = others, aes(label = label), position = dodge, size = 2.6, colour = "grey20",
                                                         angle = if (horizontal) 0 else 90, vjust = 0.5, hjust = 1.1)
    caption <- paste0("IC: the InterCatch data converted to CEF. RDBES: the CEF downloaded from the RDBES database. Two bars in each stratum, IC ", if (horizontal) "above" else "on the left",
                      " and the RDBES ", if (horizontal) "below" else "on the right",
                      "; the colour is the origin of the catch records in the bar, stacked: WGEstimate (dark), Official (light). ", unit, ".")
    caption <- paste(unlist(lapply(c(caption, note), strwrap, width = max(70, round(size[["width"]] * 9)))), collapse = "\n")
    p <- p +
      (if (horizontal) facet_grid(. ~ panel, scales = "free_x") else facet_wrap(vars(panel), scales = "free", ncol = ncol)) +
      scale_fill_manual(values = fills, breaks = names(fills)[names(fills) %in% present], labels = function(x) sub(" ", " - ", x), name = NULL) +
      labs(title = what, x = NULL, y = NULL, caption = caption) +
      app_theme() + theme(legend.position = "top", strip.text = element_text(size = 9))
    if (horizontal) p <- p + scale_x_continuous(expand = expansion(mult = c(0, 0.15))) + theme(axis.text.y = element_text(size = 8))
    else p <- p + scale_y_continuous(expand = expansion(mult = c(0, if (label_values) 0.25 else 0.15))) +
      theme(axis.text.x = element_text(angle = if (n_strata <= 3) 0 else if (n_strata > 6) 90 else 45,
                                       hjust = if (n_strata <= 3) 0.5 else 1, vjust = 0.5))
    list(plot = p, size = size)
  }

  fig_files <- character()
  fig_folder <- c(catch = "catch", length = "catch_with_length", age = "catch_with_age",       # the folder of each amount, named for what is in it
                  NaL = "numbers_at_length", NaA = "numbers_at_age")
  save_fig <- function(res, folder, name) {
    if (is.null(res)) return(invisible(NULL))
    folder <- fig_folder[[folder]]
    mkdir(file.path(fig_dir, folder))
    file <- file.path(fig_dir, folder, paste0(name, ".png"))
    if (nchar(normalizePath(file, mustWork = FALSE)) > 250) warning("the path of ", file, " is too long for Windows (260): move the project nearer the root")
    ggsave(file, res$plot, width = res$size[["width"]], height = res$size[["height"]], dpi = 150, limitsize = FALSE)
    fig_files <<- c(fig_files, file)
  }

  titles <- c(catch = "Catch", length = "Catch with a length distribution (the sampled catch)", age = "Catch with an age distribution",
              NaL = "Sum of numbers at length (thousand fish)", NaA = "Sum of numbers at age (thousand fish)")
  units  <- c(catch = "Tonnes", length = "Tonnes", age = "Tonnes", NaL = "Thousand fish", NaA = "Thousand fish")
  by_what <- c(total = "all countries", country = "by country", country_area = "by country and area",
               country_area_season = "by country, area and season", country_area_season_metier4 = "by country, area, season and metier4")
  for (v in names(variables)) {
    numbers <- v %in% c("NaL", "NaA")
    note <- c(if (v %in% c("age", "NaA")) "The conversion of InterCatch has no ages.",
              if (numbers) "The fish of the distributions summed over their classes; the origin is that of the catch records that use the distribution.")
    for (i in seq_along(levels)) {
      l <- names(levels)[[i]]
      title <- paste(titles[[v]], "-", by_what[[l]])
      shared <- if (numbers && i >= 3) "A distribution that several catch records use is shared among them in proportion to their catch weight."
      if (l == "country_area_season_metier4") {
        ## the finest level: a figure for each country, with all its strata
        for (cty in sort(unique(catch_all$country)))
          save_fig(plot_levels(filter(catch_all, country == cty), levels[[l]], variables[[v]],
                               paste0(titles[[v]], " - ", cty, ": by area, season and metier4"), unit = units[[v]],
                               horizontal = TRUE, note = c(note, shared)),
                   v, paste0(i, "_", cty, "_area_season_metier4"))
      } else {
        save_fig(plot_levels(catch_all, levels[[l]], variables[[v]], title, unit = units[[v]], all_categories = l %in% c("total", "country"),
                             by_country = l %in% c("country_area", "country_area_season"), label_values = l %in% c("total", "country"),
                             label_origin = l %in% c("total", "country"), ncol = c(3, 3, 5, 4)[min(i, 4)], note = c(note, shared)), v, paste0(i, "_", l))
      }
    }
  }
  message(length(fig_files), " figures in ", fig_dir)
  print(data.frame(figure = sub(paste0(fig_dir, "/"), "", fig_files, fixed = TRUE)))

  list(catch_compare = catch_compare, dist_compare = dist_compare, dist_class_compare = dist_class_compare, rdbes_gap = rdbes_gap)
}), envir = globalenv()))
