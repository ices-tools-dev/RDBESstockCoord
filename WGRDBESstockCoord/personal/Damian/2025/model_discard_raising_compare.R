## Compare the raisings of the discards: the usual method, the usual method with 3.a.21 (when model_discard_raising.R made it) and the
## hierarchical groups

## Before: model/catch_data_raised.csv, model/catch_data_raised_3a21.csv (model_discard_raising.R: its section 7, which a year may leave out),
##         model/catch_data_raised_hierarchical.csv (model_discard_raising_hierarchical.R)
## After:  output/2c_discard_raising_comparison/raising_methods.csv (the table of the report)

## Run the raising scripts first. The figures are in output/figures/2c_discard_raising_comparison.

library(icesTAF)
library(dplyr)
library(rcef)
library(here)

## The project folder, wherever the repo (RDBESstockCoord) is cloned: every path below is relative to it
here::i_am("WGRDBESstockCoord/personal/Damian/2025/model_discard_raising_compare.R")
setwd(here("WGRDBESstockCoord", "personal", "Damian", "2025"))

mkdir("model")
mkdir("output")
mkdir("output/2c_discard_raising_comparison")

## Every figure of this script is written to output/figures/2c_discard_raising_comparison (the folders of output/figures are
## numbered in the order the scripts make them), numbered in the order it is drawn. What an earlier run left there goes first.
## A figure the report shows has a name as well - fig("name") - and the report finds it by the name, not by the number.
fig <- local({
  dir <- "output/figures/2c_discard_raising_comparison"
  unlink(dir, recursive = TRUE)
  mkdir(dir)
  n <- 0
  function(name = NULL) sprintf("%s/discard_raising_comparison_%02d%s.png", dir, n <<- n + 1, if (is.null(name)) "" else paste0("_", name))
})

## What an earlier version of this script saved for the report goes
unlink(c("output/raising_scenarios_comparison.csv", file.path("output/2c_discard_raising_comparison", paste0("raising_", c("two", "three"), "_methods.csv"))))

### The methods that were run, one raised catch each (the one with 3.a.21 is only there when model_discard_raising.R ran its section 7).
### Landings and discards only: BMS is not raised, and after model_length_allocation.R the saved catch of the usual method has none.
raised <- lapply(Filter(file.exists, c("Usual method" = "model/catch_data_raised.csv",
                                       "Usual method with 3.a.21" = "model/catch_data_raised_3a21.csv",
                                       "Hierarchical groups" = "model/catch_data_raised_hierarchical.csv")),
                 function(f) filter(read.taf(file = f), catchCategory != "BMS"))
stopifnot("two raisings or more are compared: run the raising scripts first" = length(raised) >= 2)
names(raised)

## ###########################################################################
## The methods side by side
## ###########################################################################

### Imported against raised, by catch category, for each method. Rows that came out as NaN are raised rows whose group had no donor to
### draw a rate from: they raise to nothing, and the "No donor (rows)" column counts them. The groups of recommend_groups() have no raised
### row without a donor.
raising_methods <- raised_overview_table(raised)
raising_methods

### Saved for the report (report.qmd): imported against raised, by catch category, for each method.
write.taf(raising_methods, file = "raising_methods.csv", dir = "output/2c_discard_raising_comparison")

### The same at any other level: add the variables to aggregate by.
raised_overview_table(raised, by = "metier4")

raised_overview_table(raised, by = "areaValue")

### The discards of the methods side by side: bars = catch weight, colour = method, shade = imported or raised. catchCategory = "Dis" (or
### "Lan") keeps just the discards (or the landings). Per metier, per area and per season ...
plot_compare_raised(raised, x = "metier4", catchCategory = "Dis", save = fig("methods_metier"))

plot_compare_raised(raised, x = "areaValue", catchCategory = "Dis", save = fig())

plot_compare_raised(raised, x = "seasonValue", catchCategory = "Dis", save = fig())

### ... and per metier in every country.
plot_compare_raised(raised, x = "metier4", facet = "vesselFlagCountry", catchCategory = "Dis", save = fig())
