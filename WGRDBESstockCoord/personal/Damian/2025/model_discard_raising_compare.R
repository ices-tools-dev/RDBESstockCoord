## Compare the raisings of the discards: the usual method, the usual method with 3.a.21 (when model_discard_raising.R made it) and the
## hierarchical groups

## Before: model/catch_data_raised.csv, model/catch_data_raised_3a21.csv (model_discard_raising.R),
##         model/catch_data_raised_hierarchical.csv (model_discard_raising_hierarchical.R)
## After:  output/raising_scenarios_comparison.csv (the two methods of model_discard_raising.R, for the report),
##         output/2c_discard_raising_comparison/raising_three_methods.csv

## Run the three raising scripts first. The figures are in output/figures/2c_discard_raising_comparison.

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
fig_dir <- "output/figures/2c_discard_raising_comparison"
unlink(fig_dir, recursive = TRUE)
mkdir(fig_dir)
fig_n <- 0
fig <- function() sprintf("%s/discard_raising_comparison_%02d.png", fig_dir, fig_n <<- fig_n + 1)

### Landings and discards only: BMS is not raised, and after model_length_allocation.R the saved catch of the usual method has none.
raised_catch_usual <- read.taf(file = "model/catch_data_raised.csv") %>%
  filter(catchCategory != "BMS")

### The alternative with 3.a.21 raised on its own is section 7 of model_discard_raising.R: when that section is not run (it is commented out in
### the script) there is no file of it, and the usual method is compared with the hierarchical groups only.
with_3a21 <- file.exists("model/catch_data_raised_3a21.csv")
raised_catch_3a21 <- if (with_3a21) read.taf(file = "model/catch_data_raised_3a21.csv") %>%
  filter(catchCategory != "BMS")

raised_catch_hierarchical <- read.taf(file = "model/catch_data_raised_hierarchical.csv") %>%
  filter(catchCategory != "BMS")

## ###########################################################################
## The two methods of model_discard_raising.R: the usual method, and the usual method with 3.a.21 raised on its own
## ###########################################################################

if (with_3a21) {
  ### The scenarios are called after their objects unless they are named; the names show in the tables and in the plot legend.
  scenarios_names_two <- c("Usual method", "Usual method with 3.a.21")

  ### Imported against raised, by catch category, for both. Rows that came out as NaN are raised rows whose group had no donor to
  ### draw a rate from: they raise to nothing, and the "No donor (rows)" column counts them.
  comp_table <- raised_overview_table(c(raised_catch_usual, raised_catch_3a21),
                                      scenarios_names = scenarios_names_two)
  print(comp_table)

  ### Saved for the report (report.qmd): imported against raised, by catch category, for both methods.
  write.taf(comp_table, file = "raising_scenarios_comparison.csv", dir = "output")

  ### The same at any other level: add the variables to aggregate by.
  print(raised_overview_table(c(raised_catch_usual, raised_catch_3a21),
                              scenarios_names = scenarios_names_two,
                              by = c("seasonValue", "metier4")))

  ### The methods side by side: bars = catch weight, colour = method, shade = imported or raised. catchCategory = "Dis" (or "Lan")
  ### keeps just the discards (or the landings).
  print(plot_compare_raised(c(raised_catch_usual, raised_catch_3a21),
                            scenarios_names = scenarios_names_two,
                            x = "metier4",
                            catchCategory = "Dis", save = fig()))

  print(plot_compare_raised(c(raised_catch_usual, raised_catch_3a21),
                            scenarios_names = scenarios_names_two,
                            x = "metier4",
                            facet = "vesselFlagCountry",
                            catchCategory = "Dis", save = fig()))
} else unlink("output/raising_scenarios_comparison.csv")   # no table of an earlier run (or of another year) for the report

## ###########################################################################
## The methods side by side: the ones above and the hierarchical groups (model_discard_raising_hierarchical.R)
## ###########################################################################

### The methods that were run, in this order (a list: the one with 3.a.21 may be absent)
raised_methods <- c(list(raised_catch_usual), if (with_3a21) list(raised_catch_3a21), list(raised_catch_hierarchical))
scenarios_names <- c("Usual method", if (with_3a21) "Usual method with 3.a.21", "Hierarchical groups")

### Imported against raised, by catch category, for each. The groups of recommend_groups() have no raised row without a donor.
methods_table <- raised_overview_table(raised_methods,
                                     scenarios_names = scenarios_names)
methods_table

write.taf(methods_table, file = sprintf("raising_%s_methods.csv", c("two", "three")[length(raised_methods) - 1]), dir = "output/2c_discard_raising_comparison")

### The same by metier and by area: where the methods differ.
raised_overview_table(raised_methods,
                      scenarios_names = scenarios_names,
                      by = "metier4")

raised_overview_table(raised_methods,
                      scenarios_names = scenarios_names,
                      by = "areaValue")

### The discards of the three, per metier, per area and per season ...
plot_compare_raised(raised_methods,
                    scenarios_names = scenarios_names,
                    x = "metier4",
                    catchCategory = "Dis", save = fig())

plot_compare_raised(raised_methods,
                    scenarios_names = scenarios_names,
                    x = "areaValue",
                    catchCategory = "Dis", save = fig())

plot_compare_raised(raised_methods,
                    scenarios_names = scenarios_names,
                    x = "seasonValue",
                    catchCategory = "Dis", save = fig())

### ... and per metier in every country.
plot_compare_raised(raised_methods,
                    scenarios_names = scenarios_names,
                    x = "metier4",
                    facet = "vesselFlagCountry",
                    catchCategory = "Dis", save = fig())
