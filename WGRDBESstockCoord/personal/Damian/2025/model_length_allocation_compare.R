## Compare the two allocation plans of the lengths: the groups written by hand and the groups chosen from the hierarchy

## Before: model/catch_data_raised.csv and model/distributions_data_allocated.csv (the plan of model_length_allocation.R, which saves
##         the catch of its allocation as model/catch_data_raised.csv),
##         model/catch_data_allocated_hierarchical.csv and model/distributions_data_allocated_hierarchical.csv
##         (model_length_allocation_hierarchical.R)
## After:  output/3c_length_allocation_comparison/*.csv

## Run the two allocation scripts first. The figures are in output/figures/3c_length_allocation_comparison.

library(icesTAF)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rcef)
library(here)

## The project folder, wherever the repo (RDBESstockCoord) is cloned: every path below is relative to it
here::i_am("WGRDBESstockCoord/personal/Damian/2025/model_length_allocation_compare.R")
setwd(here("WGRDBESstockCoord", "personal", "Damian", "2025"))

mkdir("model")
mkdir("output")
mkdir("output/3c_length_allocation_comparison")

## Every figure of this script is written to output/figures/3c_length_allocation_comparison (the folders of output/figures are
## numbered in the order the scripts make them), numbered in the order it is drawn. What an earlier run left there goes first.
## A figure the report shows has a name as well - fig("name") - and the report finds it by the name, not by the number.
fig <- local({
  dir <- "output/figures/3c_length_allocation_comparison"
  unlink(dir, recursive = TRUE)
  mkdir(dir)
  n <- 0
  function(name = NULL) sprintf("%s/length_allocation_comparison_%02d%s.png", dir, n <<- n + 1, if (is.null(name)) "" else paste0("_", name))
})

## The two plans, each with the two tables of its allocation: the catch and the distributions
allocated <- list(
  "Usual method" = list(catch = read.taf(file = "model/catch_data_raised.csv"),
                        distribution = read.taf(file = "model/distributions_data_allocated.csv")),
  "Hierarchical groups" = list(catch = read.taf(file = "model/catch_data_allocated_hierarchical.csv"),
                               distribution = read.taf(file = "model/distributions_data_allocated_hierarchical.csv")))
plans <- names(allocated)

stopifnot("the catch of a plan is not the catch of an allocation (no allocGroup): run the allocation scripts first" =
            all(vapply(allocated, function(a) "allocGroup" %in% names(a$catch), NA)))

### The same colours as the comparison of the raisings (model_discard_raising_compare.R)
plan_colours <- plot_colours(factor(plans, plans), "scenario")

### The landings before the discards, with their names, in every panel (the BMS last, in a panel of its own when there is any)
cat_labels <- c(Lan = "Landings", Dis = "Discards", BMS = "BMS")

## ###########################################################################
## Numbers and mean weight at length, added up the same way for both plans
## ###########################################################################

### catchAtAoL() adds the numbers at length up over whatever is named in `grouping`, meanWoLatAoL() the mean weight at length; the
### same call on the allocation of each plan. The lengths are the classes of the CEF, in mm.
at_length <- function(grouping = "catchCategory", what = c("numbers", "mean weight")) {
  what <- match.arg(what)
  bind_rows(lapply(plans, function(p) {
    a <- allocated[[p]]
    d <- if (what == "numbers")
      catchAtAoL(a$distribution, a$catch, grouping = grouping,
                 originType_catch = "WGEstimate", variableType_catch = "WeightLive",
                 distributionType = "LengthTotal", plusGroup = FALSE)
    else
      meanWoLatAoL(a$distribution, a$catch, grouping = grouping,
                   originType_catch = "WGEstimate", variableType_catch = "WeightLive",
                   distributionType = "LengthTotal", weighting = "NumberAtAoL", plusGroup = FALSE)
    d %>% mutate(plan = p)
  })) %>%
    mutate(plan = factor(plan, plans),
           catchCategory = factor(catchCategory, names(cat_labels)),
           length_mm = distributionClass)            # the classes of the CEF are in mm, and so is every length of the scripts
}

numbers <- at_length("catchCategory", "numbers")
weights <- at_length("catchCategory", "mean weight")

### The totals: numbers (thousands of fish) over all lengths, for each plan and catch category.
totals <- numbers %>%
  group_by(plan, catchCategory) %>%
  summarise(numbers_thousands = sum(value, na.rm = TRUE) / 1000, .groups = "drop") %>%
  pivot_wider(names_from = plan, values_from = numbers_thousands) %>%
  mutate(difference = .data[[plans[2]]] - .data[[plans[1]]],
         percent = 100 * difference / .data[[plans[1]]])
totals

write.taf(totals, file = "numbers_total_by_plan.csv", dir = "output/3c_length_allocation_comparison")

### Numbers at length of the landings and of the discards, one line for each plan.
ggsave(fig("numbers"),
       ggplot(numbers, aes(x = length_mm, y = value / 1000, colour = plan)) +
         geom_line(linewidth = 0.8) +
         scale_colour_manual(values = plan_colours, name = NULL) +
         facet_wrap(~catchCategory, ncol = 1, scales = "free_y", labeller = labeller(catchCategory = cat_labels)) +
         labs(title = "Numbers at length: the two allocation plans",
              x = "Length (mm)", y = "Numbers (thousands)") +
         app_theme(),
       width = 10, height = 7, dpi = 150)

### Mean weight at length, the same way.
ggsave(fig("mean_weight"),
       ggplot(weights, aes(x = length_mm, y = value, colour = plan)) +
         geom_line(linewidth = 0.8) +
         scale_colour_manual(values = plan_colours, name = NULL) +
         facet_wrap(~catchCategory, ncol = 1, scales = "free_y", labeller = labeller(catchCategory = cat_labels)) +
         labs(title = "Mean weight at length: the two allocation plans",
              x = "Length (mm)", y = "Mean weight (g)") +
         app_theme(),
       width = 10, height = 7, dpi = 150)

### The difference in numbers at length, hierarchical against usual: where the two plans give a different length distribution.
ggsave(fig("difference"),
       numbers %>%
         select(plan, catchCategory, length_mm, value) %>%
         pivot_wider(names_from = plan, values_from = value, values_fill = 0) %>%
         mutate(difference = .data[[plans[2]]] - .data[[plans[1]]]) %>%
         ggplot(aes(x = length_mm, y = difference / 1000)) +
         geom_col(fill = plan_colours[[plans[2]]], width = 8) +
         geom_hline(yintercept = 0, colour = "grey30") +
         facet_wrap(~catchCategory, ncol = 1, scales = "free_y", labeller = labeller(catchCategory = cat_labels)) +
         labs(title = "Numbers at length: hierarchical groups minus usual method",
              x = "Length (mm)", y = "Difference in numbers (thousands)") +
         app_theme(),
       width = 10, height = 7, dpi = 150)

## ###########################################################################
## By metier: where the plans differ
## ###########################################################################

### The same numbers at length for every metier: the landings, and the discards, where the groups differ most.
numbers_metier <- at_length(c("catchCategory", "metier4"), "numbers")

write.taf(numbers_metier %>% select(plan, catchCategory, metier4, distributionClass, value),
          file = "numbers_at_length_by_metier.csv", dir = "output/3c_length_allocation_comparison")

invisible(lapply(c("Lan", "Dis"), function(cc)
  ggsave(fig(),
         ggplot(numbers_metier %>% filter(catchCategory == cc), aes(x = length_mm, y = value / 1000, colour = plan)) +
           geom_line(linewidth = 0.8) +
           scale_colour_manual(values = plan_colours, name = NULL) +
           facet_wrap(~metier4, scales = "free_y") +
           labs(title = sprintf("Numbers at length of the %s by metier: the two allocation plans",
                                if (cc == "Lan") "landings" else "discards"),
                x = "Length (mm)", y = "Numbers (thousands)") +
           app_theme(),
         width = 11, height = 8, dpi = 150)))
