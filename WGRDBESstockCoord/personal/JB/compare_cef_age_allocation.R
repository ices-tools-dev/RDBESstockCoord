################################################################################
### Comparison of the age allocation of two CEF data sets
### sole8ab_cef_rdbes_age_allocation.R (distributions from the RDBES download)
### against sole8ab_cef_ic_age_allocation.R (distributions from the IC conversion).
### Same layout as compare_cef_raised_datasets.R: tables, one Excel workbook, figures.
###
### What is compared is df_canum_all_age_{year}.rds, saved by both scripts: a row per stratum (country, season, area,
### fleet), catch category and age, with
###   - the age distribution of the stratum imported from the data (numbers: Number_Total_SopR, mean weight:
###     WeightLive_Mean), or
###   - the age distribution allocated to a stratum with no age data (numbers, weights), `Source` says with which
###     rule (strata_c_q_g, strata_q_g ...).
### Both are put in the same columns (numbers, mean weight, source) before the comparison.
################################################################################

library("tidyverse")
library("here")
library("glue")
library("icesTAF")
library("rcef")

### Settings
data_year <- 2025
max_agestock <- 8                 # the plus group, as in the age allocation scripts
path_to_output <- here("WGRDBESstockCoord/personal/JB/output")

### The age allocation of the two scripts. The IC script still saves in output/ (no sub folder): it is used
### when the file is not in output/{year}/cef_ic/ yet.
find_age_file <- function(folder, file) {
  path <- glue("{path_to_output}/{data_year}/{folder}/{file}")
  if (file.exists(path)) return(path)
  fallback <- glue("{path_to_output}/{file}")
  if (folder == "cef_ic" && file.exists(fallback)) {
    message(file, " not in ", dirname(path), ": using ", fallback)
    return(fallback)
  }
  stop("cannot find ", path, call. = FALSE)
}

file_cef_rdbes <- find_age_file("cef_rdbes", glue("df_canum_all_age_{data_year}.rds"))
file_cef_ic <- find_age_file("cef_ic", glue("df_canum_all_age_{data_year}.rds"))
label_cef_rdbes <- "CEF-RDBES"
label_cef_ic <- "CEF-IC"

### Two amounts are the same when they differ by less than this (fish) or by less than this share (%)
tol_fish <- 1
tol_pct <- 0.1

compare_dir <- file.path(path_to_output,
                         glue("cef_age_{label_cef_rdbes}_vs_{label_cef_ic}") |> str_replace_all(" ", "_"),
                         data_year)
fig_dir <- file.path(compare_dir, "figures")
unlink(compare_dir, recursive = TRUE)
mkdir(file.path(compare_dir, "tables"))
mkdir(fig_dir)

################################################################################
### Read and put the two data sets on the same footing
################################################################################
stratum <- c("year", "vesselFlagCountry", "catchCategory", "seasonValue", "areaValue", "fleetValue")

### RDBES codes the countries (FR), the IC conversion names them (France): both are put on the code
country_code <- c(Belgium = "BE", Denmark = "DK", Estonia = "EE", Finland = "FI", France = "FR", Germany = "DE",
                  Ireland = "IE", Latvia = "LV", Lithuania = "LT", Netherlands = "NL", Norway = "NO", Poland = "PL",
                  Portugal = "PT", Spain = "ES", Sweden = "SE", "UK (England)" = "GB-ENG", "UK(England)" = "GB-ENG",
                  "UK(Northern Ireland)" = "GB-NIR", "UK(Scotland)" = "GB-SCT", "UK(Wales)" = "GB-WLS")
as_iso <- function(x) unname(if_else(x %in% names(country_code), country_code[x], x))

### One row per stratum and age (the ages from max_agestock on are the plus group). numbers are fish; sop_kg is the sum
### of the numbers x the mean weights (g -> kg), Caton the catch of the stratum (kg)
read_cef_age <- function(file) {
  readRDS(file) |>
    ungroup() |>
    mutate(vesselFlagCountry = as_iso(as.character(vesselFlagCountry)),
           year = as.character(year),
           seasonValue = as.character(seasonValue),
           fleetValue = as.character(fleetValue),
           catchCategory = as.character(catchCategory),
           Age = pmin(as.numeric(Age), max_agestock),
           numbers_age = coalesce(numbers, Number_Total_SopR),         # allocated, or imported with the SOP correction
           weight_age = coalesce(weights, WeightLive_Mean),            # g
           source = coalesce(as.character(Source), "imported")) |>
    summarise(numbers = sum(numbers_age, na.rm = TRUE),
              sop_kg = sum(numbers_age * weight_age, na.rm = TRUE) / 1000,
              Caton = first(na.omit(Caton)),
              source = str_flatten(sort(unique(source)), "/"),
              .by = all_of(c(stratum, "Age")))
}

cef_rdbes <- read_cef_age(file_cef_rdbes)
cef_ic <- read_cef_age(file_cef_ic)

################################################################################
### Strata and age by strata and age
################################################################################
side_cols <- c("numbers", "sop_kg", "Caton", "source")

Compare_strata_age <-
  full_join(cef_rdbes |> rename_with(\(x) str_c(x, "_rdbes"), all_of(side_cols)),
            cef_ic |> rename_with(\(x) str_c(x, "_ic"), all_of(side_cols)),
            by = c(stratum, "Age")) |>
  mutate(numbers_rdbes = replace_na(numbers_rdbes, 0),
         numbers_ic = replace_na(numbers_ic, 0),
         numbers_difference = numbers_ic - numbers_rdbes,
         numbers_ratio = numbers_ic / numbers_rdbes) |>
  arrange(catchCategory, vesselFlagCountry, seasonValue, areaValue, fleetValue, Age)

### Added up over the ages, a row for each stratum: the numbers, the catch and the sum of products of the numbers and
### the mean weights over the catch (1 after the SOP correction of the age allocation scripts)
Compare_strata <-
  Compare_strata_age |>
  summarise(source_rdbes = str_flatten(sort(unique(na.omit(source_rdbes))), "/"),
            source_ic = str_flatten(sort(unique(na.omit(source_ic))), "/"),
            Caton_rdbes = first(na.omit(Caton_rdbes)),
            Caton_ic = first(na.omit(Caton_ic)),
            numbers_rdbes = sum(numbers_rdbes),
            numbers_ic = sum(numbers_ic),
            sop_kg_rdbes = sum(sop_kg_rdbes, na.rm = TRUE),
            sop_kg_ic = sum(sop_kg_ic, na.rm = TRUE),
            .by = all_of(stratum)) |>
  mutate(across(c(source_rdbes, source_ic), \(x) na_if(x, "")),
         numbers_difference = numbers_ic - numbers_rdbes,
         numbers_ratio = numbers_ic / numbers_rdbes,
         sop_over_caton_rdbes = sop_kg_rdbes / Caton_rdbes,
         sop_over_caton_ic = sop_kg_ic / Caton_ic,
         status = case_when(numbers_rdbes == 0 & numbers_ic > 0 ~ glue("only {label_cef_ic}"),
                            numbers_ic == 0 & numbers_rdbes > 0 ~ glue("only {label_cef_rdbes}"),
                            abs(numbers_difference) < tol_fish |
                              abs(numbers_difference) < tol_pct / 100 * pmax(numbers_rdbes, numbers_ic) ~ "same",
                            .default = "differs")) |>
  arrange(catchCategory, vesselFlagCountry, seasonValue, areaValue, fleetValue)

### Numbers and mean weights at age, by catch category (the strata of both sides added up)
Compare_age <-
  Compare_strata_age |>
  summarise(across(c(numbers_rdbes, numbers_ic, sop_kg_rdbes, sop_kg_ic), \(x) sum(x, na.rm = TRUE)),
            .by = c(catchCategory, Age)) |>
  mutate(weca_rdbes = 1000 * sop_kg_rdbes / numbers_rdbes,         # g
         weca_ic = 1000 * sop_kg_ic / numbers_ic,
         numbers_difference = numbers_ic - numbers_rdbes,
         numbers_ratio = numbers_ic / numbers_rdbes,
         weca_ratio = weca_ic / weca_rdbes) |>
  arrange(catchCategory, Age)

### The totals, by catch category
Compare_total <-
  Compare_strata |>
  summarise(strata = n(),
            strata_differ = sum(status != "same"),
            across(c(numbers_rdbes, numbers_ic, sop_kg_rdbes, sop_kg_ic), \(x) sum(x, na.rm = TRUE)),
            .by = catchCategory) |>
  bind_rows(Compare_strata |>
              summarise(catchCategory = "All", strata = n(), strata_differ = sum(status != "same"),
                        across(c(numbers_rdbes, numbers_ic, sop_kg_rdbes, sop_kg_ic), \(x) sum(x, na.rm = TRUE)))) |>
  mutate(numbers_difference = numbers_ic - numbers_rdbes,
         numbers_ratio = numbers_ic / numbers_rdbes,
         sop_ratio = sop_kg_ic / sop_kg_rdbes)

### How the age distribution of a stratum was obtained (imported, or the allocation rule), in each of the two
Compare_source <-
  bind_rows(Compare_strata |> transmute(catchCategory, origin = label_cef_rdbes, source = source_rdbes, numbers = numbers_rdbes),
            Compare_strata |> transmute(catchCategory, origin = label_cef_ic, source = source_ic, numbers = numbers_ic)) |>
  filter(!is.na(source)) |>
  summarise(strata = n(), numbers = sum(numbers), .by = c(origin, catchCategory, source)) |>
  mutate(share_numbers = 100 * numbers / sum(numbers), .by = c(origin, catchCategory)) |>
  arrange(catchCategory, origin, desc(numbers))

Compare_total
Compare_age
Compare_source
count(Compare_strata, catchCategory, status)

compare_tables <- list(total = Compare_total, age = Compare_age, source = Compare_source,
                       strata = Compare_strata, strata_age = Compare_strata_age)

################################################################################
### The comparison, saved
################################################################################
iwalk(compare_tables,
      \(tab, nm) tryCatch(write.taf(tab, file = paste0(nm, ".csv"), dir = file.path(compare_dir, "tables"), quote = TRUE),
                          error = \(e) warning("could not write ", nm, ": ", conditionMessage(e), call. = FALSE)))

if (requireNamespace("openxlsx", quietly = TRUE)) {
  wb <- openxlsx::createWorkbook()
  iwalk(compare_tables, \(tab, nm) {
    openxlsx::addWorksheet(wb, nm)
    openxlsx::writeData(wb, nm, as.data.frame(tab), withFilter = TRUE)
    openxlsx::freezePane(wb, nm, firstRow = TRUE)
    openxlsx::setColWidths(wb, nm, cols = seq_along(tab), widths = "auto")
  })
  tryCatch(openxlsx::saveWorkbook(wb, file.path(compare_dir, "cef_age_comparison.xlsx"), overwrite = TRUE),
           error = \(e) warning("could not write the workbook: ", conditionMessage(e), call. = FALSE))
} else message("install the package openxlsx to get the tables as one Excel workbook")

################################################################################
### The figures of the comparison
################################################################################
src_lab <- c(ic = label_cef_ic, rdbes = label_cef_rdbes)     # IC first, RDBES second
src_col <- set_names(c("#D55E00", "#4D4D4D"), src_lab)
category_name <- c(Lan = "Landings", Dis = "Discards", BMS = "BMS landings")
cmp_caption <- glue("{label_cef_ic}: {basename(dirname(file_cef_ic))}/{basename(file_cef_ic)}\n",
                    "{label_cef_rdbes}: {basename(dirname(file_cef_rdbes))}/{basename(file_cef_rdbes)}")
save_cmp <- function(p, name, width = 9, height = 5.5)
  ggsave(file.path(fig_dir, paste0(name, ".png")), p, width = width, height = height, dpi = 150)
theme_cmp <- function() theme_bw() + theme(legend.position = "top")
theme_cmp_x90 <- function() theme_cmp() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))
### The labels are values, not a function of this script, so that the plots saved in list_plots.rds draw without it
age_label_plus <- set_names(paste0(max_agestock, "+"), max_agestock)           # discrete axis: only the plus group is renamed
age_label_all <- c(as.character(0:(max_agestock - 1)), paste0(max_agestock, "+"))   # continuous axis, breaks 0:max_agestock

### The two sides of a table with columns <what>_rdbes and <what>_ic, one under the other
two_sides <- function(data, cols, id_cols, values_to) {
  data |>
    pivot_longer(all_of(cols), names_to = "source", values_to = values_to, names_pattern = ".*_(rdbes|ic)$") |>
    mutate(source = factor(src_lab[source], src_lab)) |>
    select(all_of(id_cols), source, all_of(values_to))
}

### 1. The numbers at age of each catch category
cmp_age_numbers <- Compare_age |>
  filter(catchCategory %in% names(category_name)) |>
  two_sides(c("numbers_rdbes", "numbers_ic"), c("catchCategory", "Age"), "fish") |>
  mutate(fish = fish / 1000,
         catchCategory = factor(category_name[catchCategory], category_name))
plot_numbers_at_age <- ggplot(cmp_age_numbers, aes(factor(Age), fish, fill = source)) +
  geom_col(position = position_dodge2()) +
  facet_wrap(vars(catchCategory), ncol = 1, scales = "free_y") +
  scale_fill_manual(values = src_col, name = NULL) +
  scale_x_discrete(labels = age_label_plus) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(title = "Numbers at age", x = "Age", y = "Numbers (thousand fish)", caption = cmp_caption) +
  theme_cmp()
save_cmp(plot_numbers_at_age, "01_numbers_at_age", height = 7)

### 2. The mean weights at age of each catch category
cmp_age_weca <- Compare_age |>
  filter(catchCategory %in% names(category_name)) |>
  two_sides(c("weca_rdbes", "weca_ic"), c("catchCategory", "Age"), "weca") |>
  mutate(catchCategory = factor(category_name[catchCategory], category_name))
plot_mean_weight_at_age <- ggplot(cmp_age_weca, aes(Age, weca, colour = source)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.8) +
  facet_wrap(vars(catchCategory), ncol = 1, scales = "free_y") +
  scale_colour_manual(values = src_col, name = NULL) +
  scale_x_continuous(breaks = 0:max_agestock, labels = age_label_all) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(title = "Mean weight at age", x = "Age", y = "Mean weight (g)", caption = cmp_caption) +
  theme_cmp()
save_cmp(plot_mean_weight_at_age, "02_mean_weight_at_age", height = 7)

### 3. The numbers at age by country
cmp_country <- Compare_strata_age |>
  filter(catchCategory %in% names(category_name)) |>
  summarise(numbers_rdbes = sum(numbers_rdbes), numbers_ic = sum(numbers_ic),
            .by = c(vesselFlagCountry, catchCategory, Age)) |>
  two_sides(c("numbers_rdbes", "numbers_ic"), c("vesselFlagCountry", "catchCategory", "Age"), "fish") |>
  mutate(fish = fish / 1000, catchCategory = factor(category_name[catchCategory], category_name))
plot_numbers_at_age_by_country <- ggplot(cmp_country, aes(factor(Age), fish, fill = source)) +
  geom_col(position = position_dodge2()) +
  facet_grid(catchCategory ~ vesselFlagCountry, scales = "free_y") +
  scale_fill_manual(values = src_col, name = NULL) +
  scale_x_discrete(labels = age_label_plus) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(title = "Numbers at age by country", x = "Age", y = "Numbers (thousand fish)", caption = cmp_caption) +
  theme_cmp()
save_cmp(plot_numbers_at_age_by_country, "03_numbers_at_age_by_country", width = 11, height = 7)

### 4. Stratum by stratum: the numbers of CEF-IC against CEF-RDBES, one panel per catch category (on the 1:1 line the two agree)
cmp_scatter <- Compare_strata |>
  filter(catchCategory %in% names(category_name), numbers_rdbes > 0 | numbers_ic > 0) |>
  mutate(catchCategory = factor(category_name[catchCategory], category_name))
plot_numbers_by_stratum <- ggplot(cmp_scatter, aes(numbers_ic, numbers_rdbes, colour = status)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey35") +
  geom_point(alpha = 0.7, size = 1.6) +
  facet_wrap(vars(catchCategory), scales = "free") +
  scale_x_continuous(labels = scales::label_comma()) + scale_y_continuous(labels = scales::label_comma()) +
  labs(title = "Numbers, stratum by stratum", x = glue("{label_cef_ic} (fish)"), y = glue("{label_cef_rdbes} (fish)"),
       colour = NULL, caption = cmp_caption) +
  theme_cmp()
save_cmp(plot_numbers_by_stratum, "04_numbers_by_stratum", width = 11, height = 5)

### 5. How the age distribution was obtained: imported from the data, or allocated by which rule (share of the numbers)
source_levels <- Compare_source |> distinct(source) |> pull(source)
source_levels <- c(intersect("imported", source_levels), sort(setdiff(source_levels, "imported")))
cmp_source <- Compare_source |>
  filter(catchCategory %in% names(category_name)) |>
  mutate(source = factor(source, source_levels),
         origin = factor(origin, src_lab),
         catchCategory = factor(category_name[catchCategory], category_name))
plot_age_source <- ggplot(cmp_source, aes(origin, share_numbers, fill = source)) +
  geom_col(width = 0.7) +
  facet_wrap(vars(catchCategory)) +
  scale_fill_viridis_d(name = "How the age distribution\nwas obtained") +
  labs(title = "How the age distribution of the strata was obtained", x = NULL, y = "Share of the numbers (%)",
       caption = cmp_caption) +
  theme_bw() + theme(legend.position = "right")
save_cmp(plot_age_source, "05_how_age_was_obtained", width = 10, height = 5)

### 6. The check of the SOP, stratum by stratum: the sum of products of the numbers and the mean weights over the catch
### (1 for every stratum when the age allocation is right; the SCF rejects a total more than 20 % off)
cmp_sop <- bind_rows(
  Compare_strata |> filter(Caton_rdbes > 0) |> transmute(catchCategory, source = "rdbes", ratio = sop_over_caton_rdbes),
  Compare_strata |> filter(Caton_ic > 0) |> transmute(catchCategory, source = "ic", ratio = sop_over_caton_ic)) |>
  filter(catchCategory %in% c("Lan", "Dis")) |>
  mutate(source = factor(src_lab[source], src_lab),
         catchCategory = factor(category_name[catchCategory], category_name[c("Lan", "Dis")]),
         outside = ratio < 0.8 | ratio > 1.2)
n_outside <- cmp_sop |>
  summarise(label = sprintf("%d of %d strata\noutside the 20%% band", sum(outside), n()), .by = c(catchCategory, source))
plot_sop_over_catch <- ggplot(cmp_sop, aes(source, ratio, colour = source)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0.8, ymax = 1.2, fill = "grey85", alpha = 0.5) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey35") +
  geom_jitter(width = 0.25, height = 0, alpha = 0.6, size = 1.4) +
  geom_text(data = n_outside, aes(y = 1.28, label = label), colour = "grey20", size = 3, lineheight = 0.9, vjust = 0) +
  facet_wrap(vars(catchCategory)) +
  scale_colour_manual(values = src_col) +
  scale_y_continuous(expand = expansion(mult = c(0.03, 0.12))) +
  labs(title = "The check of the SOP, stratum by stratum", x = NULL, y = "Sum of numbers x mean weights / catch",
       caption = paste0(cmp_caption, "\nThe grey band is the 20 % the SCF accepts.")) +
  theme_cmp() + theme(legend.position = "none")
save_cmp(plot_sop_over_catch, "06_sop_over_catch", width = 9, height = 5.5)

### 7. The strata whose numbers differ most
largest <- Compare_strata |>
  filter(status != "same") |>
  mutate(label = paste(vesselFlagCountry, catchCategory, seasonValue, areaValue, fleetValue)) |>
  slice_max(abs(numbers_difference), n = 20, with_ties = FALSE) |>
  mutate(share = 100 * numbers_difference / numbers_rdbes) |>
  arrange(abs(numbers_difference))
plot_largest_differences <- NULL                # no plot when every stratum is the same
if (nrow(largest) > 0) {
  largest$label <- factor(largest$label, largest$label)
  plot_largest_differences <- ggplot(largest, aes(numbers_difference, label)) +
    geom_col(fill = src_col[[label_cef_ic]], width = 0.7) +
    geom_text(aes(label = sprintf("%+.0f (%+.1f%%)", numbers_difference, share),
                  hjust = ifelse(numbers_difference >= 0, -0.1, 1.1)), size = 3) +
    scale_x_continuous(expand = expansion(mult = 0.25), labels = scales::label_comma()) +
    labs(title = glue("The strata whose numbers differ most: {label_cef_ic} minus {label_cef_rdbes}"),
         x = "Difference (fish)", y = NULL, caption = cmp_caption) +
    theme_bw()
  save_cmp(plot_largest_differences, "07_strata_that_differ_most", width = 10, height = 7)
}

################################################################################
### The figures by metier5
################################################################################
### metier5 (gear and target species) is derived from the fleet by rcef::derive_fields()
cmp_metier5 <- Compare_strata |>
  filter(catchCategory %in% c("Lan", "Dis")) |>
  rcef::derive_fields() |>
  two_sides(c("numbers_rdbes", "numbers_ic"), c("catchCategory", "metier5", "vesselFlagCountry", "seasonValue"), "fish") |>
  mutate(catchCategory = factor(category_name[catchCategory], category_name[c("Lan", "Dis")]))

### 8. The numbers by metier5, landings and discards
plot_numbers_by_metier5 <- cmp_metier5 |>
  summarise(fish = sum(fish) / 1000, .by = c(catchCategory, metier5, source)) |>
  ggplot(aes(metier5, fish, fill = source)) +
  geom_col(position = position_dodge2()) +
  facet_wrap(vars(catchCategory), ncol = 1, scales = "free_y") +
  scale_fill_manual(values = src_col, name = NULL) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(title = "Numbers by metier5", x = NULL, y = "Numbers (thousand fish)", caption = cmp_caption) +
  theme_cmp_x90()
save_cmp(plot_numbers_by_metier5, "08_numbers_by_metier5", width = 10, height = 8)

### 9. The numbers by metier5 and country
plot_numbers_by_metier5_country <- cmp_metier5 |>
  summarise(fish = sum(fish) / 1000, .by = c(catchCategory, metier5, vesselFlagCountry, source)) |>
  ggplot(aes(metier5, fish, fill = source)) +
  geom_col(position = position_dodge2()) +
  facet_grid(catchCategory ~ vesselFlagCountry, scales = "free") +
  scale_fill_manual(values = src_col, name = NULL) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(title = "Numbers by metier5 and country", x = NULL, y = "Numbers (thousand fish)", caption = cmp_caption) +
  theme_cmp_x90()
save_cmp(plot_numbers_by_metier5_country, "09_numbers_by_metier5_country", width = 12, height = 8)

### 10. The numbers by metier5 and quarter
plot_numbers_by_metier5_quarter <- cmp_metier5 |>
  summarise(fish = sum(fish) / 1000, .by = c(catchCategory, metier5, seasonValue, source)) |>
  ggplot(aes(metier5, fish, fill = source)) +
  geom_col(position = position_dodge2()) +
  facet_grid(catchCategory ~ paste0("Q", seasonValue), scales = "free_y") +
  scale_fill_manual(values = src_col, name = NULL) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(title = "Numbers by metier5 and quarter", x = NULL, y = "Numbers (thousand fish)", caption = cmp_caption) +
  theme_cmp_x90()
save_cmp(plot_numbers_by_metier5_quarter, "10_numbers_by_metier5_quarter", width = 12, height = 8)

### The plots in one list (named as the figure files), saved as an rds file
list_plots <- list(
  `01_numbers_at_age` = plot_numbers_at_age,
  `02_mean_weight_at_age` = plot_mean_weight_at_age,
  `03_numbers_at_age_by_country` = plot_numbers_at_age_by_country,
  `04_numbers_by_stratum` = plot_numbers_by_stratum,
  `05_how_age_was_obtained` = plot_age_source,
  `06_sop_over_catch` = plot_sop_over_catch,
  `07_strata_that_differ_most` = plot_largest_differences,
  `08_numbers_by_metier5` = plot_numbers_by_metier5,
  `09_numbers_by_metier5_country` = plot_numbers_by_metier5_country,
  `10_numbers_by_metier5_quarter` = plot_numbers_by_metier5_quarter) |>
  compact()
saveRDS(list_plots, file = file.path(compare_dir, "list_plots.rds"))

message(length(list.files(fig_dir)), " figures in ", fig_dir)
