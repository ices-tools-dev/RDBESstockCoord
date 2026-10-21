################################################################################
### Comparison of two CEF data sets after discards raising
### Same idea as the end of Damian/output.R (tables, one Excel workbook, figures),
### but the two sides are two runs of sole8ab_discards_raising.R, i.e. two versions of
### df_stock_overview_L_D_raised (one row per stratum and catch category, Catchkg in kg)
################################################################################

library("tidyverse")
library("here")
library("glue")
library("icesTAF")

### Settings
data_year <- 2025
path_to_output <- here("WGRDBESstockCoord/personal/JB/output")

### The two data sets, saved by sole8ab_discards_raising.R. Change the files and the labels to compare
### other runs (e.g. raised from the IC conversion against raised from the RDBES download).
file_cef_rdbes  <- glue("{path_to_output}/{data_year}/cef_rdbes/df_stock_overview_L_D_raised{data_year}.rds")
file_cef_ic  <- glue("{path_to_output}/{data_year}/cef_ic/df_stock_overview_L_D_raised{data_year}.rds")
label_cef_rdbes <- "CEF-RDBES"
label_cef_ic <- "CEF-IC"

### Two catches are the same when they differ by less than this (kg) or by less than this share (%)
tol_kg  <- 1
tol_pct <- 0.1

compare_dir <- file.path(path_to_output, glue("cef_raised_{label_cef_rdbes}_vs_{label_cef_ic}") |> str_replace_all(" ", "_"), data_year)
fig_dir <- file.path(compare_dir, "figures")
unlink(compare_dir, recursive = TRUE)
mkdir(file.path(compare_dir, "tables"))
mkdir(fig_dir)

################################################################################
### Read and put the two data sets on the same footing
################################################################################
stratum <- c("year", "vesselFlagCountry", "catchCategory", "seasonType", "seasonValue", "areaValue", "fleetValue")

### RDBES codes the countries (FR), the IC conversion names them (France): both are put on the code
### (same table as comparison_cef_ic_rdbes_input_data.R)
country_code <- c(Belgium = "BE", Denmark = "DK", Estonia = "EE", Finland = "FI", France = "FR", Germany = "DE",
                  Ireland = "IE", Latvia = "LV", Lithuania = "LT", Netherlands = "NL", Norway = "NO", Poland = "PL",
                  Portugal = "PT", Spain = "ES", Sweden = "SE", "UK (England)" = "GB-ENG", "UK(England)" = "GB-ENG",
                  "UK(Northern Ireland)" = "GB-NIR", "UK(Scotland)" = "GB-SCT", "UK(Wales)" = "GB-WLS")
as_iso <- function(x) unname(if_else(x %in% names(country_code), country_code[x], x))

read_cef_raised <- function(file) {
  readRDS(file) |>
    mutate(vesselFlagCountry = as_iso(vesselFlagCountry),
           seasonValue = as.character(seasonValue),
           fleetValue = as.character(fleetValue),
           catchCategory = as.character(catchCategory)) |>
    summarise(Catchkg = sum(Catchkg, na.rm = TRUE),
              n_records = n(),
              importedOrRaised = str_flatten(sort(unique(na.omit(importedOrRaised))), "/"),
              raising_strata = str_flatten(sort(unique(na.omit(strata))), "/"),
              .by = all_of(stratum))
}

cef_rdbes <- read_cef_raised(file_cef_rdbes)
cef_ic <- read_cef_raised(file_cef_ic)

### Rows of a stratum that are duplicated would be summed: tell if it happens
walk2(list(cef_rdbes, cef_ic), c(label_cef_rdbes, label_cef_ic),
      \(x, nm) if (any(x$n_records > 1))
        warning(nm, ": ", sum(x$n_records > 1), " strata have several records, their Catchkg is summed", call. = FALSE))

################################################################################
### Strata by strata comparison
################################################################################
cef_rdbes$year <- as.character(cef_rdbes$year)
Compare_strata <-
  full_join(cef_rdbes |> rename_with(\(x) str_c(x, "_rdbes"), c(Catchkg, n_records, importedOrRaised, raising_strata)),
            cef_ic |> rename_with(\(x) str_c(x, "_ic"), c(Catchkg, n_records, importedOrRaised, raising_strata)),
            by = stratum) |>
  mutate(Catchkg_rdbes = replace_na(Catchkg_rdbes, 0),
         Catchkg_ic = replace_na(Catchkg_ic, 0),
         difference = Catchkg_ic - Catchkg_rdbes,
         ratio = Catchkg_ic / Catchkg_rdbes,
         status = case_when(is.na(n_records_rdbes) ~ glue("only {label_cef_ic}"),
                            is.na(n_records_ic) ~ glue("only {label_cef_rdbes}"),
                            abs(difference) < tol_kg | abs(difference) < tol_pct / 100 * pmax(Catchkg_rdbes, Catchkg_ic) ~ "same",
                            .default = "differs")) |>
  arrange(catchCategory, vesselFlagCountry, seasonValue, areaValue, fleetValue)

### Totals, by catch category
Compare_category <-
  Compare_strata |>
  summarise(strata = n(),
            strata_differ = sum(status != "same"),
            Catchkg_rdbes = sum(Catchkg_rdbes),
            Catchkg_ic = sum(Catchkg_ic),
            .by = catchCategory) |>
  bind_rows(summarise(Compare_strata, catchCategory = "All",
                      strata = n(), strata_differ = sum(status != "same"),
                      Catchkg_rdbes = sum(Catchkg_rdbes), Catchkg_ic = sum(Catchkg_ic))) |>
  mutate(difference = Catchkg_ic - Catchkg_rdbes,
         ratio = Catchkg_ic / Catchkg_rdbes)

### By country, catch category: where the difference sits
Compare_country <-
  Compare_strata |>
  summarise(strata = n(),
            strata_differ = sum(status != "same"),
            Catchkg_rdbes = sum(Catchkg_rdbes),
            Catchkg_ic = sum(Catchkg_ic),
            .by = c(vesselFlagCountry, catchCategory)) |>
  mutate(difference = Catchkg_ic - Catchkg_rdbes,
         ratio = Catchkg_ic / Catchkg_rdbes) |>
  arrange(catchCategory, desc(abs(difference)))

### By how the discards were obtained in A and in B (imported from the data, or raised, and which raising stratum)
Compare_summary <-
  Compare_strata |>
  filter(catchCategory == "Dis") |>
  summarise(strata = n(),
            strata_differ = sum(status != "same"),
            Catchkg_rdbes = sum(Catchkg_rdbes),
            Catchkg_ic = sum(Catchkg_ic),
            .by = c(importedOrRaised_rdbes, importedOrRaised_ic)) |>
  mutate(difference = Catchkg_ic - Catchkg_rdbes,
         ratio = Catchkg_ic / Catchkg_rdbes)

Compare_category
Compare_summary
count(Compare_strata, catchCategory, status)

compare_tables <- list(category = Compare_category, country = Compare_country,
                       summary_discards = Compare_summary, strata = Compare_strata)

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
  tryCatch(openxlsx::saveWorkbook(wb, file.path(compare_dir, "cef_raised_comparison.xlsx"), overwrite = TRUE),
           error = \(e) warning("could not write the workbook: ", conditionMessage(e), call. = FALSE))
} else message("install the package openxlsx to get the tables as one Excel workbook")

################################################################################
### The figures of the comparison
################################################################################
src_lab <- c(ic = label_cef_ic, rdbes = label_cef_rdbes)     # IC first, RDBES second
src_col <- set_names(c("#D55E00", "#4D4D4D"), src_lab)
category_name <- c(Lan = "Landings", Dis = "Discards", BMS = "BMS landings")
cmp_caption <- glue("{label_cef_ic}: {basename(file_cef_ic)}\n{label_cef_rdbes}: {basename(file_cef_rdbes)}")
save_cmp <- function(p, name, width = 9, height = 5.5)
  ggsave(file.path(fig_dir, paste0(name, ".png")), p, width = width, height = height, dpi = 150)

### 1. The totals by catch category
cmp_total <- Compare_category |>
  filter(catchCategory != "All") |>
  pivot_longer(c(Catchkg_rdbes, Catchkg_ic), names_to = "source", values_to = "tonnes", names_prefix = "Catchkg_") |>
  mutate(tonnes = tonnes / 1000,
         source = factor(src_lab[source], src_lab),
         catchCategory = factor(category_name[catchCategory], category_name))
plot_totals <- ggplot(cmp_total, aes(source, tonnes, fill = source)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = format(round(tonnes, 1), big.mark = ",")), vjust = -0.5, size = 3.4) +
  facet_wrap(vars(catchCategory), scales = "free_y") +
  scale_fill_manual(values = src_col) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12)), labels = scales::label_comma()) +
  labs(title = "Catch by category", x = NULL, y = "Tonnes", caption = cmp_caption) +
  theme_bw() + theme(legend.position = "none")
save_cmp(plot_totals, "01_totals", width = 10, height = 4.8)

### 2. Stratum by stratum: B against A, one panel per catch category (on the 1:1 line the two agree)
cmp_scatter <- Compare_strata |>
  filter(catchCategory %in% names(category_name), Catchkg_rdbes > 0 | Catchkg_ic > 0) |>
  mutate(catchCategory = factor(category_name[catchCategory], category_name))
plot_by_stratum <- ggplot(cmp_scatter, aes(Catchkg_ic, Catchkg_rdbes, colour = status)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey35") +
  geom_point(alpha = 0.7, size = 1.6) +
  facet_wrap(vars(catchCategory), scales = "free") +
  scale_x_continuous(labels = scales::label_comma()) + scale_y_continuous(labels = scales::label_comma()) +
  labs(title = "Stratum by stratum", x = glue("{label_cef_ic} (kg)"), y = glue("{label_cef_rdbes} (kg)"),
       colour = NULL, caption = cmp_caption) +
  theme_bw() + theme(legend.position = "top")
save_cmp(plot_by_stratum, "02_by_stratum", width = 11, height = 5)

### 3. The strata that differ most (kg)
largest <- Compare_strata |>
  filter(status != "same") |>
  mutate(label = paste(vesselFlagCountry, catchCategory, seasonValue, areaValue, fleetValue)) |>
  slice_max(abs(difference), n = 20, with_ties = FALSE) |>
  mutate(share = 100 * difference / Catchkg_rdbes) |>
  arrange(abs(difference))
plot_largest_differences <- NULL                # no plot when every stratum is the same
if (nrow(largest) > 0) {
  largest$label <- factor(largest$label, largest$label)
  plot_largest_differences <- ggplot(largest, aes(difference, label)) +
    geom_col(fill = src_col[[label_cef_ic]], width = 0.7) +
    geom_text(aes(label = sprintf("%+.0f kg (%+.1f%%)", difference, share), hjust = ifelse(difference >= 0, -0.1, 1.1)), size = 3) +
    scale_x_continuous(expand = expansion(mult = 0.25), labels = scales::label_comma()) +
    labs(title = glue("The strata that differ most: {label_cef_ic} minus {label_cef_rdbes}"), x = "Difference (kg)", y = NULL, caption = cmp_caption) +
    theme_bw()
  save_cmp(plot_largest_differences, "03_strata_that_differ_most", width = 10, height = 7)
}

### 4. The discards, stratum by stratum, both sides side by side (as in sole8ab_discards_raising.R)
cmp_dis <- Compare_strata |>
  filter(catchCategory == "Dis", Catchkg_rdbes > 0 | Catchkg_ic > 0) |>
  mutate(strata_id = glue("{vesselFlagCountry};{areaValue};{seasonValue};{fleetValue}")) |>
  pivot_longer(c(Catchkg_rdbes, Catchkg_ic), names_to = "source", values_to = "Catchkg", names_prefix = "Catchkg_") |>
  mutate(source = factor(src_lab[source], src_lab))

plot_discards_by_stratum <- ggplot(cmp_dis, aes(strata_id, Catchkg, fill = source)) +
  geom_col(position = position_dodge2()) +
  scale_fill_manual(values = src_col, name = NULL) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(title = "Discards by stratum", x = NULL, y = "Catch (kg)", caption = cmp_caption) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4), legend.position = "top")
save_cmp(plot_discards_by_stratum, "04_discards_by_stratum", width = 14, height = 7)



################################################################################
### The figures by metier5 (instead of by stratum)
################################################################################
### metier5 (gear and target species, e.g. GNS_DEF) is derived from the fleet by rcef::derive_fields(); the catches are
### added up by metier5, so the two sources are compared on a handful of bars instead of on every stratum.
library(rcef)

cmp_metier5 <- Compare_strata |>
  filter(catchCategory %in% c("Lan", "Dis")) |>
  rcef::derive_fields() |>
  pivot_longer(c(Catchkg_rdbes, Catchkg_ic), names_to = "source", values_to = "Catchkg", names_prefix = "Catchkg_") |>
  mutate(source = factor(src_lab[source], src_lab),
         catchCategory = factor(category_name[catchCategory], category_name[c("Lan", "Dis")]))

### 5. The catch by metier5, landings and discards, both sources side by side
cmp_metier5_total <- cmp_metier5 |>
  summarise(tonnes = sum(Catchkg) / 1000, .by = c(catchCategory, metier5, source))
plot_catch_by_metier5 <- ggplot(cmp_metier5_total, aes(metier5, tonnes, fill = source)) +
  geom_col(position = position_dodge2()) +
  facet_wrap(vars(catchCategory), ncol = 1, scales = "free_y") +
  scale_fill_manual(values = src_col, name = NULL) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(title = "Catch by metier5", x = NULL, y = "Tonnes", caption = cmp_caption) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4), legend.position = "top")
save_cmp(plot_catch_by_metier5, "05_catch_by_metier5", width = 10, height = 8)

### 6. The discards by metier5 and quarter
plot_discards_by_metier5_quarter <- cmp_metier5 |>
  filter(catchCategory == "Discards", seasonType == "Quarter") |>
  summarise(Catchkg = sum(Catchkg), .by = c(metier5, seasonValue, source)) |>
  ggplot(aes(metier5, Catchkg, fill = source)) +
  geom_col(position = position_dodge2()) +
  facet_wrap(vars(paste0("Q", seasonValue))) +
  scale_fill_manual(values = src_col, name = NULL) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(title = "Discards by metier5 and quarter", x = NULL, y = "Catch (kg)", caption = cmp_caption) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4), legend.position = "top")
save_cmp(plot_discards_by_metier5_quarter, "06_discards_by_metier5_quarter", width = 11, height = 7)

### 7. The discards by metier5 and country
plot_discards_by_metier5_country <- cmp_metier5 |>
  filter(catchCategory == "Discards") |>
  summarise(Catchkg = sum(Catchkg), .by = c(metier5, vesselFlagCountry, source)) |>
  ggplot(aes(metier5, Catchkg, fill = source)) +
  geom_col(position = position_dodge2()) +
  facet_wrap(vars(vesselFlagCountry), scales = "free_y") +
  scale_fill_manual(values = src_col, name = NULL) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(title = "Discards by metier5 and country", x = NULL, y = "Catch (kg)", caption = cmp_caption) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4), legend.position = "top")
save_cmp(plot_discards_by_metier5_country, "07_discards_by_metier5_country", width = 11, height = 7)

### 8. The ratio of discards to landings by metier5 (what the raising is meant to get right)
plot_discard_rate_by_metier5 <- cmp_metier5_total |>
  pivot_wider(names_from = catchCategory, values_from = tonnes, values_fill = 0) |>
  filter(Landings > 0) |>
  mutate(discard_rate = 100 * Discards / Landings) |>
  ggplot(aes(metier5, discard_rate, fill = source)) +
  geom_col(position = position_dodge2()) +
  scale_fill_manual(values = src_col, name = NULL) +
  labs(title = "Discards over landings by metier5", x = NULL, y = "Discards / landings (%)", caption = cmp_caption) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4), legend.position = "top")
save_cmp(plot_discard_rate_by_metier5, "08_discard_rate_by_metier5", width = 9, height = 5.5)

### The plots in one list (named as the figure files), saved as an rds file
list_plots <- list(
  `01_totals` = plot_totals,
  `02_by_stratum` = plot_by_stratum,
  `03_strata_that_differ_most` = plot_largest_differences,
  `04_discards_by_stratum` = plot_discards_by_stratum,
  `05_catch_by_metier5` = plot_catch_by_metier5,
  `06_discards_by_metier5_quarter` = plot_discards_by_metier5_quarter,
  `07_discards_by_metier5_country` = plot_discards_by_metier5_country,
  `08_discard_rate_by_metier5` = plot_discard_rate_by_metier5) |>
  compact()
saveRDS(list_plots, file = file.path(compare_dir, "list_plots.rds"))

message(length(list.files(fig_dir)), " figures in ", fig_dir)
