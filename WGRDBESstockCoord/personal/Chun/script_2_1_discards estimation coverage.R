library(dplyr)
library(tidyr)
library(ggplot2)

discard_coverage <- catch_data %>%
  filter(catchCategory == "Dis",
         originType == "WGEstimate",
         variableType == "WeightLive",
         !is.na(total),
         total > 0) %>%
  distinct(Fleetgroup, Season) %>%
  mutate(has_discards = TRUE)

coverage_matrix <- catch_data %>%
  distinct(Fleetgroup, Season) %>%
  left_join(discard_coverage, by = c("Fleetgroup", "Season")) %>%
  mutate(has_discards = coalesce(has_discards, FALSE)) %>%
  arrange(Fleetgroup, Season)

coverage_matrix %>%
  pivot_wider(names_from = Season,
              values_from = has_discards,
              values_fill = FALSE)


coverage_matrix %>%
  group_by(Fleetgroup) %>%
  summarise(
    n_seasons_with_dis  = sum(has_discards),
    n_seasons_total     = n(),
    seasons_with_dis    = paste(Season[has_discards], collapse = ", "),
    seasons_without_dis = paste(Season[!has_discards], collapse = ", "),
    .groups = "drop"
  ) %>%
  mutate(
    raising_strategy = case_when(
      n_seasons_with_dis == n_seasons_total ~ "raise per season",
      n_seasons_with_dis > 0               ~ "partial: borrow season",
      n_seasons_with_dis == 0              ~ "no data: borrow fleet group"
    )
  ) %>%
  arrange(raising_strategy, Fleetgroup)



## items privided discards ----

coverage_detail <- catch_data %>%
  filter(catchCategory == "Dis",
         originType == "WGEstimate",
         variableType == "WeightLive",
         !is.na(total),
         total > 0) %>%
  #mutate(country_domain = paste(vesselFlagCountry, domainCatchDis, sep = "_")) %>%
  group_by(Fleetgroup, Season) %>%
  summarise(n_strata_with_dis = n_distinct(domainCatchDis_ctr), .groups = "drop")

plot_data <- catch_data %>%
  distinct(Fleetgroup, Season) %>%
  left_join(coverage_detail, by = c("Fleetgroup", "Season")) %>%
  mutate(
    n_strata_with_dis = coalesce(n_strata_with_dis, 0L),
    Season = factor(Season, levels = c(1, 2, 3, 4, 2025))
  )

ggplot(plot_data, aes(x = Season, y = Fleetgroup, fill = n_strata_with_dis)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = n_strata_with_dis), color = "white", size = 4) +
  scale_fill_gradient(low = "#d73027", high = "#1a9850",
                      name = "strata with\ndiscards") +
  labs(x = "Season", y = NULL,
       title = "Discard coverage by fleetgroup and season",
       subtitle = "Cell value = number of country+domains with observed discards; missing cells mean no catch data reported, no need to raise discards.") +
  theme_minimal(base_size = 13) +
  theme(panel.grid = element_blank(),
        plot.title.position = "plot")


## items that need estimation (landings without a discard domain) ---
needs_estimation <- catch_data %>%
  filter(catchCategory == "Lan",
         originType == "WGEstimate",
         variableType == "WeightLive",
         is.na(domainCatchDis)) %>%
  mutate(country_domain = paste(vesselFlagCountry, seasonValue, areaValue, fleetValue, sep = "_")) %>%
  group_by(Fleetgroup, Season) %>%
  summarise(n_strata_no_dis = n_distinct(country_domain), .groups = "drop")

needs_estimation <- needs_estimation %>%
  mutate(Season = factor(Season, levels = c(1, 2, 3, 4, 2025)))

plot_data <- plot_data %>%
  mutate(Season = factor(Season, levels = c(1, 2, 3, 4, 2025)))

all_combinations <- catch_data %>%
  distinct(Fleetgroup, Season) %>%
  mutate(Season = factor(Season, levels = c(1, 2, 3, 4, 2025)),
         has_landings = TRUE)

plot_data_needs <- expand.grid(
  Fleetgroup = unique(catch_data$Fleetgroup),
  Season     = factor(c(1, 2, 3, 4, 2025), levels = c(1, 2, 3, 4, 2025))
) %>%
  left_join(needs_estimation, by = c("Fleetgroup", "Season")) %>%
  left_join(all_combinations, by = c("Fleetgroup", "Season")) %>%
  mutate(
    label = case_when(
      is.na(has_landings)    ~ "–",
      is.na(n_strata_no_dis) ~ "0",
      TRUE                   ~ as.character(n_strata_no_dis)
    ),
    n_strata_no_dis = case_when(
      is.na(has_landings) ~ NA_integer_,   # keep NA so na.value = grey kicks in
      TRUE                ~ coalesce(n_strata_no_dis, 0L)
    )
  )

ggplot(plot_data_needs %>% filter(!is.na(has_landings)),
       aes(x = Season, y = Fleetgroup, fill = n_strata_no_dis)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = label), color = "white", size = 4) +
  scale_fill_gradient(low = "#1a9850", high = "#d73027",
                      name = "strata\nneeding\nestimation") +
  labs(x = "Season", y = NULL,
       title = "Strata requiring discard estimation",
       subtitle = "Cell value = landing strata (country × domain) with no discard domain.") +
  theme_minimal(base_size = 13) +
  theme(panel.grid = element_blank(),
        plot.title.position = "plot")

## combined ----

combined_plot_data <- expand.grid(
  Fleetgroup = unique(catch_data$Fleetgroup),
  Season     = factor(c(1, 2, 3, 4, 2025), levels = c(1, 2, 3, 4, 2025))
) %>%
  left_join(all_combinations, by = c("Fleetgroup", "Season")) %>%
  filter(!is.na(has_landings)) %>%
  left_join(needs_estimation  %>% mutate(Season = factor(Season, levels = c(1,2,3,4,2025))),
            by = c("Fleetgroup", "Season")) %>%
  left_join(coverage_detail %>% mutate(Season = factor(Season, levels = c(1,2,3,4,2025))),
            by = c("Fleetgroup", "Season")) %>%
  mutate(
    n1 = coalesce(n_strata_no_dis, 0L),
    n2 = coalesce(n_strata_with_dis, 0L),
    label = paste0(n1, " / ", n2),
    status = case_when(
      n1 == 0              ~ "no estimation needed",
      n1 > 0 & n2 == 0    ~ "needs borrowing",
      n1 > 0 & n2 == 1    ~ "sparse discards",
      n1 > 0 & n2 > 1     ~ "ok to estimate"
    ),
    status = factor(status, levels = c("no estimation needed",
                                       "ok to estimate",
                                       "sparse discards",
                                       "needs borrowing"))
  )

status_colors <- c(
  "no estimation needed" = "grey80",
  "ok to estimate"       = "#1a9850",
  "sparse discards"      = "#fd8d3c",
  "needs borrowing"      = "#d73027"
)

ggplot(combined_plot_data, aes(x = Season, y = Fleetgroup, fill = status)) +
  geom_tile(color = "white", linewidth = 0.8) +
  geom_text(aes(label = label), color = "white", size = 3.5) +
  scale_fill_manual(values = status_colors, name = NULL) +
  labs(x = "Season", y = NULL,
       title = "Discard estimation coverage by fleetgroup and season",
       subtitle = "Cell value = strata needing estimation (n1) / strata with observed discards (n2)") +
  theme_minimal(base_size = 13) +
  theme(panel.grid = element_blank(),
        plot.title.position = "plot",
        legend.position = "bottom")
