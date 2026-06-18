library("icesVocab")
library("icesSD")
library("tidyverse")
library("here")
library("glue")
library("data.table")

################################################################################
### From InterCatch to cef data format
################################################################################
source("R/funIntercatchCEF.R")
source("R/funICoutCEF.R")
source("R/funMakeRelation.R")

### needed functions for raising D
source("R/cef_add_strata.R")
source("R/split_L_with_without_D.R")
source("R/raise_discards_strata.R")
source("R/strata_coverage_summary.R")


path_to_data <- here("WGRDBESstockCoord/personal/JB/data")

data_year <- 2024

## Returned as a list:
list_cef <- funICoutCEF(dat_path = glue("{path_to_data}/"),
                        years = data_year,
                        metier6 = "Fleet",
                        output_format = "to_list",
                        out_path = dat_path,
                        keep_temp_file = TRUE,
                        file_prefix = "sol_8ab")

### Compute total landings for checking code after
list_cef_total_landings <- list_cef$catches %>%
  cef_add_strata(only_full_strata = TRUE) %>%
  filter(
    catchCategory == "Lan",
    originType == "WGEstimate") %>%
  mutate(total = as.numeric(total)) %>%
  summarise(total = sum(total),
            total_ton = total/1000)

### Check dimension and number of entries per category
list_cef$catches_WGEstimates <-  list_cef$catches %>%
  filter(originType == "WGEstimate") %>%
  cef_add_strata(only_full_strata = TRUE)
dim(list_cef$catches_WGEstimates)
table(list_cef$catches_WGEstimates$catchCategory)
# view(list_cef$catches_WGEstimates)

list_cef$catches_WGEstimates

################################################################################
### Discards raising
################################################################################

### Split data with L and D and with L only, provide tables as wider table (Landings and discards in columns)
list_stock_overview <- split_L_with_without_D(cef_catches = list_cef$catches,
                                              originType = "WGEstimate",
                                              domain = "domainCatchDis",
                                              type = "discards",
                                              add_strata = TRUE)


dim(list_cef$catches_WGEstimates)

### Check Landing total
dim(list_stock_overview$dfw_stock_overview)
dim(list_stock_overview$dfw_stock_overview_L_D)
dim(list_stock_overview$dfw_stock_overview_L_noD)

sum(list_stock_overview$dfw_stock_overview$Lan)
sum(list_stock_overview$dfw_stock_overview_L_D$Lan) +
  sum(list_stock_overview$dfw_stock_overview_L_noD$Lan)

strata_raising_ordered <- c("strata_c_q_f",
                            "strata_c_q_a_g",
                            "strata_c_q_a_sg",
                            "strata_q_a_sg",
                            "strata_q_a",
                            "strata_q")

strata_var <- "strata_c_q_a_g"

results_coverage <- map(strata_raising_ordered, function(strata_var) {
  strata_coverage_summary(
    dfw_stock_overview = list_stock_overview$dfw_stock_overview,
    dfw_stock_overview_L_D =  list_stock_overview$dfw_stock_overview_L_D,
    dfw_stock_overview_L_noD =  list_stock_overview$dfw_stock_overview_L_noD,
    strata_var = !!sym(strata_var)
  )
}) %>%
  set_names(strata_raising_ordered)

#### Strata to be used for raising
# c: country
# q: quarter
# f: fleet
# a: area
# sg: "super-gear" : O, G, T ...

strata_raising_ordered <- c("strata_c_q_f",
                            "strata_c_q_a_g",
                            "strata_c_q_a_sg",
                            "strata_q_a_sg",
                            "strata_q_a",
                            "strata_q")

### only keep strata to be used in discards raising
list_stock_overview <- purrr::map(list_stock_overview,
                                  cef_remove_strata,
                                  extra_strata = strata_raising_ordered)

### Raising discards
list_raised_discards <- NULL
emhUtils::unpack_list(list_stock_overview)

for (i_strata in  strata_raising_ordered) {
  print(i_strata)
  ### raise discards per strata
  list_raised_discards[[i_strata]] <- raise_discards_strata(strata_var = i_strata,
                                                            dfw_stock_overview_L_D = dfw_stock_overview_L_D,
                                                            dfw_stock_overview_L_noD = dfw_stock_overview_L_noD)

  dfw_stock_overview_L_noD <- list_raised_discards[[i_strata]]$dfw_stock_overview_L_noD
}


df_raised_all <- list_raised_discards %>%
  keep(~ "df_raised_discards" %in% names(.x)) %>%
  map("df_raised_discards") %>%
  bind_rows(.id = "strata")

dfw_stock_overview_L_D_raised <- bind_rows(dfw_stock_overview_L_D,
                                           df_raised_all)


cef_catches_domainBiology <- list_stock_overview$cef_catches_originType %>%
  select(         strata_full, domainBiology, catchCategory) %>%
  filter(!is.na(domainBiology))


df_stock_overview_L_D_raised <- pivot_longer(dfw_stock_overview_L_D_raised,
                                             cols = any_of(c("Lan", "Dis", "BMS")),
                                             names_to = "catchCategory",
                                             values_to = "Catch_kg") %>%
  left_join(.,
            cef_catches_domainBiology,
            by = join_by(strata_full, catchCategory))


saveRDS(dfw_stock_overview_L_D_raised,
        file = glue("WGRDBESstockCoord/personal/JB/output/dfw_stock_overview_L_D_raised_{data_year}.rds"))

saveRDS(df_stock_overview_L_D_raised,
        file = glue("WGRDBESstockCoord/personal/JB/output/df_stock_overview_L_D_raised{data_year}.rds"))


### Check that input and output have the same number of rows
dim(list_stock_overview$dfw_stock_overview_L_noD)[1]
dim(df_raised_all)[1]

### Check that input and output have the amount of landings with no discards
sum(df_raised_all$Lan)
sum(list_stock_overview$dfw_stock_overview_L_noD$Lan)

### Check that input and output have the amount of total landings
list_cef_total_landings$total
sum(dfw_stock_overview_L_D$Lan) +
  sum(df_raised_all$Lan)

###-----------------------------------------------------------------------------
### Export percentage of landings raised by each stratum

df_strata_summary_raising <- list_raised_discards %>%
  keep(~ "strata_summary_raising" %in% names(.x)) %>%
  map("strata_summary_raising") %>%
  bind_rows(.id = "strata")

df_strata_summary_raising <- df_strata_summary_raising %>%
  mutate(percentage_landings_raised_with_noDiscards = round(persentage_landings_raised_without_D,
                                                            digits = 4) * 100,
         # total_landings = sum(dfw_stock_overview_L_D$Lan_total),
         percentage_landings_raised = round(raised_landings / total_landings,
                                            digits = 4) * 100)

###-----------------------------------------------------------------------------
plot_stratum_raised_percentage <- ggplot() +
  geom_col(data = df_strata_summary_raising,
           aes(x = strata, y =  percentage_landings_raised_with_noDiscards),
           position = position_dodge2()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4)) +
  ylab("Percentage") +
  ggtitle("Percentage of landings with raised discards over total landings without discards")

plot_stratum_raised_percentage_total <- ggplot() +
  geom_col(data = df_strata_summary_raising,
           aes(x = strata, y =  percentage_landings_raised),
           position = position_dodge2()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4)) +
  ylab("Percentage") +
  ggtitle("Percentage of landings with raised discards over total landings")

list_plot_stratum_raised <- list(plot_stratum_raised_percentage = plot_stratum_raised_percentage,
                                 plot_stratum_raised_percentage_total = plot_stratum_raised_percentage_total)

################################################################################
### Comparison with IC raised data
################################################################################
source(here("WGRDBESstockCoord/personal/JB/load_IC_raised_allocated_2024.R"))

df_stock_overview_L_D_raised <- df_stock_overview_L_D_raised %>%
  mutate(raising_type = "CEF")

df_stock_overview_L_D_comparison <- bind_rows(df_stock_overview_L_D_raised,
                                              df_stock_overview_raised_w_2024_IC)


df_stock_overview_L_D_comparison_Dis <- df_stock_overview_L_D_comparison %>%
  filter(catchCategory == "Dis") %>%
  mutate(strata_id = glue("{year};{vesselFlagCountry};{areaValue};{seasonValue};{fleetValue}")) %>%
  droplevels()


plot_comparison <- ggplot() +
  geom_col(data = df_stock_overview_L_D_comparison_Dis,
           aes(x = strata_id, y =  Catch_kg, fill = raising_type),
           position = position_dodge2()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4)) +
  ylab("Percentage") +
  ggtitle("Percentage of landings with raised discards over total landings")

plot_comparison



df_stock_overview_L_D_comparison_Dis0 <- df_stock_overview_L_D_comparison_Dis %>%
  filter(Catch_kg > 0) %>%
  droplevels()


plot_comparison_Dis0 <- ggplot() +
  geom_col(data = df_stock_overview_L_D_comparison_Dis0,
           aes(x = strata_id, y =  Catch_kg, fill = raising_type),
           position = position_dodge2()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4)) +
  ylab("Percentage") +
  ggtitle("Percentage of landings with raised discards over total landings")

plot_comparison_Dis0
