library("icesVocab")
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

path_to_data <- here("WGRDBESstockCoord/personal/JB/data")

# ### Make my own stock relation data frame
# df_stock_relation_sole8ab <- data.frame(
#   SpeciesName = c("Solea solea", "Solea solea"),
#   StockCode = c("sol.27.8ab", "sol.27.8ab"),
#   EG = c("WGBIE", "WGBIE"),
#   ICESArea = c("27.8.a", "27.8.b"),
#   speciesCode = c(198720, 198720),
#   Species = "SOL")

# list_cef <- convExchange(dat_path = glue("{path_to_data}/tmp"),
#                          stock_relation = df_stock_relation_sole8ab,
#                          output_format = "to_list",
#                          metier6 = "Fleet")


## To files:
# funICoutCEF(dat_path = glue("{path_to_data}"),
#             years = 2025,
#             metier6 = "fleet",
#             output_format = "to_file",
#             out_path = glue("{path_to_data}/tmp"),
#             keep_temp_file = TRUE,
#             file_prefix = "sol_8ab")

## Returned as a list:
list_cef <- funICoutCEF(dat_path = glue("{path_to_data}/"),
                        years = 2025,
                        metier6 = "Fleet",
                        output_format = "to_list",
                        out_path = dat_path,
                        keep_temp_file = TRUE,
                        file_prefix = "sol_8ab")


################################################################################
### Discards raising
################################################################################

### needed functions
source("WGRDBESstockCoord/personal/JB/fun/cef_add_strata.R")
source("WGRDBESstockCoord/personal/JB/fun/split_L_with_without_D.R")
source("WGRDBESstockCoord/personal/JB/fun/raise_discards_strata.R")
source("WGRDBESstockCoord/personal/JB/fun/strata_coverage_summary.R")

### Split data with L and D and with L only, provide tables as wider table (Landings and discards in columns)
list_stock_overview <- split_L_with_without_D(cef_catches = list_cef$catches,
                                              originType = "WGEstimate",
                                              domain = "domainCatchDis",
                                              add_strata = TRUE)

strata_raising_ordered <- c("strata_c_q_f",
                            "strata_c_q_a_g",
                            "strata_c_q_a_sg",
                            "strata_q_a_sg",
                            "strata_q_a",
                            "strata_q")

strata_var <- "strata_c_q_a_sg"

results_coverage <- map(strata_raising_ordered, function(strata_var) {
  strata_coverage_summary(
    dfw_stock_overview = list_stock_overview$dfw_stock_overview,
    dfw_stock_overview_L_D =  list_stock_overview$dfw_stock_overview_L_D,
    dfw_stock_overview_L_noD =  list_stock_overview$dfw_stock_overview_L_noD,
    strata_var = !!sym(strata_var)
  )
}) %>%
  set_names(strata_raising_ordered)

####
strata_raising_ordered <- c("strata_c_q_f",
                            # "strata_c_q_a_g",
                            "strata_c_q_a_sg",
                            "strata_q_a_sg",
                            "strata_q_a",
                            "strata_q")

### Raising discards
list_raised_discards <- NULL
emhUtils::unpack_list(list_stock_overview)
i_strata <- strata_raising_ordered[1]

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





###-----------------------------------------------------------------------------
### Export percentage of landings raised by each stratum

df_strata_summary_raising <- list_raised_discards %>%
  keep(~ "strata_summary_raising" %in% names(.x)) %>%
  map("strata_summary_raising") %>%
  bind_rows(.id = "strata")

df_strata_summary_raising <- df_stratum_raised %>%
  mutate(percentage_landings_raised_with_noDiscards = round(percentage_landings_raised_with_noDiscards,
                                                            digits = 4) * 100,
         total_landings = sum(dfw_stock_overview_L_D$Lan_total),
         percentage_landings_raised = round(raised_landings / total_landings,
                                            digits = 4) * 100)

###-----------------------------------------------------------------------------
plot_stratum_raised_percentage <- ggplot() +
  geom_col(data = df_strata_summary_raising,
           aes(x = strata, y =  percentage_landings_raised_with_noDiscards),
           position = position_dodge2()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))+
  ylab("Percentage of landings with raised discards over total landings without discards")

plot_stratum_raised_percentage_total <- ggplot() +
  geom_col(data = df_strata_summary_raising,
           aes(x = strata, y =  percentage_landings_raised),
           position = position_dodge2()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4)) +
  ylab("Percentage of landings with raised discards over total landings")

ggsave( plot = plot_stratum_raised_percentage_total,
        filename = glue("{path_data_raised}/plot_stratum_raised_percentage_total.png"),
        width = 20, height = 20, units = "cm")

list_plot_stratum_raised <- list(plot_stratum_raised_percentage = plot_stratum_raised_percentage,
                                 plot_stratum_raised_percentage_total = plot_stratum_raised_percentage_total)

saveRDS(list_plot_stratum_raised,
        file = glue("{path_data_raised}/df_stratum_raised.rds"))
