
build_summary_table <- function(df, thresh_val = 50) {
  source("01_data_generation.R")
  summary_table <- df %>%  
    group_by(stock, area, quarter, metier_group, domainBiology) %>%
    summarise(
      landings = sum(OfficialWeight[catchCategory == "Lan"], na.rm = TRUE),
      discards = sum(OfficialWeight[catchCategory == "Dis"], na.rm = TRUE),
      coverage = case_when(
        is.na(landings) | landings == 0 ~ 0,  # o NA_real_ si prefieres
        is.na(discards)                 ~ 0,
        TRUE                            ~ round((discards / landings) * 100, 2)
      ),
      coverage_pct = sprintf("%.2f%%", coverage),
      has_dis_data = discards > 0,
      # Si no hay datos o el coverage es bajo, marcar para Yves
      needs_raising = !has_dis_data | coverage < thresh_val,
      .groups = "drop"
    ) %>%
    arrange(coverage_pct) %>% as.data.frame()
  
  # EXPORTAR LA TABLA RESUMEN
  write_csv(summary_table, "outputs/summary_table.csv")
  
  return(summary_table)
}
summary_tab <- build_summary_table(census, thresh_val = 50)
