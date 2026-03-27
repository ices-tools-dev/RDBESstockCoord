build_gap_table <- function(df, thresh_val = 50) {
  
  gap_table <- df %>%
    filter(Status != paste("✅ SAFE : >", thresh_val, " Coverage"))%>%     arrange(desc(total_landings)) %>%
    
    arrange(desc(total_landings)) %>%
    select(area, quarter, metier_group, total_landings, coverage, Status) %>%
    mutate(
      Action_Required = case_when(
        Status == "❌ NO Discard Data" ~ "❌ BORROW: Search similar metier/quarter",
        Status == paste("⚠️ WARNING: <", thresh_val, " Coverage") ~ "⚠️ POOL: Group with adjacent quarters",
        TRUE ~ "❌ BORROW: Search similar metier/quarter"
      )
    ) %>% 
    as.data.frame()
  
  write_csv(gap_table, "outputs/gap_table.csv")
  
  return(gap_table)
}
gap_tab <- build_gap_table(setup_discards,thresh_val = 50)
