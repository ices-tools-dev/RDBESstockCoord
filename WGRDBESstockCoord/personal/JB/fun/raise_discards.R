
get_ratio_raise_discards_strata <- function(data_discards_raised) {
  df_stratum_raised <- data_discards_raised %>%
    group_by(raised_strata) %>%
    summarise(total_landings = sum(Landings_Catchkg),
              raised_landings = sum(raised_landings)) %>%
    mutate(percentage_landings_raised = raised_landings / total_landings)
  
}

