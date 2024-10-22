intercatch_add_stratum <- function(df_intercatch) {
  df_intercatch <- df_intercatch %>%
    mutate(
      Fleets = forcats::as_factor(Fleets),
      stratum_full = forcats::as_factor(glue::glue("{Country}-{Season}-{Area}-{Fleets}")),
      gear = stringr::str_sub(string = Fleets, start = 1,  end = 3),
      super_gear = stringr::str_sub(string = gear, start = 1, end = 1),
      super_gear = case_when(gear == "OTM" ~ "P",
                             gear == "TTB" ~ "O",
                             gear == "TBB" ~ "O",
                             gear == "PTB" ~ "O",
                             gear == "PS" ~ "S",
                             TRUE ~ super_gear),
      stratum_c_s_f = glue::glue("{Country}-{Season}-{Fleets}"),
      stratum_c_s_a_g = glue::glue("{Country}-{Season}-{Area}-{gear}"),
      stratum_c_s_g = glue::glue("{Country}-{Season}-{gear}"),
      stratum_c_s_a_sg = glue::glue("{Country}-{Season}-{Area}-{super_gear}"),
      stratum_c_g = glue::glue("{Country}-{gear}"),
      stratum_c_s = glue::glue("{Country}-{Season}"),
      stratum_s_f = glue::glue("{Season}-{Fleets}"),
      stratum_s_a_g = glue::glue("{Season}-{Area}-{gear}"),
      stratum_s_a_sg = glue::glue("{Season}-{Area}-{super_gear}"),
      stratum_s_sg = glue::glue("{Season}-{super_gear}"),
      stratum_s_g = glue::glue("{Season}-{gear}"),
      stratum_g = glue::glue("{gear}"),
      stratum_sg = glue::glue("{super_gear}"),
      stratum_s_a = glue::glue("{Season}-{Area}"),
      stratum_s = glue::glue("{Season}"),
      stratum_c = glue::glue("{Season}")
    )
  return(df_intercatch)
}


intercatch_remove_stratum <- function(df_intercatch) {
  df_intercatch <- df_intercatch %>%
    dplyr::select(-dplyr::starts_with("stratum_") | stratum_full)
  return(df_intercatch)
}

