census_add_stratum <- function(df_census) {
  df_census <- df_census %>%
    dplyr::mutate(
      fleet = forcats::as_factor(.data$fleet),
      stratum_full = forcats::as_factor(glue::glue("{stock}{VesselFlagCountry}-{quarter}-{faoArea}-{fleet}-{FAC_EC_lvl6}")),
      gear = stringr::str_sub(string = .data$FAC_EC_lvl6, start = 1,  end = 3),
      super_gear = stringr::str_sub(string = .data$gear, start = 1, end = 1),
      super_gear = dplyr::case_when(gear == "OTM" ~ "P",
                             gear == "TTB" ~ "O",
                             gear == "TBB" ~ "O",
                             gear == "PTB" ~ "O",
                             gear == "PS" ~ "S",
                             TRUE ~ super_gear),

      # Top combinations
      stratum_c_q_f_a = glue::glue("{VesselFlagCountry}-{quarter}-{fleet}-{faoArea}"),
      stratum_c_q_f = glue::glue("{VesselFlagCountry}-{quarter}-{fleet}"),
      stratum_c_q_a = glue::glue("{VesselFlagCountry}-{quarter}-{faoArea}"),
      stratum_c_q = glue::glue("{VesselFlagCountry}-{quarter}"),
      stratum_c_f_a = glue::glue("{VesselFlagCountry}-{fleet}-{faoArea}"),
      stratum_c_f = glue::glue("{VesselFlagCountry}-{fleet}"),
      stratum_c_a = glue::glue("{VesselFlagCountry}-{faoArea}"),
      stratum_c = glue::glue("{VesselFlagCountry}"),
      stratum_q_f_a = glue::glue("{quarter}-{fleet}-{faoArea}"),
      stratum_q_f = glue::glue("{quarter}-{fleet}"),
      stratum_q_a = glue::glue("{quarter}-{faoArea}"),
      stratum_q = glue::glue("{quarter}"),
      stratum_f_a = glue::glue("{fleet}-{faoArea}"),
      stratum_f = glue::glue("{fleet}"),
      stratum_a = glue::glue("{faoArea}"),

      # Combinations with gear
      stratum_c_q_f_a_g = glue::glue("{VesselFlagCountry}-{quarter}-{fleet}-{faoArea}-{gear}"),
      stratum_c_q_f_g = glue::glue("{VesselFlagCountry}-{quarter}-{fleet}-{gear}"),
      stratum_c_q_a_g = glue::glue("{VesselFlagCountry}-{quarter}-{faoArea}-{gear}"),
      stratum_c_q_g = glue::glue("{VesselFlagCountry}-{quarter}-{gear}"),
      stratum_c_f_a_g = glue::glue("{VesselFlagCountry}-{fleet}-{faoArea}-{gear}"),
      stratum_c_f_g = glue::glue("{VesselFlagCountry}-{fleet}-{gear}"),
      stratum_c_a_g = glue::glue("{VesselFlagCountry}-{faoArea}-{gear}"),
      stratum_c_g = glue::glue("{VesselFlagCountry}-{gear}"),
      stratum_q_f_a_g = glue::glue("{quarter}-{fleet}-{faoArea}-{gear}"),
      stratum_q_f_g = glue::glue("{quarter}-{fleet}-{gear}"),
      stratum_q_a_g = glue::glue("{quarter}-{faoArea}-{gear}"),
      stratum_q_g = glue::glue("{quarter}-{gear}"),
      stratum_f_a_g = glue::glue("{fleet}-{faoArea}-{gear}"),
      stratum_f_g = glue::glue("{fleet}-{gear}"),
      stratum_a_g = glue::glue("{faoArea}-{gear}"),
      stratum_g = glue::glue("{gear}"),

      # Combinations with super_gear
      stratum_c_q_f_a_sg = glue::glue("{VesselFlagCountry}-{quarter}-{fleet}-{faoArea}-{super_gear}"),
      stratum_c_q_f_sg = glue::glue("{VesselFlagCountry}-{quarter}-{fleet}-{super_gear}"),
      stratum_c_q_a_sg = glue::glue("{VesselFlagCountry}-{quarter}-{faoArea}-{super_gear}"),
      stratum_c_q_sg = glue::glue("{VesselFlagCountry}-{quarter}-{super_gear}"),
      stratum_c_f_a_sg = glue::glue("{VesselFlagCountry}-{fleet}-{faoArea}-{super_gear}"),
      stratum_c_f_sg = glue::glue("{VesselFlagCountry}-{fleet}-{super_gear}"),
      stratum_c_a_sg = glue::glue("{VesselFlagCountry}-{faoArea}-{super_gear}"),
      stratum_c_sg = glue::glue("{VesselFlagCountry}-{super_gear}"),
      stratum_q_f_a_sg = glue::glue("{quarter}-{fleet}-{faoArea}-{super_gear}"),
      stratum_q_f_sg = glue::glue("{quarter}-{fleet}-{super_gear}"),
      stratum_q_a_sg = glue::glue("{quarter}-{faoArea}-{super_gear}"),
      stratum_q_sg = glue::glue("{quarter}-{super_gear}"),
      stratum_f_a_sg = glue::glue("{fleet}-{faoArea}-{super_gear}"),
      stratum_f_sg = glue::glue("{fleet}-{super_gear}"),
      stratum_a_sg = glue::glue("{faoArea}-{super_gear}"),
      stratum_sg = glue::glue("{super_gear}"),

      # Combinations with FAC_EC_lvl6
      stratum_c_q_f_a_lvl6 = glue::glue("{VesselFlagCountry}-{quarter}-{fleet}-{faoArea}-{FAC_EC_lvl6}"),
      stratum_c_q_f_lvl6 = glue::glue("{VesselFlagCountry}-{quarter}-{fleet}-{FAC_EC_lvl6}"),
      stratum_c_q_a_lvl6 = glue::glue("{VesselFlagCountry}-{quarter}-{faoArea}-{FAC_EC_lvl6}"),
      stratum_c_q_lvl6 = glue::glue("{VesselFlagCountry}-{quarter}-{FAC_EC_lvl6}"),
      stratum_c_f_a_lvl6 = glue::glue("{VesselFlagCountry}-{fleet}-{faoArea}-{FAC_EC_lvl6}"),
      stratum_c_f_lvl6 = glue::glue("{VesselFlagCountry}-{fleet}-{FAC_EC_lvl6}"),
      stratum_c_a_lvl6 = glue::glue("{VesselFlagCountry}-{faoArea}-{FAC_EC_lvl6}"),
      stratum_c_lvl6 = glue::glue("{VesselFlagCountry}-{FAC_EC_lvl6}"),
      stratum_q_f_a_lvl6 = glue::glue("{quarter}-{fleet}-{faoArea}-{FAC_EC_lvl6}"),
      stratum_q_f_lvl6 = glue::glue("{quarter}-{fleet}-{FAC_EC_lvl6}"),
      stratum_q_a_lvl6 = glue::glue("{quarter}-{faoArea}-{FAC_EC_lvl6}"),
      stratum_q_lvl6 = glue::glue("{quarter}-{FAC_EC_lvl6}"),
      stratum_f_a_lvl6 = glue::glue("{fleet}-{faoArea}-{FAC_EC_lvl6}"),
      stratum_f_lvl6 = glue::glue("{fleet}-{FAC_EC_lvl6}"),
      stratum_a_lvl6 = glue::glue("{faoArea}-{FAC_EC_lvl6}"),
      stratum_lvl6 = glue::glue("{FAC_EC_lvl6}")
    )
  return(df_census)
}



census_remove_stratum <- function(df_census) {
  df_census <- df_census %>%
    dplyr::select(-dplyr::starts_with("stratum_") | stratum_full)
  return(df_census)
}

