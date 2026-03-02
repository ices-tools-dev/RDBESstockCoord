raise_discards_strata <- function(strata_var,
                                   dfw_stock_overview_L_D,
                                   dfw_stock_overview_L_noD) {
  # strata_var is a bare column name, e.g. strata_c_q_f
  strata_var <- rlang::enquo(strata_var)

  ## 1) Build the sampled discards object, grouped by the chosen strata
  df_sampled_discards <- dfw_stock_overview_L_D %>%
    dplyr::group_by(!!strata_var) %>%
    dplyr::mutate(
      total_Landings_Catchkg = sum(Landings_Catchkg)
    ) %>%
    dplyr::mutate(
      weightingfactor = Landings_Catchkg / total_Landings_Catchkg,
      weightingfactor = ifelse(is.na(weightingfactor), 1, weightingfactor)
    ) %>%
    dplyr::select(!!strata_var, discards_ratio, weightingfactor)

  ## 2) Raise discards
  df_raised_discards <- dplyr::inner_join(
    dfw_stock_overview_L_noD,
    df_sampled_discards,
    by = rlang::as_name(strata_var),
    relationship = "many-to-many"
  ) %>%
    dplyr::mutate(
      raised_discards = Landings_Catchkg * discards_ratio * weightingfactor,
      raised_discards = ifelse(Landings_Catchkg == 0, 0, raised_discards)
    ) %>%
    dplyr::filter(!is.na(raised_discards)) %>%
    dplyr::group_by(strata_full, Fleets) %>%
    dplyr::summarise(
      raised_discards = sum(raised_discards, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    droplevels() %>%
    dplyr::left_join(
      dfw_stock_overview_L_noD %>%
        dplyr::select(strata_full, Fleets, Landings_Catchkg),
      by = c("strata_full", "Fleets")
    ) %>%
    dplyr::rename(raised_landings = Landings_Catchkg)

  ## 3) Strata with no discards
  strata_no_discards <- dfw_stock_overview_L_noD %>%
    dplyr::distinct(strata_full) %>%
    dplyr::mutate(Fleets = NA) %>%
    dplyr::filter(!(strata_full %in% levels(df_raised_discards$strata_full))) %>%
    droplevels()

  ## 4) Summary tibble for this strata variable
  df_strata_raised <- tibble::tibble(
    strata = rlang::as_name(strata_var),
    total_landings_with_noDiscards =
      sum(dfw_stock_overview_L_noD$Landings_Catchkg),
    raised_landings = sum(df_raised_discards$raised_landings)
  ) %>%
    dplyr::mutate(
      percentage_landings_raised_with_noDiscards =
        raised_landings / total_landings_with_noDiscards
    )

  ## Return as a named list with generic component names
  list(
    strata_summary = df_strata_raised,
    strata_no_discards = strata_no_discards,
    raised_discards = df_raised_discards
  )
}
