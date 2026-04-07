raise_discards_strata <- function(strata_var,
                                  dfw_stock_overview_L_D,
                                  dfw_stock_overview_L_noD) {

  total_landings <-

  if( dim(dfw_stock_overview_L_noD)[1] == 0) {
    warning(glue::glue("No discards left to be raised with {strata_var}"))
  }

  if(!(strata_var %in% names(dfw_stock_overview_L_D))) {
    stop("strata_var not in data frames")
  }

  # strata_var is a bare column name, e.g. strata_c_q_f
  strata_var <- rlang::enquo(strata_var)

  ## 1) Build the sampled discards object, grouped by the chosen strata
  df_sampled_discards <- dfw_stock_overview_L_D %>%
    group_by(across(!!strata_var)) %>%
    mutate(
      total_Lan_strata = sum(Lan_total),
      n_per_strata = n()
    ) %>%
    dplyr::mutate(
      weightingfactor = ifelse(n_per_strata == 1, 1, Lan_total / total_Lan_strata)
    ) %>%
    dplyr::select(!!strata_var,
                  # Lan_total,
                  total_Lan_strata,
                  # total_landings,
                  Discards_ratio,
                  weightingfactor)

  ## 2) Raise discards
  df_raised_discards <- dplyr::inner_join(
    dfw_stock_overview_L_noD,
    df_sampled_discards,
    by = rlang::as_name(strata_var),
    relationship = "many-to-many"
  ) %>%
    dplyr::mutate(
      raised_discards = ifelse(Lan_total == 0, 0,
                               Lan_total * Discards_ratio * weightingfactor)
    ) %>%
    dplyr::filter(!is.na(raised_discards)) %>%
    dplyr::group_by(strata_full, fleetValue) %>%
    dplyr::summarise(
      raised_discards = sum(raised_discards, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    droplevels() %>%
    dplyr::left_join(
      dfw_stock_overview_L_noD %>%
        dplyr::select(strata_full, fleetValue, Lan_total),
      by = c("strata_full", "fleetValue")
    ) %>%
    dplyr::mutate(importedOrRaised = "raised")

  ## 4) Summary tibble for this strata variable
  df_strata_raised <- tibble::tibble(
    strata = rlang::as_name(strata_var),
    total_landings_with_noDiscards =
      sum(dfw_stock_overview_L_noD$Lan_total),
    landings_with_Discards = sum(dfw_stock_overview_L_D$Lan_total),
    total_landings = sum(c(dfw_stock_overview_L_noD$Lan_total, dfw_stock_overview_L_D$Lan_total)),
    raised_landings = sum(df_raised_discards$Lan_total)
  ) %>%
    dplyr::mutate(
      persentage_landings_raised_without_D = raised_landings /
        landings_with_Discards,
      discards_ratio_raised = raised_landings /
        total_landings_with_noDiscards
    )

  ### 5) Merge raised discards to dfw_stock_overview_L_D
  df_raised_discards <- left_join(dplyr::select(dfw_stock_overview_L_noD,
                                                -importedOrRaised),
                                  dplyr::select(df_raised_discards,
                                                -Lan_total),
                                  by = join_by(fleetValue, strata_full)) %>%
    dplyr::mutate(Dis_total = ifelse(importedOrRaised == "raised",
                                     raised_discards, NA)) %>%
    dplyr::filter(!is.na(Dis_total)) %>%
    droplevels()

  dfw_stock_overview_L_noD <- dfw_stock_overview_L_noD %>%
    dplyr::filter(!(strata_full %in% df_raised_discards$strata_full))

  ## Return as a named list
  list(
    strata_summary_raising = df_strata_raised,
    dfw_stock_overview_L_D = dfw_stock_overview_L_D,
    dfw_stock_overview_L_noD = dfw_stock_overview_L_noD,
    df_raised_discards = df_raised_discards
  )
}
