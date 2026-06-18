#' Compute strata coverage summary for discards data
#'
#' Groups data by a dynamic strata variable, computes total Lan and coverage,
#' and adds summary statistics.
#'
#' @param dfw_stock_overview A wide-format data frame with `Lan` and
#'   `Dis` columns (e.g. from `compute_discards_ratio()`).
#' @param strata_var Unquoted name of the strata column to group by and label
#'   (e.g. `strata_c`, `stratum_full`).
#'
#' @return A data frame with:
#' \itemize{
#'   \item `{strata_var}`: the grouping strata values
#'   \item `strata_total_Lan`: sum of `Lan` per strata
#'   \item `coverage_total_Lan`: proportion of total_landings
#'   \item `strata`: same values as `{strata_var}`
#'   \item `values`: copy of `{strata_var}`
#'   \item `sum_coverage_total_Lan`: sum of coverage across all strata
#' }
#'
#' @examples
#' strata_coverage_summary(
#'   dfw_stock_overview,
#'   strata_var = strata_c,
#'   total_landings = 1e6
#' )
#'
#' @export
strata_coverage_summary <- function(dfw_stock_overview,
                                    dfw_stock_overview_L_D = NULL,
                                    dfw_stock_overview_L_noD = NULL,
                                    strata_var) {

  strata_col <- rlang::ensym(strata_var)
  strata_name <- rlang::as_name(strata_col)

  if (!strata_name %in% names(dfw_stock_overview)) {
    stop("Strata column '", strata_name, "' not found")
  }

  dfsw_stock_overview <- dfw_stock_overview %>%
    dplyr::filter(!is.na(Dis)) %>%
    dplyr::group_by(!!strata_col) %>%
    dplyr::reframe(
      strata_total_Lan = sum(Lan, na.rm = TRUE),
      coverage_total_Lan = strata_total_Lan / total_landings * 100
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      strata = strata_name,
      values = !!strata_col,
      values_strata = glue::glue("{strata_name}-{values}"),
      sum_coverage_total_Lan = sum(coverage_total_Lan, na.rm = TRUE) * 100
    ) %>%
    dplyr::select(strata_total_Lan, coverage_total_Lan,
                  strata, values, values_strata, sum_coverage_total_Lan) %>%
    filter(coverage_total_Lan > 0) %>%
    droplevels()

  plot_strata_coverage <-  ggplot(data = dfsw_stock_overview,
                                  aes(x = forcats::fct_reorder(values_strata, coverage_total_Lan),
                                      y = coverage_total_Lan,
                                      fill = values_strata )) +
    geom_col() +
    ylim(0, 100) +
    theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
    ylab("Percentage of landings with discards") +
    xlab("Strata") +
    facet_wrap(~ strata)

  if(!is.null(dfw_stock_overview_L_D) & !is.null(dfw_stock_overview_L_noD)) {

    dfw_stock_overview_L_D <- dfw_stock_overview_L_D %>%
      semi_join(dfw_stock_overview_L_noD, by = strata_name) %>%
      dplyr::select(any_of(c(strata_name,
                             "strata_full", "Dis", "Lan"))) %>%
      group_by(!!strata_col) %>%
      summarise(Lan = sum(Lan),
                Dis = sum(Dis),
                .groups = "drop_last")

    dfw_stock_overview_L_noD <- dfw_stock_overview_L_noD %>%
      dplyr::select(
        # -starts_with("strata"),
        any_of(c(strata_name, "strata_full", "Dis", "Lan")))

    df_stock_overview_L_D_coverage <- left_join(dfw_stock_overview_L_noD,
                                                 dfw_stock_overview_L_D,
                                                 relationship = "many-to-many",
                                                 suffix = c("_noD", "_withD"),
                                                 by = strata_name) %>%
      filter(!is.na(Dis_withD)) %>%
      group_by(!!strata_col) %>%
      reframe(Lan_withD = sum(Lan_withD),
                Dis_withD = sum(Dis_withD),
                Lan_noD = Lan_noD,
                .groups = "drop_last") %>%
      ungroup() %>%
      pivot_longer(cols = c(Lan_withD, Dis_withD, Lan_noD),
                   names_to = "Values") %>%
      distinct()

    plot_landings_discards_coverage <- ggplot() +
      geom_col(data = df_stock_overview_L_D_coverage,
               aes(x = !!strata_col, y = value, fill = Values),
               position = position_dodge2()) +
      theme(axis.text.x = element_text(angle = 90, hjust = 0.5)) +
      # facet_wrap(~ strata_full_withD) +
      ylab("Weight in kg") +
      ggtitle(glue::glue("Strata use to raise : {strata_name}"))

    if(dim(df_stock_overview_L_D_coverage)[1] == 0){
      warning(glue::glue("No match data with Landings and Discards for {strata_name}"))
    }

      return(list(dfsw_stock_overview = dfsw_stock_overview,
                  df_stock_overview_L_D_coverage = df_stock_overview_L_D_coverage,
                  plot_strata_coverage = plot_strata_coverage,
                  df_stock_overview_L_D_coverage = df_stock_overview_L_D_coverage,
                  plot_landings_discards_coverage = plot_landings_discards_coverage))

  } else {
    return(list(dfsw_stock_overview = dfsw_stock_overview,
                plot_strata_coverage = plot_strata_coverage))
  }
}
