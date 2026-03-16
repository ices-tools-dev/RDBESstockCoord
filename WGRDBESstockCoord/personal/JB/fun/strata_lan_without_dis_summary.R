#' Compute strata coverage summary for discards data
#'
#' Groups data by a dynamic strata variable, computes total Lan and coverage,
#' and adds summary statistics.
#'
#' @param dfw_stock_overview A wide-format data frame with `Lan_total` and
#'   `Dis_total` columns (e.g. from `compute_discards_ratio()`).
#' @param strata_var Unquoted name of the strata column to group by and label
#'   (e.g. `strata_c`, `stratum_full`).
#'
#' @return A data frame with:
#' \itemize{
#'   \item `{strata_var}`: the grouping strata values
#'   \item `strata_total_Lan`: sum of `Lan_total` per strata
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
strata_lan_without_dis_summary <- function(dfw_stock_overview_L_noD) {

  dfw_stock_overview_L_noD <- dfw_stock_overview_L_noD %>%
    dplyr::group_by(!!strata_col) %>%
    dplyr::reframe(
      strata_total_Lan = sum(Lan_total, na.rm = TRUE),
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
                  strata, values, values_strata, sum_coverage_total_Lan)

  return(dfsw_stock_overview)
}
