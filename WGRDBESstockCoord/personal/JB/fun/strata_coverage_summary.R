#' Compute strata coverage summary for discards data
#'
#' Groups data by a dynamic strata variable, computes total LAN and coverage,
#' and adds summary statistics.
#'
#' @param dfw_stock_overview A wide-format data frame with `LAN_total` and
#'   `DIS_total` columns (e.g. from `compute_discards_ratio()`).
#' @param strata_var Unquoted name of the strata column to group by and label
#'   (e.g. `strata_c`, `stratum_full`).
#' @param total_landings Numeric scalar; total landings value for coverage calc.
#'
#' @return A data frame with:
#' \itemize{
#'   \item `{strata_var}`: the grouping strata values
#'   \item `strata_total_LAN`: sum of `LAN_total` per strata
#'   \item `coverage_total_LAN`: proportion of total_landings
#'   \item `strata`: same values as `{strata_var}`
#'   \item `values`: copy of `{strata_var}`
#'   \item `sum_coverage_total_LAN`: sum of coverage across all strata
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
                                    strata_var,
                                    total_landings) {

  strata_col <- rlang::ensym(strata_var)
  strata_name <- rlang::as_name(strata_col)

  if (!strata_name %in% names(dfw_stock_overview)) {
    stop("Strata column '", strata_name, "' not found")
  }

  dfsw_stock_overview <- dfw_stock_overview %>%
    dplyr::filter(!is.na(DIS_total)) %>%
    dplyr::group_by(!!strata_col) %>%
    dplyr::reframe(
      strata_total_LAN = sum(LAN_total, na.rm = TRUE),
      coverage_total_LAN = strata_total_LAN / total_landings
    ) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      strata = strata_name,
      values = !!strata_col,
      sum_coverage_total_LAN = sum(coverage_total_LAN, na.rm = TRUE)
    ) %>%
    dplyr::select(!!strata_col, strata_total_LAN, coverage_total_LAN,
                  strata, values, sum_coverage_total_LAN)

  return(dfsw_stock_overview)
}
