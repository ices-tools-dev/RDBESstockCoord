#' Compute reference numbers-at-age for allocation
#'
#' @param df_canum_imported Data frame with numbers-at-age data.
#' @param group_strata Name of grouping column as a string.
#' @param number_col Name of the column containing numbers-at-age
#'   (e.g. "Number_Total"), as a string.
#' @export
cef_compute_ref_numbers <- function(df_canum_imported,
                                    group_strata,
                                    number_col = "Number_Total") {

  # ---------------------------------------------------------------------------
  # Check arguments
  # ---------------------------------------------------------------------------

  if (!is.character(group_strata) || length(group_strata) != 1) {
    rlang::abort("`group_strata` must be a single character string.")
  }

  if (!is.character(number_col) || length(number_col) != 1) {
    rlang::abort("`number_col` must be a single character string.")
  }

  # Check that the requested grouping column exists
  if (!group_strata %in% names(df_canum_imported)) {
    rlang::abort(
      paste0(
        "Column `", group_strata,
        "` was not found in `df_canum_imported`."
      )
    )
  }

  # Check that all other required columns are present
  required_cols <- c("catchCategory", "Age", number_col, "WeightLive_Mean")
  missing_cols <- setdiff(required_cols, names(df_canum_imported))

  if (length(missing_cols) > 0) {
    rlang::abort(
      paste0(
        "Missing required columns in `df_canum_imported`: ",
        paste(missing_cols, collapse = ", ")
      )
    )
  }

  # ---------------------------------------------------------------------------
  # Compute reference numbers-at-age and weighted mean live weight
  # ---------------------------------------------------------------------------

  dfs_canum_stratum <- df_canum_imported %>%
    dplyr::group_by(
      !!rlang::sym(group_strata),
      .data$catchCategory,
      .data$Age
    ) %>%
    dplyr::summarise(
      numbers_ref = sum(.data[[number_col]], na.rm = TRUE),
      weights_ref = dplyr::if_else(
        sum(.data[[number_col]], na.rm = TRUE) > 0,
        sum(.data[[number_col]] * .data$WeightLive_Mean, na.rm = TRUE) /
          sum(.data[[number_col]], na.rm = TRUE),
        NA_real_
      ),
      .groups = "drop"
    ) %>%
    dplyr::group_by(
      !!rlang::sym(group_strata),
      .data$catchCategory
    ) %>%
    dplyr::mutate(
      SoP = sum(.data$numbers_ref * .data$weights_ref / 1000, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()

  dfs_canum_stratum
}
