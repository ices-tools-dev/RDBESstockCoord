#' Allocate age compositions to missing strata
#'
#' This helper allocates numbers-at-age to strata that do not have sampling
#' observations, using reference numbers computed from a broader grouping level.
#' It filters the strata that are still unallocated, joins reference numbers,
#' computes allocated numbers, and tags the source used for allocation.
#'
#' @param df_allocation_fleets A data frame of landings/discards strata that
#'   still need age allocation.
#' @param df_canum_for_allocation A data frame of observed numbers-at-age used
#'   to compute reference numbers.
#' @param strata_no_age A data frame containing the strata that still have no
#'   age information, typically with at least a `strata_full` column.
#' @param group_strata A single character string giving the grouping stratum
#'   name used by `cef_compute_ref_numbers()`, for example
#'   `"strata_c_s_g"` or `"strata_s_g"`.
#' @param join_by_cols A character vector of join columns used to match
#'   `df_allocation_fleets` with the reference numbers table.
#' @param source_name A single character string written to the `Source` column
#'   to indicate which allocation rule was used.
#' @param extra_filter Optional additional filter expression applied to
#'   `df_allocation_fleets` before joining, for example `gear == "MIS"`.
#' @param number_col Name of the column containing numbers-at-age
#'
#' @return A tibble with allocated rows, including a `numbers` column and a
#'   `Source` column.
#'
#' @details
#' The allocation is done as:
#' \deqn{numbers = Catchkg \times numbers\_ref / SoP}
#'
#' The helper keeps only rows with non-missing `Age`. If factor columns are
#' present, their unused levels are dropped before returning the result.
#'
#' @examples
#' \dontrun{
#' cef_allocate_strata(
#'   df_allocation_fleets = df_allocation_fleets,
#'   df_canum_for_allocation = df_canum_for_allocation,
#'   strata_no_age = strata_no_age,
#'   group_strata = "strata_c_s_g",
#'   join_by_cols = c("strata_c_s_g", "CatchCat"),
#'   source_name = "strata_c_s_g"
#' )
#' }
#'
#' @export
cef_allocate_strata <- function(df_allocation_fleets,
                                df_canum_for_allocation,
                                strata_no_age,
                                group_strata,
                                join_by_cols,
                                source_name,
                                number_col = "Number_Total",
                                extra_filter = NULL) {

  # ---------------------------------------------------------------------------
  # Check main arguments
  # ---------------------------------------------------------------------------

  if (!is.character(group_strata) || length(group_strata) != 1) {
    rlang::abort("`group_strata` must be a single character string.")
  }

  if (!is.character(join_by_cols) || length(join_by_cols) < 1) {
    rlang::abort("`join_by_cols` must be a non-empty character vector.")
  }

  if (!is.character(source_name) || length(source_name) != 1) {
    rlang::abort("`source_name` must be a single character string.")
  }

  # ---------------------------------------------------------------------------
  # Check required columns in each input table
  # ---------------------------------------------------------------------------
  # Check that the grouping column exists in the CANUM data used to build

  # Check for selected strata
  if (!group_strata %in% names(df_canum_for_allocation)) {
    rlang::abort(
      paste0(
        "Column `", group_strata,
        "` was not found in `df_canum_for_allocation`."
      )
    )
  }

  if (!group_strata %in% names(df_allocation_fleets)) {
    rlang::abort(
      paste0(
        "Column `", group_strata,
        "` was not found in `df_allocation_fleets`."
      )
    )
  }

  # Check required columns in df_allocation_fleets
  required_allocation_cols <- unique(c("strata_full", join_by_cols))
  missing_allocation_cols <- setdiff(required_allocation_cols, names(df_allocation_fleets))

  if (length(missing_allocation_cols) > 0) {
    rlang::abort(
      paste0(
        "Missing required columns in `df_allocation_fleets`: ",
        paste(missing_allocation_cols, collapse = ", ")
      )
    )
  }

  # Check required columns in strata_no_age
  required_strata_cols <- "strata_full"
  missing_strata_cols <- setdiff(required_strata_cols, names(strata_no_age))

  if (length(missing_strata_cols) > 0) {
    rlang::abort(
      paste0(
        "Missing required columns in `strata_no_age`: ",
        paste(missing_strata_cols, collapse = ", ")
      )
    )
  }

  # Compute reference numbers
  ref_numbers <- cef_compute_ref_numbers(
    df_canum_imported = df_canum_for_allocation,
    number_col = number_col,
    group_strata = group_strata
  )

  # Check required columns in ref_numbers
  required_ref_cols <- unique(c(join_by_cols, "numbers_ref", "SoP", "Age"))
  missing_ref_cols <- setdiff(required_ref_cols, names(ref_numbers))

  if (length(missing_ref_cols) > 0) {
    rlang::abort(
      paste0(
        "Missing required columns in reference numbers table: ",
        paste(missing_ref_cols, collapse = ", ")
      )
    )
  }

  # ---------------------------------------------------------------------------
  # Compute reference numbers from the observed CANUM data
  # ---------------------------------------------------------------------------

  ref_numbers <- cef_compute_ref_numbers(
    df_canum_imported = df_canum_for_allocation,
    group_strata = group_strata
  )

  # Check that the reference table contains the columns needed for the join
  # and the allocation formula.
  required_ref_cols <- unique(c(join_by_cols, "numbers_ref", "SoP", "Age"))
  missing_ref_cols <- setdiff(required_ref_cols, names(ref_numbers))

  if (length(missing_ref_cols) > 0) {
    rlang::abort(
      paste0(
        "Missing required columns in reference numbers table: ",
        paste(missing_ref_cols, collapse = ", ")
      )
    )
  }

  # ---------------------------------------------------------------------------
  # Keep only strata that still need age allocation
  # ---------------------------------------------------------------------------

  df_to_allocate <- df_allocation_fleets %>%
    dplyr::filter(.data$strata_full %in% strata_no_age$strata_full)

  # Apply an optional extra filter, for example: gear == "MIS".
  if (!is.null(extra_filter)) {
    extra_filter <- rlang::enquo(extra_filter)

    df_to_allocate <- df_to_allocate %>%
      dplyr::filter(!!extra_filter)
  }

  # ---------------------------------------------------------------------------
  # Join allocation strata with reference age compositions
  # ---------------------------------------------------------------------------

  out <- dplyr::inner_join(
    df_to_allocate,
    ref_numbers,
    by = join_by_cols,
    relationship = "many-to-many"
  ) %>%
    # Convert reference composition into allocated numbers-at-age.
    dplyr::mutate(
      numbers = .data$Catchkg * .data$numbers_ref / .data$SoP,
      Source = source_name
    ) %>%
    # Remove rows where age is not defined.
    dplyr::filter(!is.na(.data$Age))

  # ---------------------------------------------------------------------------
  # Drop unused levels only on factor columns
  # ---------------------------------------------------------------------------

  out <- out %>%
    dplyr::mutate(
      dplyr::across(
        where(is.factor),
        forcats::fct_drop
      )
    )

  out
}
