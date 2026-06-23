sop <- function(caton, canum, by) {

  check_required_cols <- function(data, required, data_name) {
    missing_cols <- setdiff(required, names(data))

    if (length(missing_cols) > 0) {
      rlang::abort(
        paste0(
          "Missing required column(s) in `", data_name, "`: ",
          paste0("`", missing_cols, "`", collapse = ", ")
        )
      )
    }

    invisible(NULL)
  }

  check_required_cols(caton, c(by, "Catchkg"), "caton")
  check_required_cols(canum, c(by, "Number_Total", "WeightLive_Mean"), "canum")

  a <- caton %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
    dplyr::summarise(
      Caton = sum(.data$Catchkg, na.rm = TRUE),
      .groups = "drop"
    )

  b <- canum %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) %>%
    dplyr::summarise(
      Sop = sum(.data$Number_Total * .data$WeightLive_Mean, na.rm = TRUE) / 1000,
      .groups = "drop"
    )

  dplyr::inner_join(a, b, by = by) %>%
    dplyr::mutate(SopR = .data$Sop / .data$Caton)
}
