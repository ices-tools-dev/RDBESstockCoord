' Add Stratum Columns to Census Data
#'
#' This function creates various stratum combinations from census catch data
#' by combining different dimensions (country, seasonValue, fleetValue, area, gear levels)
#' to support stratified sampling and analysis procedures. It generates gear
#' classifications and creates all possible stratum combinations for flexible
#' data aggregation.
#'
#' @param df_census Data frame. Census catches data frame containing fisheries
#'   data with columns: vesselFlagCountry, seasonValue, fleetValue, area, stock, and
#'   metier6. Typically the output from \code{convExchange_tidy()} function.
#'
#' @details
#' The function performs several key operations:
#' \itemize{
#'   \item Creates gear classifications from metier6 codes:
#'     \itemize{
#'       \item \code{gear}: First 3 characters of metier6
#'       \item \code{super_gear}: First character of gear, with special mappings
#'     }
#'   \item Generates stratum combinations using all possible combinations of:
#'     \itemize{
#'       \item c = vesselFlagCountry
#'       \item q = seasonValue
#'       \item f = fleetValue
#'       \item a = area
#'       \item g = gear (3-character)
#'       \item sg = super_gear (1-character)
#'       \item lvl6 = metier6 (full 6-character code)
#'     }
#'   \item Converts fleetValue to factor for consistent handling
#'   \item Creates a full stratum identifier combining stock and all dimensions
#' }
#'
#' @section Gear Classification Logic:
#' The function creates hierarchical gear classifications:
#' \itemize{
#'   \item \strong{gear}: First 3 characters of metier6 (e.g., "OTM", "GNS")
#'   \item \strong{super_gear}: Simplified gear categories with special mappings:
#'     \itemize{
#'       \item "OTM" → "P" (Pelagic trawl)
#'       \item "TTB" → "O" (Other trawl)
#'       \item "TBB" → "O" (Other trawl)
#'       \item "PTB" → "O" (Other trawl)
#'       \item "PS" → "S" (Seine)
#'       \item Others → First character of gear
#'     }
#' }
#'
#' @section Stratum Naming Convention:
#' Stratum names follow the pattern \code{stratum_[dimensions]_[gear_level]}:
#' \itemize{
#'   \item Base combinations: c, q, f, a and their combinations
#'   \item Gear-enhanced: same combinations + g (gear)
#'   \item Super-gear-enhanced: same combinations + sg (super_gear)
#'   \item Level-6-enhanced: same combinations + lvl6 (metier6)
#' }
#'
#' Examples: \code{stratum_c_q} (country-seasonValue), \code{stratum_f_a_g} (fleetValue-area-gear)
#'
#' @returns
#' Data frame. The input data frame with additional columns:
#' \itemize{
#'   \item \code{fleetValue}: Converted to factor
#'   \item \code{stratum_full}: Complete stratum identifier with stock
#'   \item \code{gear}: 3-character gear code from metier6
#'   \item \code{super_gear}: 1-character simplified gear category
#'   \item \code{stratum_*}: 63 different stratum combination columns
#' }
#'
#' @section Required Packages:
#' The function requires the following packages:
#' \itemize{
#'   \item \code{dplyr}: For data manipulation
#'   \item \code{forcats}: For factor handling
#'   \item \code{glue}: For string interpolation
#'   \item \code{stringr}: For string manipulation
#' }
#'
#' @section Input Data Requirements:
#' The input data frame must contain the following columns:
#' \itemize{
#'   \item \code{vesselFlagCountry}: Country code
#'   \item \code{seasonValue}: Time seasonValue
#'   \item \code{fleetValue}: fleetValue identifier
#'   \item \code{area}: Fishing area
#'   \item \code{stock}: Stock identifier
#'   \item \code{metier6}: 6-character metier code
#' }
#'
#' @examples
#' \dontrun{
#' # Add stratum columns to census data
#' census_with_strata <- census_add_stratum(census_catches)
#'
#' # View the new stratum columns
#' names(census_with_strata)[grepl("stratum_", names(census_with_strata))]
#'
#' # Example: aggregate by country-seasonValue stratum
#' library(dplyr)
#' census_with_strata %>%
#'   group_by(stratum_c_q) %>%
#'   summarise(total_catch = sum(total, na.rm = TRUE))
#' }
#'
#' @seealso
#' \code{\link{census_remove_stratum}} to remove stratum columns
#' \code{\link{convExchange_tidy}} to generate census data
#'
#' @author Jean-Baptiste Lecomte
#' @export
#'
census_add_stratum <- function(df_census) {
  df_census <- df_census %>%
    dplyr::mutate(
      fleetValue = forcats::as_factor(.data$fleetValue),
      stratum_full = forcats::as_factor(glue::glue("{stock}{vesselFlagCountry}-{seasonValue}-{areaValue}-{fleetValue}-{metier6}")),
      gear = stringr::str_sub(string = .data$metier6, start = 1,  end = 3),
      super_gear = stringr::str_sub(string = .data$gear, start = 1, end = 1),
      super_gear = dplyr::case_when(gear == "OTM" ~ "P",
                                    gear == "TTB" ~ "O",
                                    gear == "TBB" ~ "O",
                                    gear == "PTB" ~ "O",
                                    gear == "PS" ~ "S",
                                    TRUE ~ super_gear),

      # Top combinations
      stratum_c_q_f_a = glue::glue("{vesselFlagCountry}-{seasonValue}-{fleetValue}-{areaValue}"),
      stratum_c_q_f = glue::glue("{vesselFlagCountry}-{seasonValue}-{fleetValue}"),
      stratum_c_q_a = glue::glue("{vesselFlagCountry}-{seasonValue}-{areaValue}"),
      stratum_c_q = glue::glue("{vesselFlagCountry}-{seasonValue}"),
      stratum_c_f_a = glue::glue("{vesselFlagCountry}-{fleetValue}-{areaValue}"),
      stratum_c_f = glue::glue("{vesselFlagCountry}-{fleetValue}"),
      stratum_c_a = glue::glue("{vesselFlagCountry}-{areaValue}"),
      stratum_c = glue::glue("{vesselFlagCountry}"),
      stratum_q_f_a = glue::glue("{seasonValue}-{fleetValue}-{areaValue}"),
      stratum_q_f = glue::glue("{seasonValue}-{fleetValue}"),
      stratum_q_a = glue::glue("{seasonValue}-{areaValue}"),
      stratum_q = glue::glue("{seasonValue}"),
      stratum_f_a = glue::glue("{fleetValue}-{areaValue}"),
      stratum_f = glue::glue("{fleetValue}"),
      stratum_a = glue::glue("{areaValue}"),

      # Combinations with gear
      stratum_c_q_f_a_g = glue::glue("{vesselFlagCountry}-{seasonValue}-{fleetValue}-{areaValue}-{gear}"),
      stratum_c_q_f_g = glue::glue("{vesselFlagCountry}-{seasonValue}-{fleetValue}-{gear}"),
      stratum_c_q_a_g = glue::glue("{vesselFlagCountry}-{seasonValue}-{areaValue}-{gear}"),
      stratum_c_q_g = glue::glue("{vesselFlagCountry}-{seasonValue}-{gear}"),
      stratum_c_f_a_g = glue::glue("{vesselFlagCountry}-{fleetValue}-{areaValue}-{gear}"),
      stratum_c_f_g = glue::glue("{vesselFlagCountry}-{fleetValue}-{gear}"),
      stratum_c_a_g = glue::glue("{vesselFlagCountry}-{areaValue}-{gear}"),
      stratum_c_g = glue::glue("{vesselFlagCountry}-{gear}"),
      stratum_q_f_a_g = glue::glue("{seasonValue}-{fleetValue}-{areaValue}-{gear}"),
      stratum_q_f_g = glue::glue("{seasonValue}-{fleetValue}-{gear}"),
      stratum_q_a_g = glue::glue("{seasonValue}-{areaValue}-{gear}"),
      stratum_q_g = glue::glue("{seasonValue}-{gear}"),
      stratum_f_a_g = glue::glue("{fleetValue}-{areaValue}-{gear}"),
      stratum_f_g = glue::glue("{fleetValue}-{gear}"),
      stratum_a_g = glue::glue("{areaValue}-{gear}"),
      stratum_g = glue::glue("{gear}"),

      # Combinations with super_gear
      stratum_c_q_f_a_sg = glue::glue("{vesselFlagCountry}-{seasonValue}-{fleetValue}-{areaValue}-{super_gear}"),
      stratum_c_q_f_sg = glue::glue("{vesselFlagCountry}-{seasonValue}-{fleetValue}-{super_gear}"),
      stratum_c_q_a_sg = glue::glue("{vesselFlagCountry}-{seasonValue}-{areaValue}-{super_gear}"),
      stratum_c_q_sg = glue::glue("{vesselFlagCountry}-{seasonValue}-{super_gear}"),
      stratum_c_f_a_sg = glue::glue("{vesselFlagCountry}-{fleetValue}-{areaValue}-{super_gear}"),
      stratum_c_f_sg = glue::glue("{vesselFlagCountry}-{fleetValue}-{super_gear}"),
      stratum_c_a_sg = glue::glue("{vesselFlagCountry}-{areaValue}-{super_gear}"),
      stratum_c_sg = glue::glue("{vesselFlagCountry}-{super_gear}"),
      stratum_q_f_a_sg = glue::glue("{seasonValue}-{fleetValue}-{areaValue}-{super_gear}"),
      stratum_q_f_sg = glue::glue("{seasonValue}-{fleetValue}-{super_gear}"),
      stratum_q_a_sg = glue::glue("{seasonValue}-{areaValue}-{super_gear}"),
      stratum_q_sg = glue::glue("{seasonValue}-{super_gear}"),
      stratum_f_a_sg = glue::glue("{fleetValue}-{areaValue}-{super_gear}"),
      stratum_f_sg = glue::glue("{fleetValue}-{super_gear}"),
      stratum_a_sg = glue::glue("{areaValue}-{super_gear}"),
      stratum_sg = glue::glue("{super_gear}"),

      # Combinations with metier6
      stratum_c_q_f_a_lvl6 = glue::glue("{vesselFlagCountry}-{seasonValue}-{fleetValue}-{areaValue}-{metier6}"),
      stratum_c_q_f_lvl6 = glue::glue("{vesselFlagCountry}-{seasonValue}-{fleetValue}-{metier6}"),
      stratum_c_q_a_lvl6 = glue::glue("{vesselFlagCountry}-{seasonValue}-{areaValue}-{metier6}"),
      stratum_c_q_lvl6 = glue::glue("{vesselFlagCountry}-{seasonValue}-{metier6}"),
      stratum_c_f_a_lvl6 = glue::glue("{vesselFlagCountry}-{fleetValue}-{areaValue}-{metier6}"),
      stratum_c_f_lvl6 = glue::glue("{vesselFlagCountry}-{fleetValue}-{metier6}"),
      stratum_c_a_lvl6 = glue::glue("{vesselFlagCountry}-{areaValue}-{metier6}"),
      stratum_c_lvl6 = glue::glue("{vesselFlagCountry}-{metier6}"),
      stratum_q_f_a_lvl6 = glue::glue("{seasonValue}-{fleetValue}-{areaValue}-{metier6}"),
      stratum_q_f_lvl6 = glue::glue("{seasonValue}-{fleetValue}-{metier6}"),
      stratum_q_a_lvl6 = glue::glue("{seasonValue}-{areaValue}-{metier6}"),
      stratum_q_lvl6 = glue::glue("{seasonValue}-{metier6}"),
      stratum_f_a_lvl6 = glue::glue("{fleetValue}-{areaValue}-{metier6}"),
      stratum_f_lvl6 = glue::glue("{fleetValue}-{metier6}"),
      stratum_a_lvl6 = glue::glue("{areaValue}-{metier6}"),
      stratum_lvl6 = glue::glue("{metier6}")
    )
  return(df_census)
}

#' Remove Stratum Columns from Census Data
#'
#' This function removes all stratum-related columns from a census data frame,
#' keeping only the \code{stratum_full} column. This is useful for cleaning up
#' data after stratified analysis or when stratum columns are no longer needed.
#'
#' @param df_census Data frame. Census data with stratum columns, typically
#'   the output from \code{census_add_stratum()} function.
#'
#' @details
#' The function removes all columns that start with "stratum_" except for
#' \code{stratum_full}. This includes:
#' \itemize{
#'   \item All basic stratum combinations (e.g., \code{stratum_c}, \code{stratum_q})
#'   \item All gear-enhanced combinations (e.g., \code{stratum_c_g})
#'   \item All super-gear combinations (e.g., \code{stratum_c_sg})
#'   \item All level-6 metier combinations (e.g., \code{stratum_c_lvl6})
#' }
#'
#' The \code{stratum_full} column is preserved as it contains the complete
#' stratum identifier that may be needed for referencing or joining operations.
#'
#' @returns
#' Data frame. The input data frame with all stratum columns removed except
#' \code{stratum_full}. The gear classification columns (\code{gear},
#' \code{super_gear}) are also removed if they were created by
#' \code{census_add_stratum()}.
#'
#' @section Required Packages:
#' The function requires the following packages:
#' \itemize{
#'   \item \code{dplyr}: For data manipulation and column selection
#' }
#'
#' @examples
#' \dontrun{
#' # Add stratum columns
#' census_with_strata <- census_add_stratum(census_catches)
#'
#' # Check column count before removal
#' ncol(census_with_strata)
#'
#' # Remove stratum columns
#' census_cleaned <- census_remove_stratum(census_with_strata)
#'
#' # Check column count after removal
#' ncol(census_cleaned)
#'
#' # Verify stratum_full is still present
#' "stratum_full" %in% names(census_cleaned)
#' }
#'
#' @seealso
#' \code{\link{census_add_stratum}} to add stratum columns
#'
#' @note
#' This function uses a logical OR operation in the \code{dplyr::select()}
#' statement to exclude columns starting with "stratum_" while keeping
#' \code{stratum_full}. The syntax \code{-starts_with("stratum_") | stratum_full}
#' means "exclude columns starting with 'stratum_' OR keep stratum_full".
#'
#' @author Jean-Baptiste Lecomte
#' @export
census_remove_stratum <- function(df_census) {
  df_census <- df_census %>%
    dplyr::select(-dplyr::starts_with("stratum_") | stratum_full)
  return(df_census)
}

###



#' Generate Unique Stratum Combinations from Census Data
#'
#' This function creates lists of unique stratum labels based on specified combinations
#' of hierarchical grouping columns from a census-like data frame. It also derives new
#' gear columns and supports optional selection of specific strata groupings.
#'
#' @param df_census A data frame containing census or survey data. Must include columns
#'        such as vesselFlagCountry, seasonValue, fleetValue, area, and metier6.
#' @param selected_strata Optional character vector of stratum names to limit the output.
#'        If NULL, all stratum combinations defined in the function will be used.
#'
#' @details
#' The function derives additional gear columns (`gear`, `super_gear`) from the `metier6`
#' column to support more granular or aggregated gear stratifications. It defines multiple
#' stratum groupings (e.g., by country, seasonValue, fleetValue, area, gear, etc.), and, for each,
#' produces a vector of unique string labels corresponding to present combinations in the data.
#' If `selected_strata` is provided, the output will be restricted to those groups, warning about
#' any requested group not defined in the mapping.
#'
#' @return A named list. Each entry is a character vector of unique stratum labels for the
#'         grouping, with values concatenated by "-".
#'
#' @examples
#' \dontrun{
#' result <- generate_stratum_combinations(df_census)
#' # Limit to specific strata
#' result <- generate_stratum_combinations(df_census, selected_strata = c('stratum_c_q_f_a', 'stratum_f'))
#' }
#'
#' @export
generate_stratum_combinations <- function(df_census, selected_strata = NULL) {
  # Prepare any derived columns needed for combinations (like gear, super_gear)
  df_processed <- dplyr::mutate(
    df_census,
    gear = stringr::str_sub(metier6, 1, 3),
    super_gear = stringr::str_sub(gear, 1, 1),
    super_gear = dplyr::case_when(
      gear == "OTM" ~ "P",
      gear %in% c("TTB", "TBB", "PTB") ~ "O",
      gear == "PS" ~ "S",
      TRUE ~ super_gear
    )
  )
  # Define lists of column sets with their stratum names
  strata_cols <- list(
    stratum_c_q_f_a = c("vesselFlagCountry", "seasonValue", "fleetValue", "areaValue"),
    stratum_c_q_f = c("vesselFlagCountry", "seasonValue", "fleetValue"),
    stratum_c_q_a = c("vesselFlagCountry", "seasonValue", "areaValue"),
    stratum_c_q = c("vesselFlagCountry", "seasonValue"),
    stratum_c_f_a = c("vesselFlagCountry", "fleetValue", "areaValue"),
    stratum_c_f = c("vesselFlagCountry", "fleetValue"),
    stratum_c_a = c("vesselFlagCountry", "areaValue"),
    stratum_c = c("vesselFlagCountry"),
    stratum_q_f_a = c("seasonValue", "fleetValue", "areaValue"),
    stratum_q_f = c("seasonValue", "fleetValue"),
    stratum_q_a = c("seasonValue", "areaValue"),
    stratum_q = c("seasonValue"),
    stratum_f_a = c("fleetValue", "areaValue"),
    stratum_f = c("fleetValue"),
    stratum_a = c("areaValue"),
    # with gear
    stratum_c_q_f_a_g = c("vesselFlagCountry", "seasonValue", "fleetValue", "areaValue", "gear"),
    stratum_c_q_f_g = c("vesselFlagCountry", "seasonValue", "fleetValue", "gear"),
    stratum_c_q_a_g = c("vesselFlagCountry", "seasonValue", "areaValue", "gear"),
    stratum_c_q_g = c("vesselFlagCountry", "seasonValue", "gear"),
    stratum_c_f_a_g = c("vesselFlagCountry", "fleetValue", "areaValue", "gear"),
    stratum_c_f_g = c("vesselFlagCountry", "fleetValue", "gear"),
    stratum_c_a_g = c("vesselFlagCountry", "areaValue", "gear"),
    stratum_c_g = c("vesselFlagCountry", "gear"),
    stratum_q_f_a_g = c("seasonValue", "fleetValue", "areaValue", "gear"),
    stratum_q_f_g = c("seasonValue", "fleetValue", "gear"),
    stratum_q_a_g = c("seasonValue", "areaValue", "gear"),
    stratum_q_g = c("seasonValue", "gear"),
    stratum_f_a_g = c("fleetValue", "areaValue", "gear"),
    stratum_f_g = c("fleetValue", "gear"),
    stratum_a_g = c("areaValue", "gear"),
    stratum_g = c("gear"),
    # with super_gear
    stratum_c_q_f_a_sg = c("vesselFlagCountry", "seasonValue", "fleetValue", "areaValue", "super_gear"),
    stratum_c_q_f_sg = c("vesselFlagCountry", "seasonValue", "fleetValue", "super_gear"),
    stratum_c_q_a_sg = c("vesselFlagCountry", "seasonValue", "areaValue", "super_gear"),
    stratum_c_q_sg = c("vesselFlagCountry", "seasonValue", "super_gear"),
    stratum_c_f_a_sg = c("vesselFlagCountry", "fleetValue", "areaValue", "super_gear"),
    stratum_c_f_sg = c("vesselFlagCountry", "fleetValue", "super_gear"),
    stratum_c_a_sg = c("vesselFlagCountry", "areaValue", "super_gear"),
    stratum_c_sg = c("vesselFlagCountry", "super_gear"),
    stratum_q_f_a_sg = c("seasonValue", "fleetValue", "areaValue", "super_gear"),
    stratum_q_f_sg = c("seasonValue", "fleetValue", "super_gear"),
    stratum_q_a_sg = c("seasonValue", "areaValue", "super_gear"),
    stratum_q_sg = c("seasonValue", "super_gear"),
    stratum_f_a_sg = c("fleetValue", "areaValue", "super_gear"),
    stratum_f_sg = c("fleetValue", "super_gear"),
    stratum_a_sg = c("areaValue", "super_gear"),
    stratum_sg = c("super_gear"),
    # with metier6
    stratum_c_q_f_a_lvl6 = c("vesselFlagCountry", "seasonValue", "fleetValue", "areaValue", "metier6"),
    stratum_c_q_f_lvl6 = c("vesselFlagCountry", "seasonValue", "fleetValue", "metier6"),
    stratum_c_q_a_lvl6 = c("vesselFlagCountry", "seasonValue", "areaValue", "metier6"),
    stratum_c_q_lvl6 = c("vesselFlagCountry", "seasonValue", "metier6"),
    stratum_c_f_a_lvl6 = c("vesselFlagCountry", "fleetValue", "areaValue", "metier6"),
    stratum_c_f_lvl6 = c("vesselFlagCountry", "fleetValue", "metier6"),
    stratum_c_a_lvl6 = c("vesselFlagCountry", "areaValue", "metier6"),
    stratum_c_lvl6 = c("vesselFlagCountry", "metier6"),
    stratum_q_f_a_lvl6 = c("seasonValue", "fleetValue", "areaValue", "metier6"),
    stratum_q_f_lvl6 = c("seasonValue", "fleetValue", "metier6"),
    stratum_q_a_lvl6 = c("seasonValue", "areaValue", "metier6"),
    stratum_q_lvl6 = c("seasonValue", "metier6"),
    stratum_f_a_lvl6 = c("fleetValue", "areaValue", "metier6"),
    stratum_f_lvl6 = c("fleetValue", "metier6"),
    stratum_a_lvl6 = c("areaValue", "metier6"),
    stratum_lvl6 = c("metier6")
  )
  # If selected_strata given, filter the list keys accordingly
  if (!is.null(selected_strata)) {
    missing_strata <- setdiff(selected_strata, names(strata_cols))
    if (length(missing_strata) > 0) {
      warning("Some selected strata names not found: ", paste(missing_strata, collapse = ", "))
    }
    strata_cols <- strata_cols[names(strata_cols) %in% selected_strata]
  }
  # For each defined combination, extract unique pasted labels
  strata_list <- purrr::map(strata_cols, function(cols) {
    present_cols <- base::intersect(cols, base::names(df_processed))
    if (length(present_cols) == 0) {
      return(character(0))
    }
    df_processed %>%
      dplyr::select(tidyselect::all_of(present_cols)) %>%
      dplyr::distinct() %>%
      tidyr::unite("stratum_label", tidyselect::everything(), sep = "-") %>%
      dplyr::pull(stratum_label)
  })
  return(strata_list)
}

