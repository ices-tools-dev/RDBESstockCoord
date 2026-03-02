#' Compute discards ratio and related overviews
#'
#' This function takes a stock overview data frame in long format, pivots it to
#' wide format by catch category, computes the discards ratio, adds strata
#' information, and returns several derived data frames.
#'
#' The input is expected to contain at least landings and (optionally) discards
#' totals by stock, metier, fleet and other strata. Landings and discards are
#' stored in the column `total` and split by `catchCategory` (e.g. "LAN", "DIS"),
#' which are reshaped to `LAN_total` and `DIS_total` internally.
#'
#' @param df_stock_overview A data frame containing stock overview data in long
#'   format, with columns including `catchCategory` and `total`, as well as
#'   stratification variables such as `vesselFlagCountry`, `year`,
#'   `workingGroup`, `stock`, `speciesCode`, `seasonType`, `seasonValue`,
#'   `areaType`, `areaValue`, `fisheriesManagementUnit`, `metier6`, `fleetType`,
#'   `fleetValue`, `domainCatchDis`, `domainCatchBMS`, `domainBiology`,
#'   `sourceType`, `variableType`, `variableUnit`, `variance`, `PSU`,
#'   `numPSUs`, `numTrips`, and `comment`.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Removes duplicate rows based on a set of key variables, keeping all
#'   associated columns.
#'   \item Drops columns that are entirely `NA` and PSU/trip information
#'   (`PSU`, `numPSUs`, `numTrips`).
#'   \item Pivots `catchCategory` and `total` to wide format (e.g. `LAN_total`,
#'   `DIS_total`).
#'   \item Sets missing `LAN_total` to 0 and coerces `LAN_total` and `DIS_total`
#'   to numeric.
#'   \item Computes `discards_ratio` as:
#'     \itemize{
#'       \item 1 when `LAN_total == 0` and `DIS_total > 0`;
#'       \item 0 when `LAN_total == 0` and `DIS_total == 0`;
#'       \item `DIS_total / LAN_total` otherwise.
#'     }
#'   \item If discards are not present, sets `DIS_total = 0` and
#'   `discards_ratio = 0`.
#'   \item Calls \code{census_add_strata()} to add strata information.
#'   \item Renames `LAN_total` to `Landings_Catchkg` and `fleetValue` to
#'   `Fleets`, and removes columns starting with `"domain"`.
#'   \item Splits the result into data with and without `discards_ratio` and
#'   extracts strata without discards.
#' }
#'
#' @return A named list with four elements:
#' \describe{
#'   \item{dfw_stock_overview}{The full wide-format data frame with
#'   `Landings_Catchkg`, `DIS_total`, `discards_ratio`, and strata information.}
#'   \item{dfw_stock_overview_L_D}{Subset of \code{dfw_stock_overview} with
#'   non-missing `discards_ratio`.}
#'   \item{dfw_stock_overview_L_noD}{Subset of \code{dfw_stock_overview} with
#'   missing `discards_ratio`, with the `discards_ratio` column removed.}
#'   \item{strata_no_discards}{A data frame with the `strata_full` column for
#'   strata without discards.}
#' }
#'
#' @seealso \code{\link[census_add_strata]{census_add_strata}}
#'
#' @examples
#' \dontrun{
#' res <- compute_discards_ratio(df_stock_overview)
#' res$dfw_stock_overview
#' res$dfw_stock_overview_L_D
#' res$dfw_stock_overview_L_noD
#' res$strata_no_discards
#' }
#'
#' @export
#' Compute discards ratio and related overviews
#'
#' This function takes a stock overview data frame in long format, pivots it to
#' wide format by catch category, computes the discards ratio, adds strata
#' information, and returns several derived data frames.
#'
#' The input is expected to contain at least landings and (optionally) discards
#' totals by stock, metier, fleet and other strata. Landings and discards are
#' stored in the column `total` and split by `catchCategory` (e.g. "LAN", "DIS"),
#' which are reshaped to `LAN_total` and `DIS_total` internally.
#'
#' @param df_stock_overview A data frame containing stock overview data in long
#'   format, with columns including `catchCategory` and `total`, as well as
#'   stratification variables such as `vesselFlagCountry`, `year`,
#'   `workingGroup`, `stock`, `speciesCode`, `seasonType`, `seasonValue`,
#'   `areaType`, `areaValue`, `fisheriesManagementUnit`, `metier6`, `fleetType`,
#'   `fleetValue`, `domainCatchDis`, `domainCatchBMS`, `domainBiology`,
#'   `sourceType`, `variableType`, `variableUnit`, `variance`, `PSU`,
#'   `numPSUs`, `numTrips`, and `comment`.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Removes duplicate rows based on a set of key variables, keeping all
#'   associated columns.
#'   \item Drops columns that are entirely `NA` and PSU/trip information
#'   (`PSU`, `numPSUs`, `numTrips`).
#'   \item Pivots `catchCategory` and `total` to wide format (e.g. `LAN_total`,
#'   `DIS_total`).
#'   \item Sets missing `LAN_total` to 0 and coerces `LAN_total` and `DIS_total`
#'   to numeric.
#'   \item Computes `discards_ratio` as:
#'     \itemize{
#'       \item 1 when `LAN_total == 0` and `DIS_total > 0`;
#'       \item 0 when `LAN_total == 0` and `DIS_total == 0`;
#'       \item `DIS_total / LAN_total` otherwise.
#'     }
#'   \item If discards are not present, sets `DIS_total = 0` and
#'   `discards_ratio = 0`.
#'   \item Calls \code{census_add_strata()} to add strata information.
#'   \item Renames `LAN_total` to `Landings_Catchkg` and `fleetValue` to
#'   `Fleets`, and removes columns starting with `"domain"`.
#'   \item Splits the result into data with and without `discards_ratio` and
#'   extracts strata without discards.
#' }
#'
#' @return A named list with four elements:
#' \describe{
#'   \item{dfw_stock_overview}{The full wide-format data frame with
#'   `Landings_Catchkg`, `DIS_total`, `discards_ratio`, and strata information.}
#'   \item{dfw_stock_overview_L_D}{Subset of \code{dfw_stock_overview} with
#'   non-missing `discards_ratio`.}
#'   \item{dfw_stock_overview_L_noD}{Subset of \code{dfw_stock_overview} with
#'   missing `discards_ratio`, with the `discards_ratio` column removed.}
#'   \item{strata_no_discards}{A data frame with the `strata_full` column for
#'   strata without discards.}
#' }
#'
#' @seealso \code{\link[census_add_strata]{census_add_strata}}
#'
#' @examples
#' \dontrun{
#' res <- compute_discards_ratio(df_stock_overview)
#' res$dfw_stock_overview
#' res$dfw_stock_overview_L_D
#' res$dfw_stock_overview_L_noD
#' res$strata_no_discards
#' }
#'
#' @export
split_L_with_without_D <- function(cef_catches,
                                   compute_discard_ration = TRUE,
                                   sourceType = "WGValue",
                                   domain = "domainCatchDis",
                                   add_strata = TRUE) {

  # 0. Keep only rows with domain selected
  df_stock_overview <- cef_catches %>%
    dplyr::filter(!is.na(!!rlang::sym(domain))) %>%
    dplyr::select(-dplyr::starts_with("domain"), !!rlang::sym(domain)) %>%
    # 0. Filter the sourceType
    filter(sourceType == "WGValue") %>%
    mutate(total = as.numeric(total))

  # 1. Check if duplicated rows
  dup_rows <- df_stock_overview[duplicated(df_stock_overview), ]

  if (nrow(dup_rows) > 0) {
    print(dup_rows)  # or message(), or write.csv(), etc.
    stop("Data contains duplicated rows.")
  }

  # 2. Pivot to wide for LAN/DIS totals
  dfw_stock_overview <- df_stock_overview %>%
    tidyr::pivot_wider(
      names_from  = "catchCategory",
      values_from = "total",
      names_glue  = "{catchCategory}_{.value}",
      values_fill = NA,
      values_fn = ~ sum(.x, na.rm = TRUE)
    ) %>%
    dplyr::mutate(
      LAN_total = ifelse(is.na(LAN_total), 0, LAN_total),
      LAN_total = as.numeric(LAN_total),
      DIS_total = as.numeric(DIS_total)
    )

  # 3. Compute discards_ratio (or set to 0 if DIS not present)
  if ("DIS_total" %in% names(dfw_stock_overview)) {
    dfw_stock_overview <- dfw_stock_overview %>%
      dplyr::mutate(
        discards_ratio = dplyr::case_when(
          LAN_total == 0 & DIS_total > 0  ~ 1,
          LAN_total == 0 & DIS_total == 0 ~ 0,
          TRUE ~ DIS_total / LAN_total
        )
      )
  } else {
    dfw_stock_overview <- df_stock_overview %>%
      dplyr::mutate(
        DIS_total      = 0,
        discards_ratio = 0
      )
  }

  # 4. add strata if necessary
  if(add_strata){
    dfw_stock_overview <- dfw_stock_overview %>%
      census_add_strata()
  }

  # 5. Split into with/without discards
  dfw_stock_overview_L_D <- dfw_stock_overview %>%
    dplyr::filter(!is.na(discards_ratio))

  dfw_stock_overview_L_noD <- dfw_stock_overview %>%
    dplyr::filter(is.na(discards_ratio)) %>%
    dplyr::select(-discards_ratio)

  strata_no_discards <- dfw_stock_overview_L_noD %>%
    dplyr::select(strata_full)

  # 6. Return everything you need as a list
  list(
    dfw_stock_overview       = dfw_stock_overview,
    dfw_stock_overview_L_D   = dfw_stock_overview_L_D,
    dfw_stock_overview_L_noD = dfw_stock_overview_L_noD,
    strata_no_discards      = strata_no_discards
  )
}
