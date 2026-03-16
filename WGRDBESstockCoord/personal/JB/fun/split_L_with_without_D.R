#' Compute Discards ratio and related overviews
#'
#' This function takes a stock overview data frame in long format, pivots it to
#' wide format by catch category, computes the Discards ratio, adds strata
#' information, and returns several derived data frames.
#'
#' The input is expected to contain at least Landings and (optionally) Discards
#' totals by stock, metier, fleet and other strata. Landings and Discards are
#' stored in the column `total` and split by `catchCategory` (e.g. "Lan", "Dis"),
#' which are reshaped to `Lan_total` and `Dis_total` internally.
#'
#' @param df_stock_overview A data frame containing stock overview data in long
#'   format, with columns including `catchCategory` and `total`, as well as
#'   stratification variables such as `vesselFlagCountry`, `year`,
#'   `workingGroup`, `stock`, `speciesCode`, `seasonType`, `seasonValue`,
#'   `areaType`, `areaValue`, `fisheriesManagementUnit`, `metier6`, `fleetType`,
#'   `fleetValue`, `domainCatchDis`, `domainCatchBMS`, `domainBiology`,
#'   `originType`, `variableType`, `variableUnit`, `variance`, `PSU`,
#'   `numPSUs`, `numTrips`, and `comment`.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Removes duplicate rows based on a set of key variables, keeping all
#'   associated columns.
#'   \item Drops columns that are entirely `NA` and PSU/trip information
#'   (`PSU`, `numPSUs`, `numTrips`).
#'   \item Pivots `catchCategory` and `total` to wide format (e.g. `Lan_total`,
#'   `Dis_total`).
#'   \item Sets missing `Lan_total` to 0 and coerces `Lan_total` and `Dis_total`
#'   to numeric.
#'   \item Computes `Discards_ratio` as:
#'     \itemize{
#'       \item 1 when `Lan_total == 0` and `Dis_total > 0`;
#'       \item 0 when `Lan_total == 0` and `Dis_total == 0`;
#'       \item `Dis_total / Lan_total` otherwise.
#'     }
#'   \item If Discards are not present, sets `Dis_total = 0` and
#'   `Discards_ratio = 0`.
#'   \item Calls \code{census_add_strata()} to add strata information.
#'   \item Renames `Lan_total` to `Landings_Catchkg` and `fleetValue` to
#'   `Fleets`, and removes columns starting with `"domain"`.
#'   \item Splits the result into data with and without `Discards_ratio` and
#'   extracts strata without Discards.
#' }
#'
#' @return A named list with four elements:
#' \describe{
#'   \item{dfw_stock_overview}{The full wide-format data frame with
#'   `Landings_Catchkg`, `Dis_total`, `Discards_ratio`, and strata information.}
#'   \item{dfw_stock_overview_L_D}{Subset of \code{dfw_stock_overview} with
#'   non-missing `Discards_ratio`.}
#'   \item{dfw_stock_overview_L_noD}{Subset of \code{dfw_stock_overview} with
#'   missing `Discards_ratio`, with the `Discards_ratio` column removed.}
#'   \item{strata_no_Discards}{A data frame with the `strata_full` column for
#'   strata without Discards.}
#' }
#'
#' @seealso \code{\link[census_add_strata]{census_add_strata}}
#'
#' @examples
#' \dontrun{
#' res <- compute_Discards_ratio(df_stock_overview)
#' res$dfw_stock_overview
#' res$dfw_stock_overview_L_D
#' res$dfw_stock_overview_L_noD
#' res$strata_no_Discards
#' }
#'
#' @export
#' Compute Discards ratio and related overviews
#'
#' This function takes a stock overview data frame in long format, pivots it to
#' wide format by catch category, computes the Discards ratio, adds strata
#' information, and returns several derived data frames.
#'
#' The input is expected to contain at least Landings and (optionally) Discards
#' totals by stock, metier, fleet and other strata. Landings and Discards are
#' stored in the column `total` and split by `catchCategory` (e.g. "Lan", "Dis"),
#' which are reshaped to `Lan_total` and `Dis_total` internally.
#'
#' @param df_stock_overview A data frame containing stock overview data in long
#'   format, with columns including `catchCategory` and `total`, as well as
#'   stratification variables such as `vesselFlagCountry`, `year`,
#'   `workingGroup`, `stock`, `speciesCode`, `seasonType`, `seasonValue`,
#'   `areaType`, `areaValue`, `fisheriesManagementUnit`, `metier6`, `fleetType`,
#'   `fleetValue`, `domainCatchDis`, `domainCatchBMS`, `domainBiology`,
#'   `originType`, `variableType`, `variableUnit`, `variance`, `PSU`,
#'   `numPSUs`, `numTrips`, and `comment`.
#'
#' @details
#' The function:
#' \itemize{
#'   \item Removes duplicate rows based on a set of key variables, keeping all
#'   associated columns.
#'   \item Drops columns that are entirely `NA` and PSU/trip information
#'   (`PSU`, `numPSUs`, `numTrips`).
#'   \item Pivots `catchCategory` and `total` to wide format (e.g. `Lan_total`,
#'   `Dis_total`).
#'   \item Sets missing `Lan_total` to 0 and coerces `Lan_total` and `Dis_total`
#'   to numeric.
#'   \item Computes `Discards_ratio` as:
#'     \itemize{
#'       \item 1 when `Lan_total == 0` and `Dis_total > 0`;
#'       \item 0 when `Lan_total == 0` and `Dis_total == 0`;
#'       \item `Dis_total / Lan_total` otherwise.
#'     }
#'   \item If Discards are not present, sets `Dis_total = 0` and
#'   `Discards_ratio = 0`.
#'   \item Calls \code{census_add_strata()} to add strata information.
#'   \item Renames `Lan_total` to `Landings_Catchkg` and `fleetValue` to
#'   `Fleets`, and removes columns starting with `"domain"`.
#'   \item Splits the result into data with and without `Discards_ratio` and
#'   extracts strata without Discards.
#' }
#'
#' @return A named list with four elements:
#' \describe{
#'   \item{dfw_stock_overview}{The full wide-format data frame with
#'   `Landings_Catchkg`, `Dis_total`, `Discards_ratio`, and strata information.}
#'   \item{dfw_stock_overview_L_D}{Subset of \code{dfw_stock_overview} with
#'   non-missing `Discards_ratio`.}
#'   \item{dfw_stock_overview_L_noD}{Subset of \code{dfw_stock_overview} with
#'   missing `Discards_ratio`, with the `Discards_ratio` column removed.}
#'   \item{strata_no_Discards}{A data frame with the `strata_full` column for
#'   strata without Discards.}
#' }
#'
#' @seealso \code{\link[census_add_strata]{census_add_strata}}
#'
#' @examples
#' \dontrun{
#' res <- compute_Discards_ratio(df_stock_overview)
#' res$dfw_stock_overview
#' res$dfw_stock_overview_L_D
#' res$dfw_stock_overview_L_noD
#' res$strata_no_Discards
#' }
#'
#' @export
split_L_with_without_D <- function(cef_catches,
                                   originType = "WGEstimate",
                                   variableType = "WeightLive",
                                   domain = "domainCatchDis",
                                   type = "discards",
                                   add_strata = TRUE) {

  estField <- case_when(type == "discards" ~ "domainCatchDis",
                        type == "bms" ~ "domainCatchBMS",
                        TRUE ~ NA)

  ## Domains that need to be set to NA for raised data:
  otherDomains <- c("domainCatchDis", "domainCatchBMS", "domainBiology") %>%
    {.[! . %in% estField]}

  estCateg <- case_when(type == "discards" ~ "Dis",
                        type == "bms" ~ "Bms",
                        TRUE ~ NA)


  # 0. Keep only rows with domain selected
  df_stock_overview <- cef_catches %>%
    dplyr::filter(!is.na(!!rlang::sym(domain)),
                  (!is.na(!!rlang::sym(domain)) & is.na(domainBiology))) %>%
    dplyr::select(-dplyr::starts_with("domain"), !!rlang::sym(domain)) %>%
    # 0. Filter the originType
    dplyr::filter(originType %in% {{originType}}) %>%
    mutate(total = as.numeric(total)) %>%
    bind_rows(tibble(importedOrRaised = character())) %>%
    cef_add_strata(., only_full_strata = TRUE)

  ## Identify categories with estimates:
  reported_catch_cat <- df_stock_overview %>%
    filter(catchCategory %in% c(estCateg),
           ! is.na(total),
           originType %in% {{originType}},
           variableType %in% {{variableType}},
           ! is.na(!!sym(estField))) %>%
    dplyr::rename(all_of(c("domainCatch" = estField))) %>%
    dplyr::select(vesselFlagCountry:catchCategory, domainCatch, originType, variableType, total)


  # 1. Check if duplicated rows
  dup_rows <- df_stock_overview[duplicated(df_stock_overview), ]

  if (nrow(dup_rows) > 0) {
    print(dup_rows)  # or message(), or write.csv(), etc.
    stop("Data contains duplicated rows.")
  }

  # 2. Pivot to wide for Lan/Dis totals
  # 2.1 Before pivoting remove info on discards sampling ortherwise duplicadted rows
  df_stock_overview_temp <- df_stock_overview %>%
    dplyr::select(-any_of(c("Variance", "PSU",
                            "numPSUs", "numTrips", "comment")))

  dfw_stock_overview <- df_stock_overview_temp %>%
    tidyr::pivot_wider(
      names_from  = "catchCategory",
      values_from = "total",
      names_glue  = "{catchCategory}_{.value}",
      values_fill = NA,
      values_fn = ~ sum(.x, na.rm = FALSE)
    ) %>%
    dplyr::mutate(
      Lan_total = ifelse(is.na(Lan_total), 0, Lan_total),
      Lan_total = as.numeric(Lan_total),
      Dis_total = as.numeric(Dis_total),
      total_landings = sum(Lan_total)
    )

  # 3. Compute Discards_ratio (or set to 0 if Dis not present)
  if ("Dis_total" %in% names(dfw_stock_overview)) {
    dfw_stock_overview <- dfw_stock_overview %>%
      dplyr::mutate(
        Discards_ratio = dplyr::case_when(
          Lan_total == 0 & Dis_total > 0  ~ 1,
          Lan_total == 0 & Dis_total == 0 ~ 0,
          is.na(Lan_total) | is.na(Dis_total) ~ NA,
          TRUE ~ Dis_total / Lan_total
        )
      )
  } else {
    dfw_stock_overview <- df_stock_overview %>%
      dplyr::mutate(
        Dis_total      = 0,
        Discards_ratio = 0
      )
  }

  # 4. add strata if necessary
  if(add_strata){
    dfw_stock_overview <- dfw_stock_overview %>%
      cef_add_strata()
  }

  # 5. Split into with/without Discards
  dfw_stock_overview_L_D <- dfw_stock_overview %>%
    dplyr::filter(!is.na(Discards_ratio)) %>%
    mutate(importedOrRaised = "imported")

  dfw_stock_overview_L_noD <- dfw_stock_overview %>%
    dplyr::filter(is.na(Discards_ratio)) %>%
    dplyr::select(-Discards_ratio) %>%
    mutate(importedOrRaised = NA)

  strata_no_Discards <- dfw_stock_overview_L_noD %>%
    dplyr::select(strata_full)

  # 6. Return everything you need as a list
  list(
    dfw_stock_overview       = dfw_stock_overview,
    dfw_stock_overview_L_D   = dfw_stock_overview_L_D,
    dfw_stock_overview_L_noD = dfw_stock_overview_L_noD,
    strata_no_Discards      = strata_no_Discards
  )
}
