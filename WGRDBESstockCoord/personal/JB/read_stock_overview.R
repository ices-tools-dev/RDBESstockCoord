read_stock_overview <- function(path_data_intercatch) {
  
  ### import data StockOverview from IC
  file_stock_overview_raw <-  glue::glue("{path_data_intercatch}/StockOverview.txt")
  
  ### read file
  df_stock_overview <- read.delim(file_stock_overview_raw) %>%
    tidyr::as_tibble() %>%
    dplyr::rename( "Catchkg" = "Catch..kg",
                   "CatchCat" = "Catch.Cat.",
                   "ReportCat" = "Report.cat.",
                   "SeasonType" = "Season.type",
                   "DiscardsImportedOrRaised" = "Discards.Imported.Or.Raised") %>%
    dplyr::filter(!(CatchCat %in% c("BMS landing", "Logbook Registered Discard"))) %>%
    dplyr::mutate(dplyr::across(where(is.factor), forcats::fct_drop))
  
  ### pivot data frame for computing discard ratio
  dfw_stock_overview <- df_stock_overview %>%
    tidyr::pivot_wider(., 
                       names_from =  "CatchCat",
                       values_from = "Catchkg",
                       names_glue = "{CatchCat}_{.value}",
                       values_fill = NA) %>%
    dplyr::mutate(Landings_Catchkg = ifelse(is.na(Landings_Catchkg),
                                            0,
                                            Landings_Catchkg))
  
  ### check if discards are in the data
  if ("Discards_Catchkg" %in% names(dfw_stock_overview)) {
    dfw_stock_overview <- dfw_stock_overview %>%
      dplyr::mutate(discards_ratio = dplyr::case_when(Landings_Catchkg == 0 & Discards_Catchkg > 0 ~ 1,
                                                      Landings_Catchkg == 0 & Discards_Catchkg == 0 ~ 0,
                                                      TRUE ~ Discards_Catchkg/Landings_Catchkg))
  } else {
    dfw_stock_overview <- df_stock_overview %>% 
      dplyr::mutate(Discards_Catchkg = 0, 
                    discards_ratio = 0)
  }
  
  return(list(df_stock_overview = df_stock_overview,
              dfw_stock_overview = dfw_stock_overview))
}
