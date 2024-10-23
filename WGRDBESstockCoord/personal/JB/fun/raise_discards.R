###-----------------------------------------------------------------------------
### (1) First stratum allocation: Country, Season, Fleet (no Area)
###-----------------------------------------------------------------------------

raise_discards_strata <- function(data_landings_discards, 
                                  data_discards_to_raised, 
                                  grouping_strata = "stratum_c_s_f",
                                  weightingfactor = "CATON") {
    
  ### extract reference (landings + discards) data with the grouping strata 
  data_landings_discards_temp <- data_landings_discards %>%
    dplyr::group_by(!!grouping_strata) %>%
    dplyr::mutate(total_Landings_Catchkg = sum(Landings_Catchkg),
                  weightingfactor = Landings_Catchkg / total_Landings_Catchkg,
                  weightingfactor = ifelse(is.na(weightingfactor), 1,  weightingfactor)) %>%
    dplyr::select(!!grouping_strata, discards_ratio, weightingfactor)
  
  ### raise discards
  df_raised_discards_strata <- data_discards_to_raised %>%
  dplyr::filter(is.na(raised_strata)) %>%
  dplyr::inner_join(.,
            data_landings_discards_temp,
                                                 by = grouping_strata,
                                                 relationship = "many-to-many") %>%
    dplyr::mutate(raised_discards = Landings_Catchkg * discards_ratio * weightingfactor,
                  raised_discards = ifelse(Landings_Catchkg == 0, 0, raised_discards)) %>%
    dplyr::filter(!is.na(raised_discards)) %>%
    dplyr::group_by(.data$stratum_full) %>%
    dplyr::summarise(raised_discards = sum(raised_discards, na.rm = TRUE),
                     raised_landings = unique(Landings_Catchkg)) %>%
    droplevels() %>%
    dplyr::mutate(raised_strata = grouping_strata)
  
  data_discards_to_raised_output <- dplyr::left_join(dplyr::select(data_discards_to_raised, -raised_strata),
                                              df_raised_discards_strata)
return(data_discards_to_raised_output)
}


get_ratio_raise_discards_strata <- function(data_discards_raised) {
  df_stratum_raised <- data_discards_raised %>%
    group_by(raised_strata) %>%
    summarise(total_landings = sum(Landings_Catchkg),
              raised_landings = sum(raised_landings)) %>%
    mutate(percentage_landings_raised = raised_landings / total_landings)
  
}

