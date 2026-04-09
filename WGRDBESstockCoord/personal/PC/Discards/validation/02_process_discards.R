
###############################################################################
## MODULE 02: Gap Diagnosis & Labeling
## PURPOSE: Analyze data coverage and label strata for the raising process
################################################################################


process_discards <- function(census, thresh_val = 50) {
  # OPCIÓN A: usar siempre lo que viene como argumento
  # (recomendado; comenta o borra la lectura de fichero)
  # census <- read_csv("outputs/census_validation.csv")
require (dplyr)
  require (tidyr)
  require(data.table)
  setup_discards <- census %>%
    # Nos quedamos solo con lo necesario
    select(
      vesselFlagCountry, year, workingGroup, stock, domainBiology,
      quarter, area, metier_group,
      catchCategory, OfficialWeight #, SampleID
    ) %>%
    # Pasar de LONG (LAN/Dis) a WIDE (columnas LAN y Dis)
    pivot_wider(
      names_from  = catchCategory,
      values_from = OfficialWeight,
      values_fn   = sum,      # suma por si hay varias filas
      values_fill = 0
    ) %>%
    # Renombrar para claridad (ajusta mayúsculas según tus datos reales)
    rename(
      total_landings = Lan,
      total_discards = Dis
    ) %>%
    group_by(year, stock, domainBiology, quarter, area, metier_group) %>%
    summarise(
      total_landings  = sum(total_landings,  na.rm = TRUE),
      total_discards  = sum(total_discards,  na.rm = TRUE),
      # N_Samples       = n_distinct(SampleID, na.rm = TRUE),
      .groups         = "drop"
    ) %>%
    mutate(
      # Cobertura solo cuando hay landings > 0 y descartes no NA
      coverage = ifelse(
        total_landings > 0 & !is.na(total_discards),
        (total_discards / total_landings) * 100,
        0
      ),
      Status = case_when(
        (is.na(total_landings) | total_landings == 0) &
          total_discards > 0 ~ "Discard Only: No Landings",
        (is.na(total_landings) | total_landings == 0) ~ "No Activity",
        is.na(total_discards) | total_discards == 0   ~ "NO Discard Data",
        coverage >  thresh_val                        ~ paste("SAFE: >",  thresh_val, " Coverage"),
        coverage <= thresh_val & coverage > 0         ~ paste("WARNING: <", thresh_val, " Coverage"),
        TRUE                                          ~ "Other / Check"
      )
    ) %>% as.data.frame()

  # EXPORTAR LA TABLA RESUMEN
  fwrite(setup_discards, "outputs/raising_gap_analysis.csv")

  # IMPORTANTE: devolver el resultado
  return(setup_discards)
}
setup_discards<- process_discards(census, thresh_val = 50)

