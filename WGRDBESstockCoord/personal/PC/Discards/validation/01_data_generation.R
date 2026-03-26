
################################################################################
## MODULE 01: Synthetic Data Generation
## PURPOSE: Create 'Census' and 'Estimates' datasets with controlled gaps
################################################################################

data_generation.R <- 
  function()  {
    # Create Output folder if it doesn't exist
    if(!dir.exists("outputs")) dir.create("outputs")
  
  census<-expand.grid(
    vesselFlagCountry = "ES",    
    year        = 2024,
    stock       = c("mac.27.nea", "pil.27.8c9a"),
    quarter     = 1:4,
    area        = c("27.9.a", "27.8.c", "27.7.j"),
    metier_group = c("OTB_DEF", "PS_SPF", "GNS_DEF", "GTR_DEF"),
    originType        = "Official",
    variableType      = "OfficialWeight"
  ) %>%
    mutate(
      domainCatchBMS=NA,
      domainBiology = paste (stock,quarter, area, metier_group, sep="_"),
      workingGroup = ifelse(stock == "mac.27.nea", "WGWIDE", "WGHANSA"),
      speciesCodeFAO       = ifelse(stock == "mac.27.nea", "MAC", "PIL")) %>% 
    # GTR_DEF solo para mac.27.nea
    filter(stock == "mac.27.nea" | metier_group != "GTR_DEF") %>%
    mutate(
      # Valores base para simular LAN y Dis
      land_val = runif(n(), 30, 12000),
      disc_val = runif(n(), 5, 1200),# land_val * runif(n(), 0.1, 0.3)
    ) %>%
    # Formato largo: LAN / Dis
    pivot_longer(
      cols      = c(land_val, disc_val),
      names_to  = "catchCategory",
      values_to = "OfficialWeight"
    ) %>%
    mutate(
      catchCategory = ifelse(catchCategory == "land_val", "Lan", "Dis")
    ) %>%
    # Reglas específicas por metier / stock / área / quarter
    mutate(
      # Capturas más bajas en GNS_DEF (LAN)
      OfficialWeight = ifelse(
        metier_group == "GNS_DEF" & catchCategory == "Lan",
        runif(n(), 30, 300),
        OfficialWeight
      ),
      
      # GTR_DEF (solo mac.27.nea): LAN 0–250, Dis 0–100
      OfficialWeight = ifelse(
        metier_group == "GTR_DEF" & catchCategory == "Lan",
        runif(n(), 0, 250),
        OfficialWeight
      ),
      OfficialWeight = ifelse(
        metier_group == "GTR_DEF" & catchCategory == "Dis",
        runif(n(), 0, 100),
        OfficialWeight
      ),
      
      # Caso crítico: mucha LAN PS_SPF en Q3
      OfficialWeight = ifelse(
        quarter == 3 & metier_group == "PS_SPF" &
          catchCategory == "Lan",
        1200,
        OfficialWeight
      ),
      
      # 0 LAN OTB_DEF para mac.27.nea en 27.7.j Q3
      OfficialWeight = ifelse(
        quarter == 3 & metier_group == "OTB_DEF" &
          stock == "mac.27.nea" & area == "27.7.j" &
          catchCategory == "Lan",
        0,
        OfficialWeight
      ),
      
      # 0 LAN GNS_DEF para pil.27.8c9a en 27.9.a Q1
      OfficialWeight = ifelse(
        quarter == 1 & metier_group == "GNS_DEF" &
          stock == "pil.27.8c9a" & area == "27.9.a" &
          catchCategory == "Lan",
        0,
        OfficialWeight
      ),
      
      # NA LAN para pil.27.8c9a en 27.7.j
      OfficialWeight = ifelse(
        stock == "pil.27.8c9a" & area == "27.7.j" &
          catchCategory == "Lan",
        NA,
        OfficialWeight
      ),
      
      # ---- Ajustes en Dis con un único case_when ----
      OfficialWeight = case_when(
        # 1) Dis = 0 en 27.8.c Q2 para pil.27.8c9a GNS_DEF
        quarter == 2 & area == "27.8.c" &
          stock == "pil.27.8c9a" &
          catchCategory == "Dis" &
          metier_group == "GNS_DEF" ~ 0,
        
        # 2) Dis = NA en 27.9.a Q1–Q2 para pil.27.8c9a GNS_DEF
        quarter %in% c(1, 2) & area == "27.9.a" &
          stock == "pil.27.8c9a" &
          catchCategory == "Dis" &
          metier_group == "GNS_DEF" ~ NA_real_,
        
        # 3) Dis = 0 en 27.7.j PS_SPF para mac.27.nea
        area == "27.7.j" & metier_group == "PS_SPF" &
          catchCategory == "Dis" & stock == "mac.27.nea" ~ 0,
        
        # 4) Dis = 10 en 27.9.a Q2 OTB_DEF para pil.27.8c9a
        area == "27.9.a" & metier_group == "OTB_DEF" &
          catchCategory == "Dis" & stock == "pil.27.8c9a" &
          quarter == 2 ~ 10,
        
        # 5) Dis = NA en 27.7.j para pil.27.8c9a
        catchCategory == "Dis" & area == "27.7.j" &
          stock == "pil.27.8c9a" ~ NA_real_,
        
        # Resto: dejar el valor anterior
        TRUE ~ OfficialWeight
      )
    ) %>% 
    # Dominio de descartes (Dis) y casos sin actividad
    mutate(
      domainCatchDis = case_when(
        # Dominios de descartes "normales"
        catchCategory == "Dis" &
          !(area == "27.7.j" & stock == "pil.27.8c9a") ~
          paste0("D_", area, "_", quarter),
        
        # Sin dominio cuando no hay actividad (LAN y Dis = 0 o NA)
        (is.na(OfficialWeight) | OfficialWeight == 0) ~ NA_character_,
        
        TRUE ~ NA_character_
      )
    ) %>% select(vesselFlagCountry, year,workingGroup, stock,catchCategory,quarter ,  area,
                 metier_group,domainCatchDis, domainCatchBMS, domainBiology, variableType,OfficialWeight ) %>% 
    # SampleID solo cuando hay dominio
    mutate(
      rand_sample = floor(runif(n(), 1, 75)),
      SampleID = ifelse(!is.na(domainCatchDis), rand_sample, 0)
    ) %>%
    select(-rand_sample)%>% 
    
    as.data.frame()
 # SAVE OUTPUTS
 fwrite(census, "outputs/census_validation.csv")
return (census)
  }

data_generation.R()
