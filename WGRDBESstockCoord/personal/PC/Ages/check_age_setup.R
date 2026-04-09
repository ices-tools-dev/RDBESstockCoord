# ============================================================
# AGE SETUP DIAGNOSTIC FUNCTION
# ============================================================
# Checks whether age data (otoliths / ALK) is sufficient for
# each sampling domain (quarter × metier × area). Returns a
# diagnostic table and two visualisation outputs:
#   - A bubble plot  (landings size + age status colour)
#   - A heatmap      (age status tile + landings label)
#
# Arguments:
#   census_df       : landings / catch data frame
#   dist_df         : length-distribution samples data frame
#   ALK_df          : Age-Length Key data frame (otolith readings)
#   threshold_ages  : minimum number of otoliths to consider a
#                     domain "safe" (default = 100)
#   grouping_vars   : character vector defining the domain axes;
#                     position matters →
#                       [1] x-axis  (typically "quarter")
#                       [2] y-axis  (typically "metier_group")
#                       [3] facet   (typically "area")
# ============================================================

#head (ALK,4)
# A tibble: 4 × 10
# Groups:   area, cod.FAO, quarter [1]
#  area     cod.FAO quarter length   Age     n num_samples otholits  prop N_Ages
  <chr>    <chr>     <int>  <dbl> <int> <int>       <int>    <int> <dbl>  <int>
#1 27.8.c.e MAC           1     33     3     1          13      588 1          1
#2 27.8.c.e MAC           1     34     3     3          13      588 0.075      1
#3 27.8.c.e MAC           1     34     4    34          13      588 0.85       1
#4 27.8.c.e MAC           1     34     5     3          13      588 0.075      1


#headtail (distributions,2)
#    VesselFlagCountry year       Stock speciesCode CatchCategory            domainBiology
#1                  ES 2024 pil.27.8c9a      198685           Lan 1_27.8.c.e_PS_SPF_>0_0_0
#2                  ES 2024 pil.27.8c9a      198685           Lan 1_27.8.c.e_PS_SPF_>0_0_0
#631                ES 2024 pil.27.8c9a      198685           Lan 4_27.9.a.s_PS_SPF_>0_0_0
#632                ES 2024 pil.27.8c9a      198685           Lan 4_27.9.a.s_PS_SPF_>0_0_0
#    fishDomain bvType bvValue AgeType AgeGroupPlus variableType total mean varianceTotal
#1           NA Length    11.5      NA           NA       Number 0.176   NA            NA
#2           NA Length    12.0      NA           NA       Number 0.176   NA            NA
#631         NA Length    20.5      NA           NA   WeightLive    NA 0.08            NA
#632         NA Length    21.0      NA           NA   WeightLive    NA 0.08            NA
#    varianceMean PSUtype numPSUs numSamples numMeasurements
#1             NA      NA      10         10             752
#2             NA      NA      10         10             752
#631           NA      NA       2          2             264
#632           NA      NA       2          2             264

#headtail(census,2)
#    VesselFlagCountry year workingGroup      stock speciesCode
#1                  ES 2024       WGWIDE mac.27.nea      #127023
#2                  ES 2024       WGWIDE mac.27.nea      127023
#152                ES 2024       WGWIDE mac.27.nea      127023
#153                ES 2024       WGWIDE mac.27.nea      127023
 #   CatchCategory quarter AreaType   area fisheriesManagementUnit
#1             Lan       2      Div 27.9.a                      NA
#2             Lan       3      Div 27.8.c                      NA
#152           Lan       4      Div 27.8.c                      NA
#153           Lan       4      Div 27.9.a                      NA
#              metier6 fleetType  fleet domainCatchDis domainCatchBMS
#1      FPO_CRU_>0_0_0   WGFleet ES_ALL           <NA>               
#2      FPO_CRU_>0_0_0   WGFleet ES_ALL           <NA>               
#152 TBB_MOL_55-64_0_0   WGFleet ES_ALL           <NA>               
#153 TBB_MOL_55-64_0_0   WGFleet ES_ALL           <NA>               
#    domainBiology   variableType total variance      PSUtype num_PSUs
#1                 OfficialWeight 0.038          fishing_trip        0
#2                 OfficialWeight 0.150          fishing_trip        0
#152               OfficialWeight 0.002          fishing_trip        0
#153               OfficialWeight 0.006          fishing_trip        0
#    num_trips comment metier_group
#1           0                  FPO
#2           0                  FPO
#152         0                  TBB
#153         0                  TBB

#########################################################################################################
stocks<-"mac.27.nea"
stocks<-"pil.27.8c9a"
stocks<-c("pil.27.8c9a","mac.27.nea")
check_age_setup <- function(census_df,
                            dist_df,
                            ALK_df,
                            threshold_ages  = 100,
                            threshold_samples = 1,
                            grouping_vars   = c("quarter",
                                                "metier_group",
                                                "area")) {
  
  grouping_syms <- rlang::syms(grouping_vars)
  
  # ----------------------------------------------------------
  # STEP 1 – LANDINGS BY DOMAIN
  # Aggregate total landings (tonnes) from the census table,
  # restricting to official landings (CatchCategory == "Lan").
  # metier_group collapses PS_SPF codes to "PS"; all others
  # are trimmed to their first three characters (e.g. "OTB").
  # ----------------------------------------------------------
  landings_summary <- census_catches %>%
    filter(CatchCategory == "Lan") %>%
    mutate(
      cod.FAO      = toupper(substring(stock, 1, 3)),
      metier_group = ifelse(
        metier6 == "PS_SPF_>0_0_0",
        substring(metier6, 1, 2),   # → "PS"
        substring(metier6, 1, 3)    # → "OTB", "LLS", …
      )
    ) %>%
    group_by(stock , cod.FAO, !!!grouping_syms, CatchCategory) %>%
    summarise(
      Total_Landings = sum(total , na.rm = TRUE),  # kg → tonnes
      .groups = "drop"
    )
  
  # ----------------------------------------------------------
  # STEP 2 – SAMPLING SUMMARY (length distributions)
  # Count the maximum number of length samples per domain.
  # Used later to distinguish "no sampling at all" from
  # "sampled but without ages".
  # ----------------------------------------------------------
  sampling_summary <- distributions %>%
    filter(variableType == "Number",  !grepl("27.8.b", domainBiology)) %>%
    distinct() %>% 
    mutate(
      quarter = as.numeric(substring(domainBiology, 1, 1)), 
      area = ifelse(
        stock== "mac.27.nea",
        substring(domainBiology, 3, 8),          # caso WGWIDE
        substring(domainBiology, 3, 10)          # resto (incluye WGHANSA, etc.)
      ),
      metier = sub("^[^_]*_[^_]*_", "", domainBiology),  # Remueve "número_área_" y toma el resto como metier
      metier_group= ifelse(grepl("^PS_SPF", metier), 
                    substring(metier, 1, 2), 
                    substring(metier, 1, 3)),
      cod.FAO = toupper(substr(stock, 1, 3))  
    )  %>% filter( !area%in% c('27.8.b')) %>% 
    group_by(cod.FAO, !!!grouping_syms) %>%
    summarise(
      N_Samples = max(numSamples, na.rm = TRUE),
      .groups   = "drop"
    ) %>%
    as.data.frame()
 
  
  # ----------------------------------------------------------
  # STEP 3 – MAIN DIAGNOSTIC TABLE
  # Join landings → ALK summary → sampling summary, then
  # assign a traffic-light Age_Status label to every domain.
  #
  # Age_Status categories:
  #   SAFE            : ≥ threshold_ages otoliths + lengths present
  #   WEAK            : < threshold_ages otoliths + lengths present
  #   WARNING         : lengths available but NO ALK match found
  #   No Length Samp. : domain has landings but zero length samples
  #   Only Landings   : catch reported but nothing else available
  # ----------------------------------------------------------
 
 
  alk_summary <- ALK%>% mutate(
    area = if_else(
      cod.FAO == "MAC",
      substring(area, 1, 6),
      area
    )
  ) %>%
    group_by(cod.FAO, area,metier_group, quarter) %>%
    summarise(
      # N_Samples = sum(unique(num_samples), na.rm = TRUE),
      otholits    = sum(n,   na.rm = TRUE),
      N_Ages   = sum(unique(otholits),   na.rm = TRUE),
      Min_L        = min(length, na.rm = TRUE),
      Max_L        = max(length, na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    mutate(
      stock = ifelse(cod.FAO == "MAC", "mac.27.nea", "pil.27.8c9a")
    ) 
   
  
  setup_diagnostic <- landings_summary %>%

    full_join(alk_summary)%>%
    left_join(sampling_summary) %>%
    distinct() %>%
    mutate(
      Age_Status = case_when(
        Total_Landings == 0 ~ "No Landings",
        
        # --- EL NUEVO ESTADO QUE BUSCABAS ---
        is.na(N_Samples) & N_Ages > 0  ~ "Ages only \n(No Length Samples)",
        
        # --- RESTO DE LA LÓGICA ---
        is.na(N_Samples) & is.na(N_Ages) & Total_Landings > 0 ~ "CRITICAL: No Sampling",
        
        is.na(N_Ages) & N_Samples >= threshold_samples ~ "WARNING: Lengths only (Needs ALK)",
      
        N_Ages >= threshold_ages & !is.na(N_Samples)    ~ "✅ SAFE : Above threshold \nSufficient Age Data",
        N_Ages < threshold_ages  & !is.na(N_Samples)    ~ "⚠️ WEAK: below threshold \n(Need Borrowing)",
        
        TRUE ~ "Other/Check")) %>% 
  
    as.data.frame()
  # --- 5. GLOBAL QUALITY METRIC ---
  # Calculate the percentage of total landings covered by each status
  quality_check <- setup_diagnostic %>%
    filter(!is.na(Total_Landings), Total_Landings > 0) %>%
    group_by(stock, Age_Status) %>%
    summarise(Landings_t = sum(Total_Landings), .groups = "drop_last") %>%
    mutate(Percentage = round(Landings_t / sum(Landings_t) * 100, 1))
  
  # --- 6. UNIFIED BUBBLE PLOT ---
  # Size = Catch volume | Color = Quality Status | Labels = Potential Donors
  # ----------------------------------------------------------
  # STEP 4 – BUBBLE PLOT
  # Each bubble represents one domain (quarter × metier × area
  # facet). Bubble SIZE encodes total landings; FILL colour
  # encodes Age_Status. Domains with no length sampling are
  # additionally marked with a cross (shape = 4).
  # ggrepel labels show otolith counts for SAFE/WEAK domains
  # and "No ALK" for WARNING domains.
  # ----------------------------------------------------------
  
  status_colors <- c(
    "✅ SAFE : Above threshold \nSufficient Age Data" ="#B2DF8A",   # Darker green
   "CRITICAL: No Sampling" = "lightblue",#  "#33A02C",   # Lighter green
    "WARNING: Lengths only (Needs ALK)" =    "#FF7F00",     # Darker orange
   "Other/Check" =  "#1F78B4",      # Lighter orange
   "Ages only \n(No Length Samples)" = "#FDBF6F",      # Darker blue
    "⚠️ WEAK: below threshold \n(Need Borrowing)" = "cornflowerblue"          # Lighter blue
  )

  setup_diagnostic<-filter(setup_diagnostic, stock %in%stocks)  
  
  bubble_plot <- ggplot(
    filter(setup_diagnostic, stock %in%stocks),
    aes(x = factor(.data[[grouping_vars[1]]]),
        y= .data[[grouping_vars[2]]])
  ) +
    
    # Main bubbles: size = landings, fill = age status
     geom_point(
     #  data  = filter(setup_diagnostic, !is.na(N_Samples)),
       data = filter(setup_diagnostic, stock %in%stocks,
                     !is.na(N_Samples) | (!is.na(N_Ages)
                                                 & is.na(N_Samples))),
       aes(size = Total_Landings, fill = Age_Status),
       shape = 21, color = "black", stroke = 0.5,
       position = position_jitter(width = 0.1, height = 0)
    
      ) +
    geom_point(
      #  data  = filter(setup_diagnostic, !is.na(N_Samples)),
      data = filter(setup_diagnostic, stock %in%stocks,
                    is.na(N_Samples) & (!is.na(N_Ages)
                      & is.na(Total_Landings ))),
      aes(size = 0.5, fill = Age_Status),
      shape = 21, color = "black", stroke = 0.5,
      position = position_jitter(width = 0.1, height = 0)
      
    ) +
    
     #Cross overlay for domains with no length sampling
      geom_point(
     data  = filter(setup_diagnostic, stock %in%stocks,
                    is.na(N_Ages) & is.na(N_Samples)  ),
     aes(x    = factor(.data[[grouping_vars[1]]]),
        y= .data[[grouping_vars[2]]],
       shape = Age_Status),
    color = "black",  size = 2, stroke = 1
    ) +
    facet_grid(stock~area, scales= "free_y")   +
        scale_size_continuous(range = c(4, 12)) +
        scale_fill_manual(values = status_colors, name = "Status",
            guide = guide_legend(override.aes = list(size = 8))) +

    scale_shape_manual(
      values = c("CRITICAL: No Sampling" = 4,
                 "WARNING: Lengths only \n(Needs ALK)" = 21,
                 "Ages only \n(No Length Samples)" = 14,
                 "Only Landings" = 21),
      name  = "Age status",
      guide = "none"
    ) +
  
   # scale_size_continuous(breaks = sort(unique(setup_diagnostic$Total_Landings)))+
    geom_text_repel(
      aes(
        label = case_when(
          Age_Status %in% c("Ages only \n(No Length Samples)",
                            "WEAK: below threshold \n(Need Borrowing)") ~
            paste(N_Ages, "otoliths"),
          Age_Status == "WARNING: Lengths only \n(Needs ALK)" ~ "No ALK",
          TRUE ~ ""
        )
      )   ,
      size               = 3,
      fontface           = "bold",
      force              = 3,
      box.padding        = 0.6,
      point.padding      = 0.4,
      min.segment.length = 0,
      segment.size       = 0.8,
      segment.color      = "grey40",
      lineheight         = 0.8,
      max.overlaps       = Inf
    ) +
  #  theme_bw() +
    labs(
      title   = paste(unique(setup_diagnostic$stock),
                      collapse = " / "),
      subtitle = paste0(
        "Bubble size = Total Landings (t)  |  Label = N otoliths",
        "  (Safe threshold > ", threshold_ages, ")"
      ),
      caption  = "X = Landings but \nNo Length Samplings",
      x        = "Quarter",
      y        = "Métier Group"
    )  +
    theme(
      # Title elements
      plot.title = element_text(size = 16, face = "bold"),
      plot.subtitle = element_text(size = 11),
      
      # Axis formatting
      axis.title = element_text(size = 14, face = "bold"),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size =10),
      
      # Panel and grid formatting
      panel.grid.major = element_line(color = "grey85"),
      # panel.grid.minor = element_blank(),
      #panel.border = element_rect(color = "grey70"),
      
      # Facet formatting
      strip.text = element_text(size = 12, face = "bold"),
      strip.background = element_rect(fill = "grey95", color = "grey70"),
      
      # No legend needed with facets
      legend.position = "right"
    )
  
  bubble_plot
  
  # ----------------------------------------------------------
  # STEP 5 – HEATMAP
  # One tile per domain; FILL = Age_Status, TEXT = landings
  # (tonnes, rounded). Faceted by stock × area so each species
  # / area combination gets its own panel.
  # ----------------------------------------------------------
  setup_diagnostic<-filter(setup_diagnostic, stock %in%stocks)
  heatmap_plot <- ggplot(  setup_diagnostic,
    aes(x   = factor(.data[[grouping_vars[1]]]),
       y   = .data[[grouping_vars[2]]],
        fill = Age_Status)
  ) +
    geom_tile(color = "black", alpha = 0.8) +
    geom_text(
      aes(label = round(Total_Landings,0)),
      size = 2.4,color="black", fontface = "bold"
    ) +
    #facet_grid( .data[[grouping_vars[3]]]~stock ) +
   # facet_grid(stock~area, scales="free_y")+
    facet_grid(stock ~ area, scales = "free_y", space = "free_y")+
    scale_fill_manual(values = status_colors, name = "Status",
                      guide = guide_legend(override.aes = list(size = 8)))  +
    labs(
      title   = paste(unique(setup_diagnostic$stock),
                      collapse = " / "),
      subtitle = "Age Diagnostic — Heatmap",
      x       = "Quarter",
      y       = "Métier",
      caption = "Label = Landings (t)"
    ) +
    

    scale_y_discrete(expand = expansion(0)) +

    theme(
      # Títulos
      plot.title      = element_text(face = "bold", size = 14),
      plot.subtitle   = element_text(color = "grey40", margin = margin(b = 8)),
      plot.caption    = element_text(color = "grey50", size = 8, hjust = 0),
      
      # Ejes
      axis.text.x     = element_text(angle = 0, hjust = 1, vjust = 1),
      axis.text.y     = element_text(size = 8),
      axis.ticks      = element_line(linewidth = 0.3),
      
      # Facets
      strip.text      = element_text(face = "bold", size = 9),
      strip.background = element_rect(fill = "grey92", color = NA),
      
      # Panel

      panel.spacing   = unit(0.3, "lines"),
      
      # Legend
      legend.position  = "top",
      legend.title     = element_text(face = "bold", size = 9),
      legend.text      = element_text(size = 8)
    )
    
  heatmap_plot
  

  # ----------------------------------------------------------
  # STEP 6 – RETURN
  # Returns a named list so the caller can access each output
  # independently:
  #   result$diagnostic   → data.frame with Age_Status column
  #   result$bubble_plot  → ggplot object
  #   result$heatmap_plot → ggplot object
  # ----------------------------------------------------------
  return(list(
    diagnostic   = setup_diagnostic,
    quality_check = quality_check,
    bubble_plot  = bubble_plot,
    heatmap_plot = heatmap_plot
  ))
}

age_setup <- check_age_setup(
  census_df       = census,        # landings / catch table
  dist_df         = distributions, # length-distribution samples
  ALK_df          = ALK,   
 # quality_check= quality_check,# Age-Length Key (otolith readings)
  threshold_ages  = 100            # minimum otoliths to flag a domain as "SAFE"
)

age_setup$quality
age_setup$diagnostic

age_setup$diagnostic %>%
  filter(metier_group == "PS", area == "27.8.c")

# plots
age_setup$bubble_plot
age_setup$heatmap_plot
