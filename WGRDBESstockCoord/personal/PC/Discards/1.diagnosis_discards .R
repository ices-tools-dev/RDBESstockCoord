# ============================================================
# diagnosis_discards()
# ============================================================
# This function performs a full diagnostic of discard sampling coverage
# for RCEF workflows. It:
#   1. Optionally filters métiers using a 95–99% landings rule
#   2. Aggregates landings and discards by stock/year/strata
#   3. Computes discard coverage and assigns diagnostic categories
#   4. Produces:
#         - Heatmap of coverage vs threshold
#         - Bubble plot of coverage vs landings
#         - Gap table for strata requiring pooling/borrowing
#         - Full diagnostic dataset
#
# Arguments:
#   census                Data frame with census-level landings/discards
#   grouping_vars         Vector of grouping variables (metier_group, area, Quarter)
#   threshold_discards    Numeric threshold for discard coverage (e.g., 0.1 = 10%)
#   threshold_percent     Character label for threshold (e.g., "10%")
#   filter_main_metiers   Logical: apply 95–99% métier filtering rule
#
# Returns:
#   A list with:
#       diagnosis   → processed dataset with coverage & decisions
#       heatmap     → ggplot object
#       bubbles     → ggplot object
#       gap_table   → table of strata requiring action
# ============================================================
# Vector de paquetes requeridos (se instala el que falte y se carga)
packs <- c(
  "dplyr", "data.table", "janitor", "readr", "readxl", "lubridate",
  "stringr", "icesVocab", "icesSD", "tidyr", "FSA", "collapse",
  "ggplot2", "ggrepel"
)

invisible(lapply(packs, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}))

census<- fread("census_catches_MAC_PIL_2024.csv") %>% filter( stock!="pil.27.8abd")
headtail(census)
tabyl(census, metier6)
census<-census %>% mutate(
  # gear = substr(METIER_DCF, 1, 3),

  gear = gsub("_$", "", substr(metier6, 1, 3)),

  FLEET = case_when(

    # gear %in% c("GNS", "GTR", "GND") ~ "gillnets",

    gear %in% c("LHM", "LLS") ~ "small_scale_lines",

    gear %in% c("OTB", "PTB") ~ "trawl",

    gear == "PS" ~ "purse_seine",
    gear == c("GNS", "GTR") ~ "gillnets",

    metier6== "MIS_MIS_0_0_0_HC" ~ "oth",

    TRUE ~ "artisanal"
  )
)
#census$area<-substring (census$area, 1,6)
headtail(census,2)
tabyl(census, stock)
#    VesselFlagCountry year workingGroup       stock speciesCode catchCategory quarter
#  1                  ES 2024      WGHANSA pil.27.8abd      126421        RegDis       3
#  2                  ES 2024      WGHANSA pil.27.8abd      126421           Lan       1
#  262                ES 2024       WGWIDE  mac.27.nea      127023           Lan       4
#  263                ES 2024       WGWIDE  mac.27.nea      127023           Lan       4
#      AreaType     area fisheriesManagementUnit           metier6 fleetType
#  1        Div   27.8.b                      NA OTB_DEF_70-99_0_0   Metier6
#  2        Div   27.8.b                      NA     PS_SPF_>0_0_0   Metier6
#  262      Div 27.8.c.e                      NA TBB_MOL_55-64_0_0   WGFleet
#  263      Div 27.9.a.n                      NA TBB_MOL_55-64_0_0   WGFleet
#               fleet             domainCatchDis domainCatchBMS          domainBiology
#  1   OTB_DEF_70-99_0_0 3_27.8.b_OTB_DEF_70-99_0_0             NA                   <NA>
#  2       PS_SPF_>0_0_0                       <NA>             NA 1_27.8.b_PS_SPF_>0_0_0
#  262            ES_ALL                       <NA>             NA                   <NA>
#  263            ES_ALL                       <NA>             NA                   <NA>
#    variableType   total variance      PSUtype num_PSUs num_trips comment
#  1   OfficialWeight   0.030       NA fishing_trip        0         0      NA
#  2   OfficialWeight 577.707       NA fishing_trip       22        14      NA
#  262 OfficialWeight   0.002       NA fishing_trip        0         0      NA
#  263 OfficialWeight   0.006       NA fishing_trip        0         0      NA

#distributions <- read_csv("distributions_MAC_PIL.csv")

#headtail (distributions,2)
packs <- c(
  "dplyr", "data.table", "janitor", "readr", "readxl", "lubridate",
  "stringr", "icesVocab", "icesSD", "tidyr", "FSA", "collapse", "RDBEScore"
)

invisible(lapply(packs, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
  library(pkg, character.only = TRUE)
}))

# ============================================================
# 1. TARGET STOCKS
# ============================================================
census<- fread("census_catches_MAC_PIL_2024.csv") %>% filter( stock!="pil.27.8abd")
headtail(census,2)
tabyl(census, stock)




diagnosis_discards <- function(
    census,
    stocks = c("mac.27.nea", "pil.27.8c9a" ),
    grouping_vars = c("FLEET", "areaValue", "seasonValue")   ,
    threshold_discards = 30 ,
    threshold_percent = "30%"
) {


  # Convert grouping variables to symbols for tidy evaluation
  grouping_syms <- syms(grouping_vars)

  # -----------------------------
  # 2 AGREGACIÓN LANDINGS
  # -----------------------------
  landings_df <- census %>%

    group_by(stock, year, !!!grouping_syms) %>%
      summarise(
          landings = sum(total, na.rm = TRUE),

             landings_with_discards = sum(
                 total[!is.na(domainCatchDis) & domainCatchDis != ""],
                 na.rm = TRUE
              ),

             .groups = "drop"
        )

subset(landings_df, areaValue %in% c("27.7.j", "27.6.a")) %>% as.data.frame()
subset(landings_df, areaValue %in% c("27.8.c")) %>% as.data.frame()
  # -----------------------------
  # 3 AGREGACIÓN DISCARD
  # -----------------------------
  discards_df <- census %>%
    filter(catchCategory != "Lan") %>%
    mutate(
      cod.FAO      = toupper(substring(stock, 1, 3)),
      total = as.numeric(total)
    ) %>%
    group_by(stock, year, !!!grouping_syms) %>%
    summarise(
      total_discards = sum(as.numeric(total), na.rm = TRUE),
      .groups = "drop"
    ) %>% as.data.frame()

  # ============================================================
  # 4. JOIN LANDINGS + DISCARDS
  # ============================================================
  setup_analysis <- landings_df %>%
  full_join(discards_df, by = c("stock", "year", grouping_vars)) %>%
    filter(stock %in% stocks) %>%
    mutate(
      coverage_discards = (landings_with_discards/landings * 100  ),
      Status = case_when(
        landings == 0 & is.na(landings) ~ "No Landings",
        is.na(total_discards)   ~ "NO Discard Data",
        coverage_discards > threshold_discards     ~ paste("SAFE : >", threshold_percent, " Coverage"),
        coverage_discards <= threshold_discards & coverage_discards > 0 ~ paste("WARNING: <", threshold_percent, " Coverage"),
        TRUE ~ "Discard Only: No Landings"
      ),
      Raising_Decision = case_when(
        coverage_discards >= threshold_discards ~ "✅ direct Raising",
        # The raising is direct. We use the data from the layer itself (st_catch)
        # because it is statistically robust.
        coverage_discards < threshold_discards & coverage_discards > 0 ~ "⚠️ POOL: Group with adjacent quarters",
        #Insufficient data → pooling applied using neighboring strata.
      #  is.na(total_discards) ~ "❌ BORROW: Search similar metier/quarter",
        TRUE ~ "❌ BORROW: Search similar metier/quarter",
      )
    ) %>% as.data.frame()# %>% filter(areaValue=="27.7.j")


  tabyl(setup_analysis, stock,Raising_Decision)
  subset(setup_analysis, areaValue %in% c("27.7.j", "27.6.a")) %>% as.data.frame()
  # -----------------------------
  # 5. HEATMAP
  # -----------------------------
  status_colors <- c(
    "NO Discard Data" = "cornflowerblue",
    "NO Activity" = "#1F78B4" ,
    "Discard Only: No Landings"= "lightblue"
  )
  status_colors[paste("SAFE : >", threshold_percent, " Coverage")] <-   "#2ca25f"
  status_colors[paste("WARNING: <", threshold_percent, " Coverage")] <- "darkorange"
  theme_set<- theme(
    # Títulos
    plot.title      = element_text(face = "bold", size = 14),
    plot.subtitle   = element_text(color = "grey40", margin = margin(b = 8)),
    plot.caption    = element_text(color = "grey50", size = 8, hjust = 0),

    # Ejes
    axis.text.x     = element_text(angle = 0, hjust = 1, vjust = 1,size = 7),
    axis.text.y     = element_text(size = 8),
    axis.ticks      = element_line(linewidth = 0.3),

    # Facets
    strip.text      = element_text(face = "bold", size = 9),
    strip.background = element_rect(fill = "grey92", color = NA),

    # Panel
    #  panel.grid      = element_blank(),                   # ← innecesario en heatmap
    panel.spacing   = unit(0.3, "lines"),

    # Leyenda
    legend.position  = "right",
    legend.title     = element_text(face = "bold", size = 9),
    legend.text      = element_text(size = 8)
  )


  heatmap_plot <- ggplot(setup_analysis,
                          aes(x = factor(.data[[grouping_vars[3]]]), # quarter
                              y = .data[[grouping_vars[1]]],   # metier_group
                              fill = Status)) +
    geom_tile(color = "black", alpha=0.85) +
    facet_grid(stock~ .data[[grouping_vars[2]]],   scales = "free_y", space="free_y") +
    geom_text(
      aes(
        label = ifelse(
       landings > 99,
          round(landings, 0),             # > 99 → 0 decimales
          ifelse(
           landings >= 10,
            round(landings, 1),           # [10, 99] → 1 decimal
            round(landings, 2)            # < 10 → 2 decimales
          )
        )
      ),
      size = 2.5,
      fontface = "bold"
    )+
  scale_fill_manual(values = status_colors) + # Usar el vector dinámico

    labs(title = "Discard Raising Setup: Coverage vs Threshold",
         subtitle = paste(unique(setup_analysis$stock), collapse = " / "),
         caption = paste("Labels = Tons of Landings | Threshold =", threshold_percent),
         x = "Quarter") +
     facet_grid(
      stats::as.formula(paste("stock ~", grouping_vars[2])),
      scales = "free_y", space = "free_y"
    )+
    theme_set
  # -----------------------------
  # 6. BUBBLE PLOT
  # -----------------------------
  bubble_colors <- c(
    "NO Discard Data" = "cornflowerblue", #    '#6BAED6' ,
    "No Landings" = "#f0f1f1",
    "Discard Only: No Landings"= "#ffff99"
  )
  bubble_colors[paste("SAFE : >", threshold_percent, " Coverage")] <- "#2ca25f"
  bubble_colors[paste("WARNING: <", threshold_percent, " Coverage")] <- "darkorange"

  bubble_data <- setup_analysis %>%
    mutate(
      # Si landings es 0 pero hay descartes, le damos un tamaño pequeño para que sea visible
      plot_size = ifelse( is.na(landings) & total_discards > 0, 1.0, landings),
      # Marcamos la etiqueta de texto para estos casos
      label_text = case_when(
        is.na(landings) & total_discards > 0 ~ "100%\nDisc",
        is.na(coverage_discards) ~ "",
        coverage_discards == 0 ~ "",
        coverage_discards > threshold_discards ~ "",
        TRUE ~ paste0(round(coverage_discards, 1), "%")
      )
    )
  bubble_plot  <-  ggplot(bubble_data,
                        aes(x = factor(.data[[grouping_vars[3]]]),
                            y = .data[[grouping_vars[1]]])
  )  +   # metier_group +
    # 1. Burbujas con borde negro para que resalten sobre el fondo
    geom_point(aes(size = plot_size, fill = Status),
               shape = 21, color = "black", stroke = 0.8) +

    # 2. Marcador visual para descartes sin landings (X)
    geom_point(data = filter(bubble_data, (is.na(landings) | landings == 0) & total_discards > 0),
               aes(x =  factor(.data[[grouping_vars[3]]]),
                   y =.data[[grouping_vars[1]]]  ),
               shape = 4, color = "black", size = 2, stroke = 1) +


    facet_grid(stock ~ .data[[grouping_vars[2]]] , scales = "free_y", space = "free_y") +
    scale_size_continuous(range = c(5, 15), name = "Landings (t)")  +
    scale_fill_manual(values = bubble_colors,
                      guide = guide_legend(override.aes = list(size = 9))) +
    # Reducimos un poco el máx para ganar aire
    theme_bw() +
    scale_x_discrete(expand = expansion(0.2)) +
    # 3. Etiquetas inteligentes con ggrepel
    geom_text_repel(aes(label = label_text),
                    size = 2.8,
                    fontface = "bold",
                    force = 2,             # Fuerza para separar etiquetas
                    color="red",
                    box.padding = 0.6,      # Espacio para que el texto no toque el punto
                    point.padding = 0.4,    # Espacio desde el centro de la burbuja
                    min.segment.length = 0, # Siempre dibuja una línea si el texto se aleja
                    segment.size = 0.2,     # Línea muy fina
                    segment.color = "grey40",
                    lineheight = 0.8,
                    max.overlaps = Inf) +     # Ajuste para el salto de línea en "100%\nDisc"

    # 4. Configuración de escalas y facetas

    labs(
      title = "Discard Raising Diagnostic: Coverage vs Landings",
      subtitle = paste0("Bubble size = Landings (t) | Label = Coverage % (Threshold: ", threshold_percent, ")"),
      caption = "Cross (X) indicates Discards present with ZERO official Landings",
      x = "Quarter", y = "Metier",
      fill = "Sampling Status", size = "Landings (t)"
    ) + theme_set


  # -----------------------------
  # 7. GAP TABLE
  # -----------------------------
  gap_table <- setup_analysis %>%filter(Status !=paste("SAFE : >", threshold_percent, " Coverage")) %>%
    as.data.frame()

  # -----------------------------
  # 8. RETURN
  # -----------------------------
  return(list(
    diagnosis = setup_analysis,
    heatmap =   heatmap_plot,
    bubbles = bubble_plot,
    gap_table = gap_table
  ))
}

setup <- diagnosis_discards(census)

setup$diagnosis %>% headtail()
setup$heatmap
setup$bubbles
setup$gap_table

