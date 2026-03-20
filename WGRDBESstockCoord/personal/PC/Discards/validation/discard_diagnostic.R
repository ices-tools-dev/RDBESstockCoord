


# =============================================================================
# DISCARD RAISING DIAGNOSTIC — Main pipeline
# =============================================================================
# Usage:
#   output <- run_discard_diagnostic(data_fake, thresh_val = 50)
#   output$heatmap
#   output$bubble
#   output$table
# =============================================================================


# -----------------------------------------------------------------------------
# 1. COMPUTE DIAGNOSTIC METRICS
# Returns one row per domain with coverage (%) and a labelled Status
# -----------------------------------------------------------------------------
compute_discard_status <- function(df, thresh_val = 50) {
  
  df %>%
    select(
      vesselFlagCountry, year, workingGroup, stock, domainBiology,
      quarter, area, metier_group, catchCategory, OfficialWeight
    ) %>%
    # Long → wide: one column for Landings, one for Discards
    pivot_wider(
      names_from  = catchCategory,
      values_from = OfficialWeight,
      values_fn   = list(OfficialWeight = sum),
      values_fill = 0
    ) %>%
    rename(total_landings = Lan, total_discards = Dis) %>%
    group_by(year, stock, domainBiology, quarter, area, metier_group) %>%
    summarise(
      total_landings = sum(total_landings, na.rm = FALSE),
      total_discards = sum(total_discards, na.rm = FALSE),
      .groups        = "drop"
    ) %>%
    mutate(
      # Coverage only meaningful when landings > 0 and discards are not NA
      coverage = case_when(
        total_landings > 0 & !is.na(total_discards) ~
          round((total_discards / total_landings) * 100, 2),
        TRUE ~ NA_real_
      ),
      Status = case_when(
        (is.na(total_landings) | total_landings == 0) &
          total_discards > 0                        ~ "✗ Discard Only: No Landings",
        is.na(total_landings) | total_landings == 0 ~ "✗ No Activity",
        is.na(total_discards) | total_discards == 0 ~ "✗ NO Discard Data",
        coverage > thresh_val  ~ paste("✓ SAFE: >",  thresh_val, " Coverage"),
        coverage <= thresh_val ~ paste("⚠ WARNING: <", thresh_val, " Coverage"),
        TRUE ~ "Other / Check"
      )
    )
}


# -----------------------------------------------------------------------------
# 2. BUILD STATUS COLOUR PALETTE  (depends on thresh_val)
# -----------------------------------------------------------------------------
make_status_colors <- function(thresh_val) {
  c(
    "✗ NO Discard Data"           = "#de2d26",
    "✗ No Activity"               = "#f0f1f1",
    "✗ Discard Only: No Landings" = "#2ca25f",
    setNames("#756bb1",  paste("✓ SAFE: >",  thresh_val, " Coverage")),
    setNames("darkorange", paste("⚠ WARNING: <", thresh_val, " Coverage"))
  )
}


# -----------------------------------------------------------------------------
# 3. HEATMAP — Status × Quarter × Metier, faceted by Stock × Area
# -----------------------------------------------------------------------------
plot_heatmap <- function(output, thresh_val, status_colors) {
  
  ggplot(output, aes(x = factor(quarter), y = metier_group, fill = Status)) +
    geom_tile(color = "white", linewidth = 0.3) +
    geom_text(
      aes(label = case_when(
        total_landings > 99 ~ as.character(round(total_landings, 0)),
        total_landings > 0  ~ as.character(round(total_landings, 1)),
        TRUE ~ ""
      )),
      size = 2.8, fontface = "bold", color = "white"
    ) +
    facet_grid(stock ~ area) +
    scale_fill_manual(values = status_colors, name = "Sampling Status") +
    scale_x_discrete(expand = expansion(0)) +
    scale_y_discrete(expand = expansion(0)) +
    coord_fixed(ratio = 0.6) +
    labs(
      title    = "Discard Raising Diagnostic: Strategy Map",
      subtitle = paste("Labels = Landings (t) | Threshold:", thresh_val, "%"),
      x = "Quarter", y = "Metier Group"
    ) +
    #theme_bw() +
    theme(
      legend.position  = "top",
      strip.background = element_rect(fill = "grey95"),
      strip.text       = element_text(face = "bold"),
      panel.spacing    = unit(0, "lines"),
      panel.grid       = element_blank()
    )
}


# -----------------------------------------------------------------------------
# 4. BUBBLE PLOT — Landings size, coverage label, cross for discard-only
# -----------------------------------------------------------------------------
plot_bubble <- function(output, thresh_val, status_colors,
                        x_var = "quarter", y_var = "metier_group") {
  
  # Add plotting helpers: bubble size and warning label
  output <- output %>%
    mutate(
      plot_size  = ifelse(is.na(total_landings) & total_discards > 0, 1, total_landings),
      label_text = case_when(
        (is.na(total_landings) | total_landings == 0) &
          total_discards > 0                              ~ "100%\nDisc",
        is.na(coverage)                                   ~ "",
        Status == paste("✓ SAFE: >", thresh_val, " Coverage") ~ "",
        TRUE ~ paste0(round(coverage, 1), "%")
      )
    )
  
  # Rows with valid landings (main bubbles)
  has_landings   <- filter(output, !is.na(total_landings), total_landings > 0)
  # Rows with discards but zero/NA landings (cross markers)
  discard_only   <- filter(output, (is.na(total_landings) | total_landings == 0),
                           total_discards > 0)
  # Rows that need a coverage warning label
  needs_label    <- filter(output,
                           Status != "✗ No Activity",
                           Status != "✗ NO Discard Data",
                           Status != paste("✓ SAFE: >", thresh_val, " Coverage"))
  
  ggplot(output, aes(x = factor(.data[[x_var]]), y = .data[[y_var]])) +
    # Main bubbles
    geom_point(
      data    = has_landings,
      mapping = aes(size = total_landings, fill = Status),
      shape   = 21, color = "black", stroke = 0.8
    ) +
    # Cross marker where discards exist but landings are zero/missing
    geom_point(
      data  = discard_only,
      shape = 4, color = "black", size = 5, stroke = 1
    ) +
    # Coverage % label for warning cases
    geom_text_repel(
      data          = needs_label,
      aes(label     = label_text),
      size          = 2.8, fontface = "bold",
      force         = 3, box.padding = 0.6, point.padding = 0.4,
      min.segment.length = 0, segment.size = 0.2,
      segment.color = "grey40", lineheight = 0.8, max.overlaps = Inf
    ) +
    facet_grid(stock ~ area) +
    coord_fixed(ratio = 0.8) +
    scale_fill_manual(
      values = status_colors,
      guide  = guide_legend(override.aes = list(size = 9))
    ) +
    scale_size_continuous(range = c(5, 14)) +
    scale_x_discrete(expand = expansion(0.6)) +
    labs(
      title    = "Discard Raising Diagnostic: Strategy Setup",
      subtitle = paste0("Bubble size = Landings (t) | Label = Coverage % (Threshold: ",
                        thresh_val, "%)"),
      caption  = "Cross (✗) = Discards present with ZERO official Landings",
      x = "Quarter", y = "Metier Group"
    ) +
    #theme_bw() +
    theme(
      plot.title       = element_text(face = "bold", hjust = 0.5),
      strip.text       = element_text(face = "bold", size = 9),
      strip.background = element_rect(fill = "gray90", color = NA),
      legend.title     = element_text(size = 8),
      legend.text      = element_text(size = 8),
      axis.text        = element_text(size = 8)
    )
}


# -----------------------------------------------------------------------------
# 5. SUMMARY TABLE — domain-level landings, discards, coverage, flags
# -----------------------------------------------------------------------------
build_summary_table <- function(df, thresh_val = 20) {
  
  df %>%
    group_by(stock, area, quarter, metier_group, domainBiology) %>%
    summarise(
      landings  = sum(OfficialWeight[catchCategory == "Lan"], na.rm = TRUE),
      discards  = sum(OfficialWeight[catchCategory == "Dis"], na.rm = TRUE),
      .groups   = "drop"
    ) %>%
    mutate(
      coverage     = round((discards / landings) * 100, 2),
      coverage_pct = sprintf("%.2f%%", coverage),
      has_dis_data  = discards > 0,
      needs_raising = !has_dis_data | coverage < thresh_val
    ) %>%
    arrange(coverage_pct) %>% as.data.frame()
}


# =============================================================================
# MASTER WRAPPER — runs everything and returns a named list
# =============================================================================
discard_diagnostic <- function(df, thresh_val = 20,
                                   x_var = "quarter",
                                   y_var = "metier_group") {
  
  status_colors <- make_status_colors(thresh_val)
  output       <- compute_discard_status(df, thresh_val)
  
  list(
    output  = output,
    heatmap  = plot_heatmap(output, thresh_val, status_colors),
    bubble   = plot_bubble(output, thresh_val, status_colors, x_var, y_var),
    table    = build_summary_table(df, thresh_val)
  )
}


# =============================================================================
# RUN
# =============================================================================
diag <- discard_diagnostic(data_fake, thresh_val = 50)

print(diag$heatmap)
print(diag$bubble)
tabyl(diag$output, Status)
head(diag$table)
