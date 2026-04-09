plot_discards_heatmap <- function(df, thresh_val = 50) {
require (ggplot2)
  status_colors <- c(
    "NO Discard Data"        = "#de2d26",
    "No Activity"     =         "#f0f1f1",
    "Discard Only: No Landings"    ='#6BAED6'
  )
  status_colors[paste("SAFE: >",    thresh_val , " Coverage")] <-"#2ca25f"
  status_colors[paste("WARNING: <", thresh_val , " Coverage")] <- "darkorange"
 p<- ggplot(df, aes(x = factor(quarter), y = metier_group, fill = Status)) +
    geom_tile(color = "white", size = 0.3, alpha=0.9) +
    geom_text(aes(label = case_when(
      total_landings > 99 ~ as.character(round(total_landings, 0)),
      total_landings > 0  ~ as.character(round(total_landings, 1)),
      TRUE ~ ""
    )), size = 2.8, fontface = "bold") +
    facet_grid(stock ~ area, scales = "free_y", space = "free_y") +
    scale_fill_manual(values = status_colors) +
    theme(
      #legend.position = "top",
      strip.background = element_rect(fill = "grey95"),
      strip.text = element_text(face = "bold"),
      panel.spacing = unit(0.5, "lines")
    ) +
    labs(
      title = "Discard Raising Diagnostic: Strategy Map",
      subtitle = paste("Labels = Landings (t) | Threshold:", thresh_val),
      x = "Quarter", y = "Metier Group"
    )

  #GUARDAR EL GRÁFICO
  ggsave("outputs/04_heatmap_plot.png", p, width = 10, height = 6, dpi = 300)

  message(">>> Visual validation plot saved: outputs/04_Raising_Validation_Plot.png")

  return(p)
}

plot_discards_heatmap(setup_discards)
