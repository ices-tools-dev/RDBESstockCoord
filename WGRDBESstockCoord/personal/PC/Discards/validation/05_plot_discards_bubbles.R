plot_discards_bubbles<- function(df, 
                                 thresh_val = 50,
                                 threshold_percent = "50%",
                                 grouping_vars = c("metier_group", "area", "quarter")
                                 )  {
 require(ggrepel)
  
   setup_discards<- setup_discards%>% mutate(
    plot_size = ifelse(is.na(total_landings) & total_discards > 0,
                       1, total_landings),
    label_text = case_when(
      is.na(total_landings)|total_landings==0 & total_discards > 0 ~ "100%\nDisc",
      is.na(coverage)                            ~ "",
      Status == paste("✓ SAFE: >", thresh_val, " Coverage") ~ "",
      TRUE                                       ~ paste0(round(coverage, 1), "%")
    )
  )
   bubble_colors <- c(
     "✗ NO Discard Data" ='#6BAED6' ,
     "✗ No Activity" = "#f0f1f1",
     "✗ Discard Only: No Landings"= "#ffff99"
   )
   bubble_colors[paste("✓ SAFE: >",thresh_val, " Coverage")] <-  "#B2DF8A"
   bubble_colors[paste("⚠ WARNING: <",thresh_val, " Coverage")] <- "darkorange"
 
   
  
     
q<- ggplot(df,
                      aes(x = factor(.data[[grouping_vars[3]]]),
                          y = .data[[grouping_vars[1]]])) +
  geom_point(
    data = filter( setup_discards, !is.na(total_landings) | total_landings > 0),
    mapping = aes(size = total_landings, fill = Status),
    shape   = 21,
    color   = "black",
    stroke  = 0.8
  ) +
  geom_point(data = filter( setup_discards,
                            (is.na(total_landings) | total_landings == 0) &
                              total_discards > 0),
             aes(x = factor(.data[[grouping_vars[3]]]), 
                 y = metier_group),
             shape = 4, color = "black", size =5, stroke = 1) +
  facet_grid(stock ~ area, scales = "free_y", space = "free") +
  # coord_fixed(ratio = 0.8)+
  scale_fill_manual(values = bubble_colors,
                    guide = guide_legend(override.aes = list(size = 9))) +
  scale_size_continuous(range = c(5, 12)) +
  scale_shape_manual(
    values = c(
      # "SAFE — Sufficient Age Data"                        = 21,
      #  "WEAK — Low Age Sample Size"                        = 21,
      "Discard Only: No Landings"      = 4,  # cruz
      "WARNING — No Age Data (Lengths Only)"              = 21,
      "Only Landings"                                     = 21
    ),
    name  = "Age status",
    guide = "none"
  )+
  #theme_bw() +
  scale_x_discrete(expand = expansion(0.6))+
  geom_text_repel(
    data= filter(setup_discards,Status !="✗ No Activity", 
                 Status !=  "✗ NO Discard Data",
                 Status!= (paste("✓ SAFE: >", 
                                 thresh_val, " Coverage")) ),
    aes(label = label_text),
    size = 2.8, fontface = "bold",
    force = 3, box.padding = 0.6,
    point.padding = 0.4,
    min.segment.length = 0,
    segment.size = 0.2,
    segment.color = "grey40",
    lineheight = 0.8,
    max.overlaps = Inf) +
  labs(
    title = "Discard Raising Diagnostic: Strategy Setup",
    subtitle = paste0("Bubble size = Landings (t) | Label = Coverage % (Threshold: ", threshold_percent, ")"),
    caption = "Cross (X) indicates Discards present with ZERO official Landings",
    x = "quarter", y = "Metier Group"
  )+
  
  theme(
    plot.title       = element_text(face = "bold", hjust = 0.5),
    strip.text       = element_text(face = "bold", size = 9),
    strip.background = element_rect(fill = "gray90", color = NA),
    #panel.grid.minor = element_blank(),
    #legend.position  = "top",
    legend.title     = element_text(size = 8),
    legend.text      = element_text(size = 8),
    axis.text.x      = element_text(size = 8),
    axis.text.y      = element_text(size = 8)
  )
 
 #GUARDAR EL GRÁFICO
 ggsave("outputs/05_bubblep_plot.png", q, width = 10, height = 6, dpi = 300)
 
 message(">>> Visual validation plot saved: outputs/05_discards_Bubble_Plot.png")
 

}
plot_discards_bubbles(setup_discards)
