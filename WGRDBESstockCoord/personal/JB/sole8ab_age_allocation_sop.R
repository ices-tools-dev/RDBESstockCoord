###-----------------------------------------------------------------------------
### Sum Of Products (SOP) correction
###-----------------------------------------------------------------------------
source(here("WGRDBESstockCoord/personal/JB/fun/sop.R"))

### Compute SOP using sop function in fun/project folder
df_sop <-  sop(caton = df_stock_overview_L_D_raised,
               canum = df_canum_imported,
               by = c("vesselFlagCountry", "seasonValue", "year", "areaValue",
                      "fleetValue", "catchCategory"))



plot_sop <- ggplot(df_sop,
                   aes(x = Caton, y = SopR, col = vesselFlagCountry)) +
  geom_point() +
  facet_wrap(~ catchCategory)
plot_sop

ggsave(plot = plot_sop,
       filename = glue("{path_data_raised}/plot_sop.png"),
       width = 20, height = 20, units = "cm")

### Compute SOP ratio
df_sop_ratio <- df_sop %>%
  group_by(year,catchCategory) %>%
  summarise(Caton = sum(Caton), Sop = sum(Sop))

plot_sop_ratio <- ggplot(df_sop_ratio,
                         aes(x = as.factor(year),
                             y = Sop/Caton,
                             color = catchCategory)) +
  geom_point() +
  geom_line() +
  xlab("Year")

ggsave(plot = plot_sop_ratio,
       filename = glue("{path_data_raised}/plot_sop_ratio.png"),
       width = 20, height = 20, units = "cm")

list_sop <- list(df_sop = df_sop,
                 df_sop_ratio = df_sop_ratio,
                 plot_sop = plot_sop,
                 plot_sop_ratio = plot_sop_ratio)

saveRDS(list_sop,
        file = glue("{path_data_raised}/list_sop.rds"))

# sop correction
df_canum_imported_sopr <- inner_join(df_canum_imported,
                                     df_sop) %>%
  mutate(Number_Total_SopR = Number_Total / SopR)

