
### extract data with landings and discards
dfw_stock_overview_L_D <- dfw_stock_overview %>%
  filter(!is.na(discards_ratio))

### extract data without landings and discards
dfw_stock_overview_L_noD <- dfw_stock_overview %>%
  filter(is.na(discards_ratio)) %>%
  select(-discards_ratio)

 # testw <- dfw_stock_overview_L_noD %>%
 #   filter(Landings_Catchkg ==   124255.80)

stratum_no_discards <- dfw_stock_overview_L_noD %>%
  select(stratum_full)

###-----------------------------------------------------------------------------
### (1) First stratum allocation: Country, Season, Fleet (no Area)
###-----------------------------------------------------------------------------
df_sampled_discards_stratum_c_q_f <- dfw_stock_overview_L_D %>%
  group_by(stratum_c_q_f) %>%
  mutate(total_Landings_Catchkg = sum(Landings_Catchkg)) %>%
  mutate(weightingfactor = Landings_Catchkg / total_Landings_Catchkg,
         weightingfactor = ifelse(is.na(weightingfactor), 1,  weightingfactor)) %>%
  select(stratum_c_q_f, discards_ratio, weightingfactor)

### raise discards
df_raised_discards_stratum_c_q_f <- inner_join(filter(dfw_stock_overview_L_noD,
                                                      stratum_full %in% levels(stratum_no_discards$stratum_full)),
                                               df_sampled_discards_stratum_c_q_f,
                                               by = "stratum_c_q_f",
                                               relationship = "many-to-many") %>%
  mutate(raised_discards = Landings_Catchkg * discards_ratio * weightingfactor,
         raised_discards = ifelse(Landings_Catchkg == 0, 0, raised_discards)) %>%
  filter(!is.na(raised_discards)) %>%
  group_by(stratum_full, Fleets) %>%
  summarise(raised_discards = sum(raised_discards, na.rm = TRUE)) %>%
  droplevels() %>%
  left_join(., select(dfw_stock_overview_L_noD, stratum_full, Fleets, Landings_Catchkg)) %>%
  rename(raised_landings =  Landings_Catchkg)

stratum_no_discards <-  stratum_no_discards %>%
  filter(!(stratum_full %in% levels(df_raised_discards_stratum_c_q_f$stratum_full))) %>%
  droplevels()

df_stratum_raised <- data.frame(stratum = "stratum_c_q_f",
                                total_landings_with_noDiscards = sum(dfw_stock_overview_L_noD$Landings_Catchkg),
                                raised_landings = sum(df_raised_discards_stratum_c_q_f$raised_landings)) %>%
  mutate(percentage_landings_raised_with_noDiscards = raised_landings / total_landings_with_noDiscards)

test <- raise_discards_stratum(stratum_var = "stratum_c_q_f",
                               dfw_stock_overview_L_D,
                               dfw_stock_overview_L_noD)

###-----------------------------------------------------------------------------
### (2) Second stratum allocation: Country, Season, Area, Gear (e.g. GNS, OTB, ...)
###-----------------------------------------------------------------------------
df_sampled_discards_stratum_c_q_a_g <- dfw_stock_overview_L_D %>%
  group_by(stratum_c_q_a_g) %>%
  mutate(total_Landings_Catchkg = sum(Landings_Catchkg)) %>%
  mutate(weightingfactor = Landings_Catchkg / total_Landings_Catchkg,
         weightingfactor = ifelse(is.na(weightingfactor), 1,  weightingfactor)) %>%
  select(stratum_c_q_a_g, discards_ratio, weightingfactor)

### raise discards
df_raised_discards_stratum_c_q_a_g <- inner_join(filter(dfw_stock_overview_L_noD,
                                                        stratum_full %in% levels(stratum_no_discards$stratum_full)),
                                                 df_sampled_discards_stratum_c_q_a_g,
                                                 by = "stratum_c_q_a_g",
                                                 relationship = "many-to-many") %>%
  mutate(raised_discards = Landings_Catchkg * discards_ratio * weightingfactor,
         raised_discards = ifelse(Landings_Catchkg == 0, 0, raised_discards)) %>%
  filter(!is.na(raised_discards)) %>%
  group_by(stratum_full, Fleets) %>%
  summarise(raised_discards = sum(raised_discards, na.rm = TRUE),
            raised_landings = unique(Landings_Catchkg)) %>%
  droplevels()

stratum_no_discards <- stratum_no_discards %>%
  filter(!(stratum_full %in% levels(df_raised_discards_stratum_c_q_a_g$stratum_full))) %>%
  droplevels()

df_stratum_raised <- df_stratum_raised %>%
  add_row(stratum = "stratum_c_q_a_g",
          total_landings_with_noDiscards = sum(dfw_stock_overview_L_noD$Landings_Catchkg),
          raised_landings = sum(df_raised_discards_stratum_c_q_a_g$raised_landings)) %>%
  mutate(percentage_landings_raised_with_noDiscards = raised_landings / total_landings_with_noDiscards)

###-----------------------------------------------------------------------------
### (3) Third stratum allocation: Country, Season, Area, Super-Gear (e.g. G, O, ...)
###-----------------------------------------------------------------------------
df_sampled_discards_stratum_c_s_a_sg <- dfw_stock_overview_L_D %>%
  group_by(stratum_c_s_a_sg) %>%
  mutate(total_Landings_Catchkg = sum(Landings_Catchkg)) %>%
  mutate(weightingfactor = Landings_Catchkg / total_Landings_Catchkg,
         weightingfactor = ifelse(is.na(weightingfactor), 1,  weightingfactor)) %>%
  select(stratum_c_s_a_sg, discards_ratio, weightingfactor)

### raise discards
df_raised_discards_stratum_c_s_a_sg <- inner_join(filter(dfw_stock_overview_L_noD,
                                                         stratum_full %in% levels(stratum_no_discards$stratum_full)),
                                                  df_sampled_discards_stratum_c_s_a_sg,
                                                  by = "stratum_c_s_a_sg",
                                                  relationship = "many-to-many") %>%
  mutate(raised_discards = Landings_Catchkg * discards_ratio * weightingfactor,
         raised_discards = ifelse(Landings_Catchkg == 0, 0, raised_discards)) %>%
  filter(!is.na(raised_discards)) %>%
  group_by(stratum_full, Fleets) %>%
  summarise(raised_discards = sum(raised_discards, na.rm = TRUE),
            raised_landings = unique(Landings_Catchkg)) %>%
  droplevels()

stratum_no_discards <- stratum_no_discards %>%
  filter(!(stratum_full %in% levels(df_raised_discards_stratum_c_s_a_sg$stratum_full))) %>%
  droplevels()

df_stratum_raised <- df_stratum_raised %>%
  add_row(stratum = "stratum_c_s_a_sg",
          total_landings_with_noDiscards = sum(dfw_stock_overview_L_noD$Landings_Catchkg),
          raised_landings = sum(df_raised_discards_stratum_c_s_a_sg$raised_landings)) %>%
  mutate(percentage_landings_raised_with_noDiscards = raised_landings / total_landings_with_noDiscards)

###-----------------------------------------------------------------------------
### (4) Third stratum allocation: Season, Area, Super-Gear (e.g. G, O, ...)
###-----------------------------------------------------------------------------
df_sampled_discards_stratum_s_a_sg <- dfw_stock_overview_L_D %>%
  group_by(stratum_s_a_sg) %>%
  mutate(total_Landings_Catchkg = sum(Landings_Catchkg)) %>%
  mutate(weightingfactor = Landings_Catchkg / total_Landings_Catchkg,
         weightingfactor = ifelse(is.na(weightingfactor), 1, weightingfactor)) %>%
  select(stratum_s_a_sg, discards_ratio, weightingfactor)

### raise discards
df_raised_discards_stratum_s_a_sg <- inner_join(filter(dfw_stock_overview_L_noD,
                                                       stratum_full %in% levels(stratum_no_discards$stratum_full)),
                                                df_sampled_discards_stratum_s_a_sg,
                                                by = "stratum_s_a_sg",
                                                relationship = "many-to-many") %>%
  mutate(raised_discards = Landings_Catchkg * discards_ratio * weightingfactor,
         raised_discards = ifelse(Landings_Catchkg == 0, 0, raised_discards)) %>%
  filter(!is.na(raised_discards)) %>%
  group_by(stratum_full, Fleets) %>%
  summarise(raised_discards = sum(raised_discards, na.rm = TRUE),
            raised_landings = unique(Landings_Catchkg)) %>%
  droplevels()

stratum_no_discards <- stratum_no_discards %>%
  filter(!(stratum_full %in% levels(df_raised_discards_stratum_s_a_sg$stratum_full))) %>%
  droplevels()


df_stratum_raised <- df_stratum_raised %>%
  add_row(stratum = "stratum_s_a_sg",
          total_landings_with_noDiscards = sum(dfw_stock_overview_L_noD$Landings_Catchkg),
          raised_landings = sum(df_raised_discards_stratum_s_a_sg$raised_landings)) %>%
  mutate(percentage_landings_raised_with_noDiscards = raised_landings / total_landings_with_noDiscards)
df_sampled_discards_stratum_s
###-----------------------------------------------------------------------------
### (5) Third stratum allocation: Season, Area
###-----------------------------------------------------------------------------
df_sampled_discards_stratum_s_a <- dfw_stock_overview_L_D %>%
  group_by(stratum_s_a) %>%
  mutate(total_Landings_Catchkg = sum(Landings_Catchkg)) %>%
  mutate(weightingfactor = Landings_Catchkg / total_Landings_Catchkg,
         weightingfactor = ifelse(is.na(weightingfactor), 1,  weightingfactor)) %>%
  select(stratum_s_a, discards_ratio, weightingfactor)

### raise discards
df_raised_discards_stratum_s_a <- inner_join(filter(dfw_stock_overview_L_noD,
                                                    stratum_full %in% levels(stratum_no_discards$stratum_full)),
                                             df_sampled_discards_stratum_s_a,
                                             by = "stratum_s_a",
                                             relationship = "many-to-many") %>%
  mutate(raised_discards = Landings_Catchkg * discards_ratio * weightingfactor,
         raised_discards = ifelse(Landings_Catchkg == 0, 0, raised_discards)) %>%
  filter(!is.na(raised_discards)) %>%
  group_by(stratum_full, Fleets) %>%
  summarise(raised_discards = sum(raised_discards, na.rm = TRUE),
            raised_landings = unique(Landings_Catchkg)) %>%
  droplevels()

stratum_no_discards <- stratum_no_discards %>%
  filter(!(stratum_full %in% levels(df_raised_discards_stratum_s_a$stratum_full))) %>%
  droplevels()

df_stratum_raised <- df_stratum_raised %>%
  add_row(stratum = "stratum_s_a",
          total_landings_with_noDiscards = sum(dfw_stock_overview_L_noD$Landings_Catchkg),
          raised_landings = sum(df_raised_discards_stratum_s_a$raised_landings)) %>%
  mutate(percentage_landings_raised_with_noDiscards = raised_landings / total_landings_with_noDiscards)

###-----------------------------------------------------------------------------
### (6) Third stratum allocation: Season
###-----------------------------------------------------------------------------
df_sampled_discards_stratum_s <- dfw_stock_overview_L_D %>%
  group_by(stratum_s) %>%
  mutate(total_Landings_Catchkg = sum(Landings_Catchkg)) %>%
  mutate(weightingfactor = Landings_Catchkg / total_Landings_Catchkg,
         weightingfactor = ifelse(is.na(weightingfactor), 1,  weightingfactor)) %>%
  select(stratum_s_a, discards_ratio, weightingfactor)

### raise discards
df_raised_discards_stratum_s <- inner_join(filter(dfw_stock_overview_L_noD,
                                                  stratum_full %in% levels(stratum_no_discards$stratum_full)),
                                           df_sampled_discards_stratum_s,
                                           by = "stratum_s",
                                           relationship = "many-to-many") %>%
  mutate(raised_discards = Landings_Catchkg * discards_ratio * weightingfactor,
         raised_discards = ifelse(Landings_Catchkg == 0, 0, raised_discards)) %>%
  filter(!is.na(raised_discards)) %>%
  group_by(stratum_full, Fleets) %>%
  summarise(raised_discards = sum(raised_discards, na.rm = TRUE),
            raised_landings = unique(Landings_Catchkg)) %>%
  droplevels()

stratum_no_discards <- stratum_no_discards %>%
  filter(!(stratum_full %in% levels(df_raised_discards_stratum_s$stratum_full))) %>%
  droplevels()

df_stratum_raised <- df_stratum_raised %>%
  add_row(stratum = "stratum_s",
          total_landings_with_noDiscards = sum(dfw_stock_overview_L_noD$Landings_Catchkg),
          raised_landings = sum(df_raised_discards_stratum_s$raised_landings)) %>%
  mutate(percentage_landings_raised_with_noDiscards = raised_landings / total_landings_with_noDiscards)

###-----------------------------------------------------------------------------
### Bind raised data
###-----------------------------------------------------------------------------
df_raised_discards <- bind_rows(df_raised_discards_stratum_c_q_f,
                                df_raised_discards_stratum_c_q_a_g,
                                df_raised_discards_stratum_c_s_a_sg,
                                df_raised_discards_stratum_s_a_sg,
                                df_raised_discards_stratum_s_a,
                                df_raised_discards_stratum_s)

dfw_stock_overview_L_noD <- dfw_stock_overview_L_noD %>%
  left_join(., df_raised_discards,
            by = join_by(Fleets, stratum_full)) %>%
  mutate(DiscardsImportedOrRaised = "Raised",
         Discards_Catchkg = raised_discards)

sum(dfw_stock_overview_L_noD$raised_landings)

sum(dfw_stock_overview_L_noD$Landings_Catchkg)

###-----------------------------------------------------------------------------
### Export percentage of landings raised by each stratum
df_stratum_raised <- df_stratum_raised %>%
  mutate(percentage_landings_raised_with_noDiscards = round(percentage_landings_raised_with_noDiscards,
                                                            digits = 4) * 100,
         total_landings = sum(dfw_stock_overview_L_D$Landings_Catchkg),
         percentage_landings_raised = round(raised_landings / total_landings,
                                            digits = 4) * 100,
         Year = i_year)

saveRDS(df_stratum_raised,
        file = glue("{path_data_raised}/df_stratum_raised.rds"))

###-----------------------------------------------------------------------------
plot_stratum_raised_percentage <- ggplot() +
  geom_col(data = df_stratum_raised,
           aes(x = stratum, y =  percentage_landings_raised_with_noDiscards),
           position = position_dodge2()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))+
  ylab("Percentage of landings with raised discards over total landings without discards")

plot_stratum_raised_percentage_total <- ggplot() +
  geom_col(data = df_stratum_raised,
           aes(x = stratum, y =  percentage_landings_raised),
           position = position_dodge2()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4)) +
  ylab("Percentage of landings with raised discards over total landings")

ggsave( plot = plot_stratum_raised_percentage_total,
        filename = glue("{path_data_raised}/plot_stratum_raised_percentage_total.png"),
        width = 20, height = 20, units = "cm")

list_plot_stratum_raised <- list(plot_stratum_raised_percentage = plot_stratum_raised_percentage,
                                 plot_stratum_raised_percentage_total = plot_stratum_raised_percentage_total)

saveRDS(list_plot_stratum_raised,
        file = glue("{path_data_raised}/df_stratum_raised.rds"))
