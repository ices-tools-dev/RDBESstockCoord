
test <-  list_cef$catches %>%
  cef_add_strata(.,only_full_strata = TRUE)

testf = filter(test, strata_full ==

"sol.27.8ab-France-1-27.8.a-GNS_DEF_100-119_0_0_all-GNS_DEF_100-119_0_0_all")



testf_2 = filter(test, strata_full == "sol.27.8ab-France-1-27.8.a-GTR_DEF_100-119_0_0_all-GTR_DEF_100-119_0_0_all")


si <- read_csv("WGRDBESstockCoord/personal/JB/data/tmp/si.csv")


test_si <- si %>% filter(Fleet == "GNS_DEF_100-119_0_0_all",
                         Season == "1",
                         FishingArea == "27.8.a")
