source("boot.R")
source("fun/census_add_stratum.R")

### -----------------------------------------------------------------------------g
### load cenusu data
### -----------------------------------------------------------------------------

### read file and make stratum for discards raising
df_gadus_census <- readr::read_csv(file = "data/RCEF_v14_Gadus_census.csv") %>%
    census_add_stratum()



df_gadus_census <- df_gadus_census %>%
    census_remove_stratum()
