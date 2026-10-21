################################################################################
### Download CEF data from RDBES for sole 8ab
################################################################################

### Packages
library("tidyverse")
library("here")
library("glue")
library("icesRDBES")


decoded_token <- decode_token()

###
path_to_data <- here("WGRDBESstockCoord/personal/JB/data")
path_data_cef <- here(path_to_data, "rdbes_cef")
dir.create(path_data_cef, showWarnings = FALSE, recursive = TRUE)
year_range <- 2025

###
my_filter <- list(
  dataType = "CEF",
  format = "CsvFilePerTable",
  hierarchies = list("HNI", "HEN", "HSN"),
  CEFFilters = list(
    CNVesselFlagCountry = list("All"),
    CNstock = list("sol.27.8ab"),
    CNyear = as.list(as.character(year_range)),
    ENVesselFlagCountry = list("All"),
    ENyear =  as.list(as.character(year_range)),
    ENStockForDeterminingAreas = list("sol.27.8ab"),
    SNVesselFlagCountry = list("All"),
    SNstock = list("sol.27.8ab"),
    SNyear =  as.list(as.character(year_range))
  )
)

### download data given filter
icesRDBES::rdbes_download_data(my_filter,
                               dest_dir = path_data_cef)

### unzip files in the same directory
walk(here(glue("{path_data_cef}/{list.files(path_data_cef, pattern = '\\\\.zip$')}")), \(zip_file)
     archive::archive_extract(zip_file, dir = here(path_data_cef)))


### load csv tables as dataframe
df_rdbes_cn <- read_csv(file = glue("{path_data_cef}/HNI/CatchNatEst.csv.csv"), col_names = FALSE)
df_rdbes_dn <- read_csv(file = glue("{path_data_cef}/HNI/DistributionNatEst.csv"), col_names = FALSE)
