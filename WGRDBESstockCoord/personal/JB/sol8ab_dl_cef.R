################################################################################
### Comparison of CEF data from IC convertion and RDBES download
### comes from Damian's codes with edit for age data
################################################################################

### Packages
library("tidyverse")
library("here")
library("glue")
library("icesRDBES")
library("rcef") # remotes::install_github("DVillagra26/rcef", dependencies = TRUE, force = TRUE)
library("icesTAF")

### Settings
data_year <- 2025
path_to_data <- here("WGRDBESstockCoord/personal/JB/data")
path_data_cef <- here(glue("{path_to_data}/rdbes_cef/{data_year}"))

### Token for dl rdbes data
decoded_token <- decode_token()

################################################################################
### Download RDBES data
### My filter for sole in 8ab
my_filter <- list(
  dataType = "CEF",
  format = "CsvFilePerTable",
  hierarchies = list("HNI", "HEN", "HSN"),
  CEFFilters = list(
    CNVesselFlagCountry = list("All"),
    CNstock = list("sol.27.8ab"),
    CNyear = as.list(as.character(data_year)),
    ENVesselFlagCountry = list("All"),
    ENyear =  as.list(as.character(data_year)),
    ENStockForDeterminingAreas = list("sol.27.8ab"),
    SNVesselFlagCountry = list("All"),
    SNstock = list("sol.27.8ab"),
    SNyear =  as.list(as.character(data_year))
  )
)

### download data given filter
icesRDBES::rdbes_download_data(my_filter,
                               dest_dir = path_data_cef)

### unzip files in the same directory
walk(here(glue("{path_data_cef}/{list.files(path_data_cef, pattern = '\\\\.zip$')}")), \(zip_file)
     archive::archive_extract(zip_file, dir = here(path_data_cef)))

### load csv tables as dataframe
df_rdbes_cn <- read_csv(file = glue("{path_data_cef}/HNI/CatchNatEst.csv"),
                        col_names = TRUE)
df_rdbes_dn <- read_csv(file = glue("{path_data_cef}/HNI/DistributionNatEst.csv"),
                        col_names = TRUE)
