################################################################################
### Download CEF data from RDBES for sole 8ab
################################################################################

### Packages
library("tidyverse")
library("here")
library("glue")
library("icesRDBES")

###
path_data_scf <- "data/raw/scf/"
dir.create(path_data_scf, showWarnings = FALSE, recursive = TRUE)
year_range <- 2025

###
my_filter <- list(
  dataType = "SCF",
  format = "SingleCsvFile",
  hierarchies = list("HCI"),
  scfFilters = list(
    ccStock = list("sol.27.8ab"),
    ccYear = as.list(as.character(year_range))
  )
)

### download data given filter
icesRDBES::rdbes_download_data(my_filter, 
                               dest_dir = path_data_scf)

### unzip files in the same directory
unzip(
  zipfile = here(glue("{path_data_cef}{list.files(path_data_scf, pattern = '\\\\.zip$')}")),
  exdir   = here(path_data_scf),
  overwrite = TRUE
)

### load csv tables as dataframe
df_ni <- read_csv(file = glue("{path_data_cef}HNI.csv"), col_names = FALSE)
df_cn <- read_csv(file = glue("{path_data_cef}HEN.csv"), col_names = FALSE)
df_dn <- read_csv(file = glue("{path_data_cef}HSN.csv"), col_names = FALSE)
