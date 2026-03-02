
library(data.table)
library(icesVocab)
library(stringr)

## Paths:
path <- "./R" 
dat_path <- "./WGRDBESstockCoord/personal/jost/data_overviews_format/tur"

source(file.path(path, "funMakeRelation.R"))
source(file.path(path, "funICoutCEF.R"))
source(file.path(path, "funIntercatchCEF.R"))

res <- funICoutCEF(dat_path = dat_path, 
                   years = 2022,
                   metier6 = "fleet",
                   output_format = "to_list",
                   out_path = dat_path,
                   keep_temp_file = TRUE,
                   file_prefix = "")



