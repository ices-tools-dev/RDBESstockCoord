
library(data.table)
library(icesVocab)
library(stringr)

## Paths:
path <- "./R" 
dat_path <- "./data/IC_exchange"


source(file.path(path, "funMakeRelation.R"))
source(file.path(path, "funIntercatchCEF.R"))

stock_relation <- funMakeRelation(year(Sys.Date()))

funIntercatchCEF(dat_path = dat_path, 
                        stock_relation = stock_relation, 
                        output_format = "to_environment")
