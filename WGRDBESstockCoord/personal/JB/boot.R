################################################################################
### Load packages, set R session options, load project specific options
################################################################################

options(pillar.print_max = 50,
        digits = 10,
        scipen = 999)
###-----------------------------------------------------------------------------
### Load packages
###-----------------------------------------------------------------------------
library("tidyverse")
library("glue")
library("here")
library("readr")
here::i_am("README.md")

###-----------------------------------------------------------------------------
### function specific to isis: source them all
###-----------------------------------------------------------------------------
invisible(sapply(paste0(here("fun", "/"),
                        list.files(path = here("fun"))),
                 source))

