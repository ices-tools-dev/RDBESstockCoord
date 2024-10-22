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
library("remotes")
remotes::install_gitlab(repo = "emh-lab/emhutils",
                        host = "https://gitlab.ifremer.fr")
library("emhUtils")
library("FLCore")
library("here")
library("icesTAF")

###-----------------------------------------------------------------------------
### Make data raw and data tidy path
###-----------------------------------------------------------------------------
source("R/fun/make_path.R")

###-----------------------------------------------------------------------------
### function specific to isis: source them all
###-----------------------------------------------------------------------------
invisible(sapply(paste0(here("R", "fun", "project", "/"),
                        list.files(path = here("R", "fun", "project"))),
                 source))

