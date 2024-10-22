#-*- coding: utf-8 -*-

### File: report_3_doc.R
### Time-stamp: <2023-10-06 09:28:55 a23579>
###
### Created: 06/10/2023	09:28:31
### Author: Yves Reecht
###
####################################################################################################
### Description:
###
###
####################################################################################################

library(rmarkdown)

source("utilities.R")

# combine into a word document
render("report.Rmd",
       output_file = "report.docx",
       encoding = "UTF-8",
       output_dir = "report")







### Local Variables:
### ispell-local-dictionary: "english"
### fill-column: 100
### End:
