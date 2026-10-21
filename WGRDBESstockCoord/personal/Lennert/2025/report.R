## Report: the stock coordination of brill (bll.27.3a47de, WGNSSK 2026), from the converted InterCatch data to the CC and the DC

## The text, the tables and the choice of figures are in report.qmd; this script renders it to html, pdf and Word.
## It reads what the other scripts leave and shows the figures they saved: nothing is raised, allocated or drawn again.

## Before: data/catch_data_CEF.csv, data/distributions_data_CEF.csv, model/catch_data_raised.csv,
##         model/catch_data_allocated.csv, model/distributions_data_allocated.csv, output/SCF_CC.csv, output/SCF_DC.csv,
##         output/raising_scenarios_comparison.csv, output/figures/*.png
## After:  report/report.html, report/report.pdf, report/report.docx (on the template of ICES Scientific Reports,
##         which is in report_template/: ICES_Scientific_Reports_chairs_template_2025.docx and what is made of it)

library(icesTAF)
library(here)

## The project folder, wherever the repo (RDBESstockCoord) is cloned: every path below is relative to it
here::i_am("WGRDBESstockCoord/personal/Lennert/2025/report.R")
setwd(here("WGRDBESstockCoord", "personal", "Lennert", "2025"))

mkdir("report")

if (is.null(quarto::quarto_path()))
  stop("Quarto was not found: install it (https://quarto.org; RStudio has one) or give its path in the variable QUARTO_PATH")

msg("Report: the stock coordination of tur.27.4, html, pdf and Word")
quarto::quarto_render("boot/initial/report_template/report.qmd", output_format = "all")

## The Word report is written on the template of the ICES Scientific Reports (report_template/ices_reference.docx); what Quarto
## leaves to do for the template is done here: the tables of one cell around the figures and the tables go, the captions and the
## cells of the tables get the styles of the template
source("boot/initial/report_template/ices_fixups.R")
ices_fix_docx("report.docx")

## Quarto leaves its files next to the qmd: they belong in report/
for (f in c("report.html", "report.pdf", "report.docx"))
  if (file.exists(f)) {
    file.copy(f, file.path("report", f), overwrite = TRUE)
    file.remove(f)
  }
