## Report: the stock coordination of brill (bll.27.3a47de, WGNSSK), from the converted InterCatch data to the CC and the DC

## The text, the tables and the choice of figures are in report.qmd; this script renders it to html, pdf and Word.
## It reads what the other scripts leave and shows the figures they saved: nothing is raised, allocated or drawn again.

## Before: data/catch_data_CEF.csv, data/distributions_data_CEF.csv, model/catch_data_raised.csv (after the allocation: the catch of the
##         allocation), model/distributions_data_allocated.csv, output/SCF_CC.csv, output/SCF_DC.csv,
##         output/2c_discard_raising_comparison/raising_methods.csv (when the raisings were compared) and the figures in
##         output/figures/<folder>/<script>_<n>_<name>.png, which the report finds by their name
## After:  report/report.html, report/report.pdf, report/report.docx (on the template of ICES Scientific Reports,
##         which is in report_template/: ICES_Scientific_Reports_chairs_template_2025.docx and what is made of it)

library(icesTAF)
library(here)

## The project folder, wherever the repo (RDBESstockCoord) is cloned: every path below is relative to it
here::i_am("WGRDBESstockCoord/personal/Damian/2024/report.R")
setwd(here("WGRDBESstockCoord", "personal", "Damian", "2024"))

mkdir("report")

if (is.null(quarto::quarto_path()))
  stop("Quarto was not found: install it (https://quarto.org; RStudio has one) or give its path in the variable QUARTO_PATH")

## The catch year is the one of the data: the subtitle of the report says it, and the working group of the year after
## (quarto's own -M flag: it beats the subtitle written in report.qmd, which the metadata= argument of quarto_render() does not)
catch_year <- as.integer(max(read.taf("data/catch_data_CEF.csv")$year))
subtitle <- sprintf("From the converted InterCatch data to the Catch Combined and the Distributions Combined, catch year %d (WGNSSK %d)",
                    catch_year, catch_year + 1L)

msg("Report: the stock coordination of bll.27.3a47de, catch year ", catch_year, ": html, pdf and Word")
quarto::quarto_render("report.qmd", output_format = "all", quarto_args = c("-M", paste0("subtitle:", subtitle)))

## The Word report is written on the template of the ICES Scientific Reports (report_template/ices_reference.docx); what Quarto
## leaves to do for the template is done here: the tables of one cell around the figures and the tables go, the captions and the
## cells of the tables get the styles of the template
source("report_template/ices_fixups.R")
ices_fix_docx("report.docx")

## Quarto leaves its files next to the qmd: they belong in report/
for (f in c("report.html", "report.pdf", "report.docx"))
  if (file.exists(f)) {
    file.copy(f, file.path("report", f), overwrite = TRUE)
    file.remove(f)
  }
