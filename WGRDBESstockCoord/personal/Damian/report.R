## Report: the stock coordination of brill (bll.27.3a47de, WGNSSK 2026), from the converted InterCatch data to the CC and the DC

## The text, the tables and the choice of figures are in report.qmd; this script renders it to html and pdf.
## It reads what the other scripts leave and shows the figures they saved: nothing is raised, allocated or drawn again.

## Before: data/catch_data_CEF.csv, data/distributions_data_CEF.csv, model/catch_data_raised.csv,
##         model/catch_data_allocated.csv, model/distributions_data_allocated.csv, output/SCF_CC.csv, output/SCF_DC.csv,
##         output/raising_scenarios_comparison.csv, output/figures/*.png
## After:  report/report.html, report/report.pdf

library(icesTAF)

mkdir("report")

if (is.null(quarto::quarto_path()))
  stop("Quarto was not found: install it (https://quarto.org; RStudio has one) or give its path in the variable QUARTO_PATH")

msg("Report: the stock coordination of bll.27.3a47de, html and pdf")
quarto::quarto_render("report.qmd", output_format = "all")

## Quarto leaves its files next to the qmd: they belong in report/
for (f in c("report.html", "report.pdf"))
  if (file.exists(f)) {
    file.copy(f, file.path("report", f), overwrite = TRUE)
    file.remove(f)
  }
