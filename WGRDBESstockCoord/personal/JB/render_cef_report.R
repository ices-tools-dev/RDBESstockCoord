################################################################################
### Render compare_cef_report.qmd with the catch year in the name of the html
### e.g. compare_cef_report_2024.html
################################################################################

library("quarto")
library("here")
library("glue")

data_year <- 2025

quarto_render(
  input = here("WGRDBESstockCoord/personal/JB/compare_cef_report.qmd"),
  output_file = glue("compare_cef_report_{data_year}.html"),
  execute_params = list(data_year = data_year)
)
