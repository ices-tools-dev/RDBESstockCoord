


```r
# --- Option 1: CSV ---
library(data.table)

path <- getwd()
dc <- fread(file.path(path, "DC_PIL_2025_SCF.csv"))

# --- Option 2: Excel ---
library(readxl)

dc <- read_excel(
  file.path(path, "PIL_SCF_2025.xlsx"),
  sheet = "Distribution (DC)"
)

cc <- read_excel(
  file.path(path, "PIL_SCF_2025.xlsx"),
  sheet = "Catch (CC)"
)

FSA::headtail(dc, 2)

```


| recordType | calendarYear | year | workingGroup | stock | catchCategory | seasonValue | areaValue |
|---|---:|---:|---|---|---|---:|---|
| DC | Y | 2025 | WGHANSA | pil.27.8c9a | Lan | 1 | 27.8.c.e |
| DC | Y | 2025 | WGHANSA | pil.27.8c9a | Lan | 1 | 27.8.c.e |
| DC | Y | 2025 | WGHANSA | pil.27.8c9a | Lan | 4 | 27.9.a.s |
| DC | Y | 2025 | WGHANSA | pil.27.8c9a | Lan | 4 | 27.9.a.s |








# -----------------------------------------------------------------------------
# scf_to_wide()
#
#   dc            : DC table from the SCF (data.frame)
#   variable_type : "Number" (CANUM), "WeightLive" (WECA), etc.
#   value_type    : "Total" -> summed; "Mean" -> weighted mean by numbers
#   ages          : Range of output ages (columns). Ages with no data -> fill
#   plus_group    : TRUE aggregates ages > max(ages) into the plus group
#   grouping_vars : Variables defining the rows. Default is "year".
#                   Ex: c("year","seasonValue"), c("year","seasonValue","areaValue"),
#                       c("year","catchCategory"), c("year","attributeValue") (sex)
#   weight_var    : variableType used for weighting means
#   fill          : Value for ages with no data
#   scale         : "none" (absolute values), "proportion" (sum to 1 per row)
#                   or "percent" (sum to 100 per row)
# -----------------------------------------------------------------------------
