
## 1) Read input data

```r
# --- Option 1: CSV ---
library(data.table)

path <- getwd()
dc <- fread(file.path(path, "DC_PIL_2025_SCF.csv"))

# --- Option 2: Excel ---
library(readxl)

dc <- read_excel(
  file.path(path, "DC_PIL_SCF_2025.xlsx"),
  sheet = "Distribution (DC)"
)





```
<br>

```text
FSA::headtail(dc,2)
    recordType calendarYear yearStartDate year workingGroup       stock catchCategory
1           DC            Y            NA 2025      WGHANSA pil.27.8c9a           Lan
2           DC            Y            NA 2025      WGHANSA pil.27.8c9a           Lan
903         DC            Y            NA 2025      WGHANSA pil.27.8c9a           Lan
904         DC            Y            NA 2025      WGHANSA pil.27.8c9a           Lan
    seasonType seasonValue areaType areaValue fleetValue distributionType
1      Quarter           1 ICESArea  27.8.c.e         NA           Length
2      Quarter           1 ICESArea  27.8.c.e         NA           Length
903    Quarter           4 ICESArea  27.9.a.s         NA           Length
904    Quarter           4 ICESArea  27.9.a.s         NA           Length
    distributionUnit distributionClass ageGroupPlus attributeType attributeValue
1                 mm               125           NA           Sex              U
2                 mm               130           NA           Sex              U
903               mm               200           NA           Sex              U
904               mm               205           NA           Sex              U
    variableType variableUnit valueType     value variance numTrips numMeasurements
1         Number            k     Total  446.4716       NA        5             419
2         Number            k     Total 1288.9085       NA        7             574
903   WeightLive           kg      Mean    0.0720       NA       NA              NA
904   WeightLive           kg      Mean    0.0770       NA       NA              NA

```
<br>
<br>

## 2) `scf_to_wide()` function

<br>

<details>
<summary>📄 Check function <code>scf_to_wide()</code></summary>
  
<br>
<br>

  
```r
# -----------------------------------------------------------------------------
#' Transpose SCF Distribution (DC) Table to Wide Format
#'
#' @description
#' Transforms ICES Stock Summary Data Exchange format (SCF) distribution (DC) 
#' records from a long format into a wide matrix-like format commonly required 
#' by stock assessment models (e.g., CANUM for catch-at-age or WECA for weight-at-age).
#'
#' @param dc            Data frame. The DC table from the SCF file.
#' @param variable_type Character. The target variable type, e.g., "Number" (for CANUM) 
#'                      or "WeightLive" (for WECA).
#' @param value_type    Character. Aggregation type: "Total" (values are summed) 
#'                      or "Mean" (weighted mean by abundance numbers).
#' @param ages          Numeric vector. Range of output ages for the matrix columns (default is 0:8).
#' @param plus_group    Logical. If `TRUE`, aggregates all ages greater than `max(ages)` 
#'                      into the maximum age class (plus group).
#' @param grouping_vars Character vector. Variables defining the output rows. Default is `"year"`. 
#'                      Examples: `c("year","seasonValue")`, `c("year","seasonValue","areaValue")`, 
#'                      `c("year","catchCategory")`, or `c("year","attributeValue")` (for sex breakdown).
#' @param weight_var    Character. The `variableType` used as weights when `value_type = "Mean"` 
#'                      (defaults to `"Number"`).
#' @param fill          Numeric. Value assigned to age classes with missing data (defaults to `0`).
#' @param scale         Character. Row-wise scaling option: `"none"` (absolute values), 
#'                      `"proportion"` (rows sum to 1), or `"percent"` (rows sum to 100).
#' 
#' @return A wide-format `tibble` containing the grouping variables followed by 
#'         one column per age class.
#' 
#' @export
# -----------------------------------------------------------------------------

<br>


scf_to_wide <- function(dc,
                        variable_type = "Number",
                        value_type    = "Total",
                        ages          = 0:8, #The last Age will be the plusgrop
                        plus_group    = TRUE,
                        grouping_vars = "year",
                        weight_var    = "Number",
                        fill          = 0,
                        scale         = c("none", "proportion", "percent")) {
  
  value_type <- match.arg(value_type, c("Total", "Mean"))

  absence <- setdiff(grouping_vars, names(dc))
  if (length(absence) > 0)
    stop("grouping_vars not present in dc: ", paste(absence, collapse = ", "))
  
  dc <- dc %>% filter(distributionType == "Age")
  
  # Identify key columns defining a unique record (excluding metrics and metadata qualities)

  no_key <- c("variableType", "variableUnit", "valueType", "value", "variance",
              "PSU", "numPSUs", "numTrips", "numMeasurements")
  key_cols <- setdiff(names(dc), no_key)
  
  dat <- dc %>%
    filter(variableType == variable_type, valueType == value_type)
  if (nrow(dat) == 0)
    stop("No records found with variableType = '", variable_type,
         "' and valueType = '", value_type, "'")
  
  units <- unique(dat$variableUnit)
  if (length(units) > 1)
    warning("Multiple units found for ", variable_type, ": ",
            paste(units, collapse = ", "), ". Please homogenize before aggregating.")
  
  # Weights for weighted means: match with abundance numbers of the exact same record
  if (value_type == "Mean") {
    w <- dc %>%
      filter(variableType == weight_var, valueType == "Total") %>%
      select(all_of(key_cols), w = value)
    dat <- left_join(dat, w, by = key_cols)
    if (any(is.na(dat$w))) {
      warning("'Mean' records without associated ", weight_var,
              ": weight of 1 will be used for those records.")
     dat$w[is.na(dat$w)] <- 1
    }
  } else {
    dat$w <- 1
  }
  
  # Age handling: plus group aggregation and filtering
  dat <- dat %>% mutate(age = distributionClass)
  if (plus_group) dat <- dat %>% mutate(age = pmin(age, max(ages)))
 out <- sum(dat$age < min(ages) | dat$age > max(ages))
  if (out > 0) message(out, " records outside the age range were discarded")
  dat <- dat %>% filter(age >= min(ages), age <= max(ages))
  
  # Aggregation by grouping variables and age
  agg <- dat %>%
    group_by(across(all_of(grouping_vars)), age) %>%
    summarise(
      value = if (value_type == "Total") sum(value, na.rm = TRUE)
      else weighted.mean(value, w, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Pivot to wide format, ensuring all age classes in the range are present
  wide <- agg %>%
    mutate(age = factor(age, levels = ages)) %>%
    pivot_wider(names_from = age, values_from = value,
                values_fill = fill, names_expand = TRUE) %>%
    arrange(across(all_of(grouping_vars)))
  
  # Row-wise scaling (only applicable for totals, e.g., numbers)
  scale <- match.arg(scale)
  if (scale != "none") {
    if (value_type == "Mean")
      stop("scale only applies to value_type = 'Total' (not meaningful for means)")
    age_cols <- as.character(ages)
    tot <- rowSums(wide[, age_cols], na.rm = TRUE)
    k <- if (scale == "percent") 100 else 1
    wide[, age_cols] <- k * wide[, age_cols] / ifelse(tot > 0, tot, NA)
  }
  wide
}

```
</details>

<br>

## 3) Examples

<br>

### 3.1 Basic aggregation by year (by default)

<br>

```r
canum<-scf_to_wide(dc)
```
<br>

```text
FSA::headtail(canum)

   year    `0`     `1`    `2`    `3`    `4`    `5`    `6`   `7`   `8`
1  2025 51983. 170632. 81450. 58375. 50528. 33892. 10244.  908.  139.
```
<br>

### 3.2. Grouping by quarter (seasonValue)
<br>

```r
scf_to_wide(dc, grouping_vars = c("year", "seasonValue"))
```
<br>

```text

   year seasonValue    `0`    `1`    `2`    `3`    `4`    `5`   `6`    `7`   `8`
1  2025           1     0  55507. 12645.  7066.  2289.  2604.  329.   4.78    0 
2  2025           2     0  50511. 27334. 35283. 20267. 21829. 4819. 469.    139.
3  2025           3 46271. 60293. 34040. 13284. 21656.  6362. 3204. 301.      0 
4  2025           4  5711.  4321.  7432.  2741.  6317.  3097. 1891. 134.      0
```

<br>

### 3.3 Grouping by quarter and ICESarea
<br>

```r
scf_to_wide(dc, grouping_vars = c("year", "seasonValue", "areaValue"))
```
<br>

```text
year seasonValue areaValue        0           1          2          3          4
1  2025           1  27.8.c.e    0.000 6688.714076 1113.24974  803.88864  208.90561
2  2025           1  27.8.c.w    0.000    0.000000   25.67944  216.66492  534.81176
3  2025           1  27.9.a.n    0.000 2007.870007 2476.01532 5986.24858 1516.13073
17 2025           4  27.9.a.c    0.000    6.227613   62.42054   23.48611   55.90118
18 2025           4  27.9.a.n    0.000  666.582172 6681.27877 2513.87153 5983.46942
19 2025           4  27.9.a.s 5709.698 3458.835035  605.80277    0.00000    0.00000
            5           6          7 8
1    81.75286    2.747657   0.000000 0
2   799.08038  152.371859   4.779537 0
3  1710.93140  173.676290   0.000000 0
17   28.66899   17.505725   1.242506 0
18 3068.62920 1873.752263 132.993536 0
19    0.00000    0.000000   0.000000 0

```
<br>

### 3.4. Scaling output to proportions

<br>

```r
scf_to_wide(dc, scale = "proportion",grouping_vars = c("year", "seasonValue")) 
```
<br>

```text
year seasonValue         0         1         2          3          4          5
1  2025           1 0.0000000 0.6900082 0.1571841 0.08784136 0.02844986 0.03236988
2  2025           2 0.0000000 0.3144125 0.1701462 0.21962629 0.12615597 0.13587783
3  2025           3 0.2495622 0.3251871 0.1835903 0.07164776 0.11679868 0.03431073
21 2025           2 0.0000000 0.3144125 0.1701462 0.21962629 0.12615597 0.13587783
31 2025           3 0.2495622 0.3251871 0.1835903 0.07164776 0.11679868 0.03431073
4  2025           4 0.1804888 0.1365460 0.2348475 0.08661428 0.19961516 0.09787943
             6            7            8
1  0.004087242 5.941416e-05 0.0000000000
2  0.029998721 2.918597e-03 0.0008638569
3  0.017282008 1.621285e-03 0.0000000000
21 0.029998721 2.918597e-03 0.0008638569
31 0.017282008 1.621285e-03 0.0000000000
4  0.059766688 4.242067e-03 0.0000000000
```

<br>


### 3.5. WECA: weighted mean by Age
Each cell indicates the average weight of a fish of that age in the catch.
<br>

```r
weca<-scf_to_wide(dc, variable_type = "WeightLive", value_type = "Mean",  grouping_vars = c("year", "seasonValue"))
```
<br>


```text
FSA::headtail(weca,2)
year seasonValue    `0`    `1`    `2`    `3`    `4`    `5`    `6`    `7`   `8`
  <dbl>       <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl> <dbl>
1  2025           1 0      0.0188 0.0266 0.0432 0.0578 0.0596 0.0681 0.075    0  
2  2025           2 0      0.0252 0.0391 0.0530 0.0678 0.0699 0.0815 0.0872   0.1
3  2025           3 0.0210 0.0401 0.0576 0.0666 0.0785 0.0849 0.0901 0.0983   0  
4  2025           4 0.0232 0.0497 0.0634 0.0720 0.0828 0.0843 0.0868 0.0843   0
```

<br>





## 4. Export to  Lowestoft/VPA format function
<br>

<details>
<summary>📄 Check Function <code>write_lowestoft()</code></summary>
  
 <br> 
 
```r
# -----------------------------------------------------------------------------
# Data in Lowestoft/VPA format (CANUM, WECA, ...), one row per year
# -----------------------------------------------------------------------------
write_lowestoft <- function(wide, file, title = "CANUM", file_type = 2,
                            year_col = "year") {
  if (anyDuplicated(wide[[year_col]]))
    stop("Más de una fila por año: el formato Lowestoft solo admite year como grupo")
  ages <- as.numeric(setdiff(names(wide), year_col))
  yrs  <- wide[[year_col]]
  lines <- c(
    title,
    paste(1, file_type),
    paste(min(yrs), max(yrs)),
    paste(min(ages), max(ages)),
    "1",
    apply(as.matrix(wide[, as.character(ages)]), 1,
          function(x) paste(x, collapse = " "))
  )
  writeLines(lines, file)
  invisible(lines)
}

```
</details>

<br>

```r

canum_txt <- scf_to_wide(dc) |> write_lowestoft("canum.txt", title = "CANUM")

```
<br>

```text

canum_txt
[1] "CANUM"                                                                                                                                                  
[2] "1 2"                                                                                                                                                    
[3] "2025 2025"                                                                                                                                              
[4] "0 8"                                                                                                                                                    
[5] "1"                                                                                                                                                      
[6] "51982.8683533959 170631.92994734 81449.8139391589 58374.5596043714 50528.0281576734 33891.7269276588 10243.6394097789 908.494014060945 138.779269432304"

```
