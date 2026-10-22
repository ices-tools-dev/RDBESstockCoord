# Sole 27.8ab: CEF data from InterCatch (IC) vs CEF data from the RDBES

This folder holds the scripts used to build, for **sol.27.8ab** (catch year 2024 by default), two versions of
the same "CEF" data set (catches + age distributions) and to compare them:

| Label | Source of the catches and of the age distributions |
|-------|----------------------------------------------------|
| **CEF-IC** | InterCatch exports (`StockOverview.txt`, `NumbersAtAgeLength.txt`, `MeanWeigthAtAgeLength.txt`) converted to the CEF format with `funICoutCEF()` |
| **CEF-RDBES** | `CatchNatEst` and `DistributionNatEst` tables downloaded from the RDBES (CEF data type, hierarchies HNI / HEN / HSN) |

For each source the same two processing steps are applied:

1. **Discards raising**: discards are raised to landings strata that have no discards, with a cascade of strata
   from the most specific to the most aggregated.
2. **Age allocation**: age distributions (numbers and mean weight at age, SOP-corrected) are allocated to
   the strata with no age data, with the same cascade of rules, plus a plus group (`max_agestock <- 8`).

The two results are then compared (tables, Excel workbooks, figures, an HTML report).

## Conventions

- **Working directory**: scripts call `source("R/...")` and `here("WGRDBESstockCoord/personal/JB/...")`, so run
  them from the **root of the repository** (`RDBESstockCoord/`, where `RDBESstockCord.Rproj` is), not from this folder.
- Settings (`data_year`, `max_agestock`, labels, tolerances) are at the top of each script.
- Shared functions live in the repository-level `R/` folder (`funICoutCEF.R`, `cef_add_strata.R`,
  `raise_discards_strata.R`, `cef_allocate_age_strata.R`, `sop.R`, ...); `fun/` contains older helpers for the
  IC-only workflow (`boot.R` sources it).
- Required packages: `tidyverse`, `here`, `glue`, `data.table`, `icesVocab`, `icesSD`, `icesRDBES`, `icesTAF`,
  `rcef` (`remotes::install_github("DVillagra26/rcef")`), `archive`, `openxlsx` (optional, for the workbooks),
  `quarto` (for the report).

## Order of execution

```
 0. sol8ab_dl_cef.R                           download RDBES CEF (needs an ICES token)
 0b. comparison_cef_ic_rdbes_input_data.R    check IC conversion vs RDBES inputs (optional for 1b-5)
        │
        ├── CEF-IC branch                            ├── CEF-RDBES branch
 1a. sole8ab_cef_ic_discards_raising.R        1b. sole8ab_cef_rdbes_discards_raising.R
 2a. sole8ab_cef_ic_age_allocation.R          2b. sole8ab_cef_rdbes_age_allocation.R
        │                                             │
        └────────────────────┬────────────────────────┘
 3. compare_cef_raised_datasets.R        (raised catches, 1a vs 1b)
 4. compare_cef_age_allocation.R         (age allocation, 2a vs 2b)
 5. compare_cef_report.qmd               (HTML report of 3 + 4)
```

Within a branch, `a` must run before its `2` (the age allocation reads the raised catches). The IC and RDBES
branches are independent. Steps 3 and 4 need both branches done.

### Step 0 - `sol8ab_dl_cef.R` (RDBES CEF download)

- **What it does**: downloads the RDBES CEF tables for sol.27.8ab (all flag countries, hierarchies HNI / HEN / HSN, `data_year`)
  with `icesRDBES::rdbes_download_data()`, unzips them and reads `CatchNatEst` and `DistributionNatEst`. Needs an ICES token
  (`decode_token()`): run it in an interactive session where you are logged in.
- **Input**: RDBES web service.
- **Output**: `data/rdbes_cef/{year}/` : downloaded zips and unzipped tables (`HNI/CatchNatEst.csv`, `HNI/DistributionNatEst.csv`,
  `HEN/`, `HSN/`). **Steps 1b and 2b depend on this download.**

### Step 0b - `comparison_cef_ic_rdbes_input_data.R` (inputs check)

- **What it does**: converts the IC files to CEF (`funICoutCEF`) and compares them with the RDBES tables stratum by stratum
  (status `same`, `differs`, `only IC`, `only RDBES`), by country, area, season and metier4. It repeats the RDBES download of
  step 0 (same filter), so it can also be used alone to get the data; it is not needed by steps 1b to 5 once step 0 is done.
- **Input**: `data/{year}/StockOverview.txt`, `NumbersAtAgeLength.txt`, `MeanWeigthAtAgeLength.txt` (IC exports); RDBES web
  service (token).
- **Output**: `data/rdbes_cef/{year}/` (as step 0) and `output/rdbes_cef_comparison/{year}/1_IC_conversion_vs_CEF_download/` :
  tables (csv), `IC_conversion_vs_CEF_download.xlsx` (one sheet per table, `catch_compare` first) and figures in `figures/`
  (catch, catch with age, numbers at age).

### Step 1a - `sole8ab_cef_ic_discards_raising.R`

- **What it does**: IC export → CEF format (`funICoutCEF`), keeps WGEstimate records, splits landings strata with and without
  discards, raises discards on the strata cascade `strata_c_q_f` → `strata_c_q_a_g` → `strata_c_q_a_sg` → `strata_q_a_sg`
  → `strata_q_a` → `strata_q` (c = country, q = quarter, f = fleet, a = area, g = gear, sg = super-gear), checks that
  landings are conserved.
- **Input**: `data/{year}/StockOverview.txt` (+ the two other IC files, read by `funICoutCEF`).
- **Output** in `output/{year}/cef_ic/`:
  - `df_stock_overview_L_D_raised{year}.rds` : long format, one row per stratum and catch category, `Catchkg`, with how the
    discards were obtained (imported / raised and with which stratum).
  - `dfw_stock_overview_L_D_raised_{year}.rds` : same, wide (Lan / Dis in columns).
- At its end it sources `load_IC_raised_allocated_2024.R` (loads the older IC-only raising from `data/2024/df_stock_overview_raised_w_2024_IC.rds`, for comparison).

### Step 1b - `sole8ab_cef_rdbes_discards_raising.R`

- **What it does**: the same raising as 1a, but the catches come from the RDBES `CatchNatEst` table (columns `CN*` renamed to the
  IC-conversion names, only WGEstimate records, `DisBMS` treated as `Dis`, tonnes converted to kg).
- **Input**: `data/rdbes_cef/{year}/HNI/CatchNatEst.csv` (from step 0).
- **Output** in `output/{year}/cef_rdbes/`: `df_stock_overview_L_D_raised{year}.rds`, `dfw_stock_overview_L_D_raised_{year}.rds`.

### Step 2a - `sole8ab_cef_ic_age_allocation.R`

- **What it does**: links the IC age distributions to the raised catches (via `domainBiology`), applies the SOP correction
  (`sole8ab_age_allocation_sop.R`, sourced; uses `R/sop.R`), then allocates an age distribution to the strata without age
  data with ordered rules from the most specific to the most aggregated (the year-level rule takes what is left), and
  builds the plus group (`max_agestock`). Finally compares with the earlier IC allocation through
  `load_IC_age_allocated_2024.R`.
- **Input**: `data/{year}/*.txt` (IC), `output/{year}/cef_ic/df_stock_overview_L_D_raised{year}.rds` (from 1a).
- **Output** in `output/{year}/cef_ic/`: `df_canum_all_age_{year}.rds` (numbers and mean weight at age by stratum,
  catch category and age, `Source` = import or allocation rule), `df_canum_ageplus_{year}.rds`, `df_weights_strata_{year}.rds`,
  `df_weights_CatchCat_Age_{year}.rds`, `df_weights_Catch_Age_{year}.rds`, `dfs_canum_allocated.rds`, `plot_canum_allocated.rds`,
  `plot_sop.png`, `plot_sop_ratio.png`, `list_sop.rds`.

### Step 2b - `sole8ab_cef_rdbes_age_allocation.R`

- **What it does**: same age allocation as 2a, but the distributions come from the RDBES `DistributionNatEst` table
  (age only, numbers NE3 = thousands → N, mean weights kg → g, `DisBMS` → `Dis`) and the catches are the ones raised in 1b.
- **Input**: `data/rdbes_cef/{year}/HNI/DistributionNatEst.csv`, `output/{year}/cef_rdbes/df_stock_overview_L_D_raised{year}.rds`.
- **Output** in `output/{year}/cef_rdbes/`: same set of files as 2a (`df_canum_all_age_{year}.rds`, `df_canum_ageplus_{year}.rds`,
  `df_weights_*.rds`, `dfs_canum_allocated.rds`, `plot_canum_allocated.rds`, SOP plots and `list_sop.rds`).

### Step 3 - `compare_cef_raised_datasets.R`

- **What it does**: compares the two raised data sets stratum by stratum (a difference is ignored under `tol_kg = 1` kg or
  `tol_pct = 0.1` %): totals by catch category, by country, by way the discards were obtained, by metier5, ratio discards/landings.
- **Input**: `output/{year}/cef_rdbes/df_stock_overview_L_D_raised{year}.rds` and `output/{year}/cef_ic/df_stock_overview_L_D_raised{year}.rds`
  (files and labels `label_cef_rdbes` / `label_cef_ic` can be changed at the top to compare any two runs).
- **Output**: `output/cef_raised_CEF-RDBES_vs_CEF-IC/{year}/` with `tables/*.csv`, `cef_raised_comparison.xlsx`, `figures/*.png`,
  `list_plots.rds`. The folder is wiped and rebuilt at each run.

### Step 4 - `compare_cef_age_allocation.R`

- **What it does**: compares the age allocations (numbers at age, mean weight at age, which allocation rule was used) in total,
  by age, by `Source`, by stratum, by metier5, with a tolerance in number of fish or percent.
- **Input**: `df_canum_all_age_{year}.rds` from `output/{year}/cef_rdbes/` and from `output/{year}/cef_ic/`
  (falls back to `output/df_canum_all_age_{year}.rds` for the IC side, where 2a currently writes it).
- **Output**: `output/cef_age_CEF-RDBES_vs_CEF-IC/{year}/` with `tables/` (`total`, `age`, `source`, `strata`, `strata_age`, ...),
  `cef_age_comparison.xlsx`, `figures/*.png`, `list_plots.rds`.

### Step 5 - `compare_cef_report.qmd`

- **What it does**: HTML report (self-contained) summarising steps 3 and 4: what is compared, discards raising, age allocation.
  Render with `render_cef_report.R` (set `data_year` at its top), which passes the year to the report as a parameter.
- **Input**: `list_plots.rds` and `tables/*.csv` of the two comparison folders (steps 3 and 4).
- **Output**: `compare_cef_report_{year}.html` (in this folder).

## Helper and legacy scripts

| File | Role |
|------|------|
| `sole8ab_age_allocation_sop.R` | Sourced by 2a and 2b. Sum Of Products correction (`sop()` from `R/sop.R`) of the imported numbers at age by stratum; writes the SOP plots and `list_sop.rds`. |
| `load_IC_raised_allocated_2024.R` | Sourced at the end of 1a and 1b. Loads the previous IC raising (`data/2024/df_stock_overview_raised_w_2024_IC.rds`) in long format for comparison. |
| `load_IC_age_allocated_2024.R` | Sourced at the end of 2a and 2b. Loads the previous IC age allocation (`data/{year}/IC_age_allocation/*.rds`) and binds it to the CEF results (`origin` = `IC` / `CEF`) for comparison. |
| `render_cef_report.R` | Renders `compare_cef_report.qmd` for `data_year` and names the html `compare_cef_report_{year}.html`. |
| `boot.R` | Loads packages, session options and the functions in `fun/` (`here::i_am("README.md")`). |
| `sol8ab_dl_scf.R` | Early, now **superseded** download of SCF data (`HCI`); not used by the workflow (it references an undefined `path_data_cef`). The CEF download is `sol8ab_dl_cef.R` (step 0). |
| `sol8ab_upload_scf.R` | Same SCF download as `sol8ab_dl_scf.R` (SCF, `HCI`, stock sol.27.8ab); not used by the workflow. |
| `fun/` | Older helpers: `read_stock_overview()`, `raise_discards.R`, `intercatch_add_strata.R`, `strata_lan_without_dis_summary.R`. |

## Folder layout

```
data/
  {year}/                       IC exports (StockOverview, NumbersAtAgeLength, MeanWeigthAtAgeLength) + IC_age_allocation/ (earlier IC results)
  rdbes_cef/{year}/             RDBES CEF download (HNI, HEN, HSN)
output/
  {year}/cef_ic/                1a results
  {year}/cef_rdbes/             1b + 2b results
  rdbes_cef_comparison/{year}/  step 0 comparison
  cef_raised_CEF-RDBES_vs_CEF-IC/{year}/   step 3
  cef_age_CEF-RDBES_vs_CEF-IC/{year}/      step 4
```

To run another year, change `data_year` in every script (and `years`/filters in step 0), and put the IC files in `data/{year}/`.
