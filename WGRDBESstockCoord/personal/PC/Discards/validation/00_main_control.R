
################################################################################
## PROJECT: Discard Raising Validation (Synthetic Data)
## PURPOSE: Main Control Script to run the full workflow
## AUTHOR:Pepe Cebrian
## DATE: 2026-03-27
################################################################################

# 1. ENSURE LIBRARIES ARE INSTALLED & LOADED
required_packages <- c("tidyverse", "rlang", "ggplot2", "scales")

new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

lapply(required_packages, library, character.only = TRUE)

# 2. SET WORKING DIRECTORY (Automatically detects current folder)
# This prevents path errors on Sofie's or Yves' computers
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

message(">>> Starting Validation Workflow...")

# 3. RUN MODULES IN LOGICAL ORDER
# Each script saves its own outputs (CSV/Plots)
tryCatch({
  message("Step 1: Generating Synthetic Dataset...")
  source("01_data_generation.R")
  
  message("Step 2: Analysis coverage and Status...")
  source("02_process_discards.R")
  
  message("Step 3: Raising needed")
  source("03_build_gap_table.R")
  
  message("Step 4: Creating heatmap...")
  source("04_plot_discards_heatmap.R")
  
  message("Step 5: Creating bubble plot...")
  source("05_plot_discards_bubbles.R")
  
  message(">>> WORKFLOW COMPLETED SUCCESSFULLY.")
  message("Check the 'outputs/' folder for CSVs and Plots.")
  
}, error = function(e) {
  message("!!! ERROR DURING WORKFLOW: ", e$message)
})

