# ==============================================================================
# Main Analysis Pipeline: Multimodal Small Area Estimation
# ==============================================================================
# This script runs the complete analysis pipeline for estimating person-km/day
# by transport mode at the district level using Fay-Herriot small area estimation.
#
# Total runtime: ~3-5 hours (mostly variable selection and bootstrap)
# ==============================================================================

# Setup ------------------------------------------------------------------------
rm(list = ls())
options(warn = 1)  # Print warnings as they occur

# Set working directory to project root
if (!dir.exists("R") || !file.exists("run_analysis.r")) {
  stop("Please set working directory to the project root (containing R/ directory)")
}

message("\n", strrep("=", 80))
message("MULTIMODAL SMALL AREA ESTIMATION PIPELINE")
message(strrep("=", 80))
message("Start time: ", Sys.time())
message("")

# ==============================================================================
# STEP 1: Data Cleaning
# ==============================================================================
# Imports MiD 2017 survey data, filters invalid records, creates person × mode
# dataset, and merges MiD-Gemeindetyp stratification variable.

message("\n>>> STEP 1: Data Cleaning")
source("R/data_preparation/data_cleaning.r")

# ==============================================================================
# STEP 2: Census Data Acquisition
# ==============================================================================
# Downloads census tables from Destatis GENESIS/REGIO API and merges them into
# a single district-level dataset with population, demographics, and economic indicators.

message("\n>>> STEP 2: Census Data Acquisition")
source("R/data_preparation/census_data_acquisition.r")

# ==============================================================================
# STEP 3: INKAR Data Acquisition
# ==============================================================================
# Loads INKAR (Indikatoren und Karten zur Raum- und Stadtentwicklung) regional
# indicators providing 540 socioeconomic variables at the district level.

message("\n>>> STEP 3: INKAR Data Acquisition")
source("R/data_preparation/inkar_data_acquisition.r")

# ==============================================================================
# STEP 4: MiD Covariate Aggregation
# ==============================================================================
# Aggregates trip-level and person-level MiD variables to district × mode domains
# using survey-weighted means and proportions.

message("\n>>> STEP 4: MiD Covariate Aggregation")
source("R/data_preparation/aggregate_mid_covariates.r")

# ==============================================================================
# STEP 5: Direct Estimation
# ==============================================================================
# Computes Horvitz-Thompson-Hajek direct estimates with analytical variance
# using Taylor linearization via the survey package. Runtime: ~3-5 minutes.

message("\n>>> STEP 5: Direct Estimation")
source("R/direct_estimation/direct_estimation.r")

# ==============================================================================
# STEP 6: Phase 1 - Base Variable Selection (LASSO)
# ==============================================================================
# Performs LASSO regression with cross-validation to select optimal covariates
# from ~1,200 candidates (MiD + census + INKAR) → ~79 base covariates.
# Runtime: ~1-2 hours with parallel processing.

message("\n>>> STEP 6: Phase 1 - Base Variable Selection (LASSO)")
source("R/modeling/phase1_lasso_base_covariates.r")

# ==============================================================================
# STEP 7: Phase 2 - Hierarchical LASSO with sparseR
# ==============================================================================
# Applies sparseR ranked sparsity LASSO to generate interactions and polynomials
# with differential penalties. Uses Phase 1 base covariates as input.
# Output: lambda.min (~150 vars) and lambda.1se (~61 vars) candidate sets.

message("\n>>> STEP 7: Phase 2 - Hierarchical LASSO (sparseR)")
source("R/modeling/phase2_sparseR_hierarchical.r")

# ==============================================================================
# STEP 8: Phase 3 - Cross-Validation & Final Model Selection
# ==============================================================================
# Compares candidate variable sets (base, lambda.min, lambda.1se) via k-fold
# cross-validation following Ren et al. (2022). Selects optimal variable set
# based on SSE on reliable domains (CV < 20%). Fits final winning model.

message("\n>>> STEP 8: Phase 3 - Cross-Validation & Final Model Selection")
source("R/modeling/phase3_cross_validation.r")

# ==============================================================================
# STEP 9: (OPTIONAL) Fit All Candidate FH Models for Diagnostics
# ==============================================================================
# Fits FH models for ALL candidates for comparison and diagnostic plots:
#   - Base model (Phase 1 LASSO variables, ~82 vars)
#   - Lambda.min model (Phase 2 sparseR, ~150 vars)
#   - Lambda.1se model (Phase 2 sparseR, ~61 vars)
# This step is optional - Phase 3 already fits the winning model.

message("\n>>> STEP 9: (OPTIONAL) Fit All Candidate FH Models for Diagnostics")
source("R/modeling/fit_fh_models.r")

# ==============================================================================
# STEP 10: Plotting and Tables (Optional)
# ==============================================================================
# Generates all figures and tables for the paper.

message("\n>>> STEP 10: Plotting and Tables")
source("R/plotting/plot_direct.r")
source("R/plotting/plot_fh.r")
source("R/plotting/plot_comparison.r")
source("R/plotting/plot_diagnostics.r")

# Generate appendix tables
source("R/plotting/generate_final_varset_table.r")
source("R/plotting/generate_census_translations.r")

# ==============================================================================
# Summary
# ==============================================================================

message("\n", strrep("=", 80))
message("PIPELINE COMPLETE")
message(strrep("=", 80))
message("End time: ", Sys.time())
message("")
message("Key outputs:")
message("  - Direct estimates:     data/output/direct/df_HTH_perskm_analytical.rds")
message("  - Phase 1 LASSO:        data/output/variables/phase1_lasso_results.rds")
message("  - Phase 1 prepared:     data/output/models/phase1_prepared_data.rds")
message("  - Phase 2 sparseR:      data/output/variables/phase2_sparseR_results.rds")
message("  - Phase 3 CV results:   data/output/models/phase3_cv_results.rds")
message("  - Phase 3 final model:  data/output/models/fh_model_final.rds")
message("  - Phase 3 model data:   data/output/models/phase3_model_data_final.rds")
message("  - (Optional) All models:data/output/models/fh_model_*.rds")
message("  - Figures:              data/Tex/Images/")
message("  - Tables:               data/Tex/Tables/")
message("    - tbl_final_varset.tex (final variable set)")
message("    - tbl_census_translations.csv (Destatis translations)")
