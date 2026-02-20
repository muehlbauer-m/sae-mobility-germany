# ==============================================================================
# Fit All Candidate FH Models (Optional Diagnostics Script)
# ==============================================================================
# Purpose: Fit FH models for ALL candidates for comparison and diagnostics
#          - Base model (Phase 1 LASSO variables)
#          - Baseline Lambda.min model (poly2, mode × covar only)
#          - Baseline Lambda.1se model (poly2, mode × covar only)
#          - Maximal Lambda.min model (poly3, mode × covar + covar × covar)
#          - Maximal Lambda.1se model (poly3, mode × covar + covar × covar)
#          - Base model non-log (for transformation comparison)
#
# Author: Michael Mühlbauer
# Date: 2026-01-21
# Updated: 2026-02-04 - Added maximal model support (5 candidates total)
#
# This script is OPTIONAL - it runs AFTER Phase 3 CV to fit all candidate
# models for diagnostic comparison. Phase 3 already fits the winning model.
#
# Use this script when you need:
#   - Side-by-side comparison of all 5 models
#   - Diagnostic plots comparing baseline vs maximal models
#   - Log vs non-log transformation comparison
#   - Model tables for the paper
#
# Pipeline structure:
#   Phase 1: Main Effect LASSO → phase1_lasso_results.rds + phase1_prepared_data.rds
#   Phase 2: sparseR Hierarchical LASSO (2 configs) →
#            phase2_sparseR_results_baseline.rds, phase2_sparseR_results_maximal.rds
#   Phase 3: Cross-Validation → fh_model_final.rds + phase3_model_data_final.rds
#   [OPTIONAL] This script → fh_model_*.rds for all candidates
#
# Input:
#   - data/output/models/phase3_model_data_final.rds (contains prepared data for all models)
#   - data/output/variables/phase1_lasso_results_nolog.rds (non-log LASSO selected variables)
#
# Output:
#   - data/output/models/fh_model_base.rds
#   - data/output/models/fh_model_baseline_min.rds
#   - data/output/models/fh_model_baseline_1se.rds
#   - data/output/models/fh_model_maximal_min.rds
#   - data/output/models/fh_model_maximal_1se.rds
#   - data/output/models/fh_model_base_nolog.rds (optional, for transformation comparison)
#   - data/output/models/model_comparison_summary.rds (comparison table)
#
# ==============================================================================

rm(list = ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyr, emdi)

data_path <- "./data"

message("\n", strrep("=", 70))
message("  Fit All Candidate FH Models (Diagnostics)")
message(strrep("=", 70), "\n")

# ==============================================================================
# Configuration
# ==============================================================================

FIT_BASE_MODEL <- TRUE       # Fit base model (Phase 1 LASSO variables)
FIT_BASELINE_MIN <- TRUE     # Fit baseline lambda.min (poly2, mode × covar)
FIT_BASELINE_1SE <- TRUE     # Fit baseline lambda.1se (poly2, mode × covar)
FIT_MAXIMAL_MIN <- TRUE      # Fit maximal lambda.min (poly3, + covar × covar)
FIT_MAXIMAL_1SE <- TRUE      # Fit maximal lambda.1se (poly3, + covar × covar)
FIT_BASE_NOLOG <- TRUE       # Fit non-log base model (for transformation comparison)
COMPUTE_MSE <- TRUE          # Compute MSE via parametric bootstrap (slow)

message("Configuration:")
message("  Fit base model (log): ", FIT_BASE_MODEL)
message("  Fit baseline lambda.min: ", FIT_BASELINE_MIN)
message("  Fit baseline lambda.1se: ", FIT_BASELINE_1SE)
message("  Fit maximal lambda.min: ", FIT_MAXIMAL_MIN)
message("  Fit maximal lambda.1se: ", FIT_MAXIMAL_1SE)
message("  Fit base model (non-log): ", FIT_BASE_NOLOG)
message("  Compute MSE: ", COMPUTE_MSE)

# ==============================================================================
# Load Phase 3 model data (contains prepared data for all candidates)
# ==============================================================================

message("\n=== Loading Phase 3 model data ===")

phase3_data_path <- file.path(data_path, "output/models/phase3_model_data_final.rds")
if (!file.exists(phase3_data_path)) {
  stop("Phase 3 model data not found at: ", phase3_data_path,
       "\nPlease run phase3_cross_validation.r first")
}

phase3_data <- readRDS(phase3_data_path)

message("Loaded Phase 3 model data:")
message("  - CV winner: ", phase3_data$winner)
message("  - Final model variables: ", length(phase3_data$selected_vars))

# Extract prepared data for all candidates
df_fh_base <- phase3_data$base_data$combined_data
vars_base <- phase3_data$base_data$selected_vars

# Baseline data (poly2, mode × covar only)
df_fh_baseline <- phase3_data$baseline_data$combined_data
vars_baseline_min <- phase3_data$baseline_data$vars_min
vars_baseline_1se <- phase3_data$baseline_data$vars_1se

# Maximal data (poly3, + covar × covar)
df_fh_maximal <- phase3_data$maximal_data$combined_data
vars_maximal_min <- phase3_data$maximal_data$vars_min
vars_maximal_1se <- phase3_data$maximal_data$vars_1se

message("\nCandidate models:")
message("  - Base: ", length(vars_base), " variables, ", nrow(df_fh_base), " domains")
message("  - Baseline lambda.min: ", length(vars_baseline_min), " variables, ", nrow(df_fh_baseline), " domains")
message("  - Baseline lambda.1se: ", length(vars_baseline_1se), " variables, ", nrow(df_fh_baseline), " domains")
message("  - Maximal lambda.min: ", length(vars_maximal_min), " variables, ", nrow(df_fh_maximal), " domains")
message("  - Maximal lambda.1se: ", length(vars_maximal_1se), " variables, ", nrow(df_fh_maximal), " domains")

# ==============================================================================
# Load Non-Log LASSO Results (for log vs non-log comparison)
# ==============================================================================

vars_base_nolog <- NULL

if (FIT_BASE_NOLOG) {
  message("\n=== Loading Non-Log LASSO results ===")

  nolog_path <- file.path(data_path, "output/variables/phase1_lasso_results_nolog.rds")
  if (!file.exists(nolog_path)) {
    warning("Non-log LASSO results not found at: ", nolog_path,
            "\nSkipping non-log model fitting. Run phase1_lasso_base_covariates.r first.")
    FIT_BASE_NOLOG <- FALSE
  } else {
    nolog_results <- readRDS(nolog_path)
    vars_base_nolog <- nolog_results$selected_vars_min

    # Add mode contrasts if not already present
    mode_contrasts <- c("mode_C1", "mode_C2", "mode_C3")
    vars_base_nolog <- unique(c(vars_base_nolog, mode_contrasts))

    message("  Non-log LASSO selected: ", length(nolog_results$selected_vars_min), " variables")
    message("  With mode contrasts: ", length(vars_base_nolog), " variables")
  }
}

# ==============================================================================
# Build Model Formulas
# ==============================================================================

message("\n=== Building model formulas ===")

formula_base <- as.formula(paste("perskm_hth ~", paste(vars_base, collapse = " + ")))
formula_baseline_min <- as.formula(paste("perskm_hth ~", paste(vars_baseline_min, collapse = " + ")))
formula_baseline_1se <- as.formula(paste("perskm_hth ~", paste(vars_baseline_1se, collapse = " + ")))
formula_maximal_min <- as.formula(paste("perskm_hth ~", paste(vars_maximal_min, collapse = " + ")))
formula_maximal_1se <- as.formula(paste("perskm_hth ~", paste(vars_maximal_1se, collapse = " + ")))

message("  Base model: ", length(vars_base), " predictors")
message("  Baseline lambda.min: ", length(vars_baseline_min), " predictors")
message("  Baseline lambda.1se: ", length(vars_baseline_1se), " predictors")
message("  Maximal lambda.min: ", length(vars_maximal_min), " predictors")
message("  Maximal lambda.1se: ", length(vars_maximal_1se), " predictors")

if (FIT_BASE_NOLOG && !is.null(vars_base_nolog)) {
  formula_base_nolog <- as.formula(paste("perskm_hth ~", paste(vars_base_nolog, collapse = " + ")))
  message("  Base model (non-log): ", length(vars_base_nolog), " predictors")
}

# ==============================================================================
# Helper Function: Calculate Diagnostics
# ==============================================================================

calc_diagnostics <- function(model, name) {
  std_re <- model$model$random_effects
  n <- length(std_re)
  m <- mean(std_re)
  s <- sd(std_re)
  skew <- sum((std_re - m)^3) / (n * s^3)
  kurt <- sum((std_re - m)^4) / (n * s^4)
  shapiro <- shapiro.test(std_re)

  n_vars <- nrow(model$model$coefficients) - 1

  message(name, ":")
  message("  - Variables: ", n_vars)
  message("  - sigma_u (district variance): ", round(model$model$variance, 4))
  message("  - Kurtosis (random effects): ", round(kurt, 2), " (target: 3.0)")
  message("  - Skewness (random effects): ", round(skew, 2), " (target: 0.0)")
  message("  - Shapiro-Wilk p-value: ", format(shapiro$p.value, scientific = TRUE, digits = 3))

  return(list(
    variance = model$model$variance,
    kurtosis = kurt,
    skewness = skew,
    shapiro_p = shapiro$p.value,
    n_vars = n_vars
  ))
}

# ==============================================================================
# Fit FH Models
# ==============================================================================

message("\n", strrep("=", 70))
message("  Fitting Fay-Herriot Models")
message(strrep("=", 70), "\n")

message("Method: Maximum Likelihood (ML)")
message("Transformation: log with bc_sm back-transformation")
if (COMPUTE_MSE) {
  message("MSE estimation: Parametric bootstrap")
} else {
  message("MSE estimation: Disabled")
}

# Initialize result lists
fh_models <- list()
diagnostics <- list()

# --- Fit Base Model (Log-Transformed) ---
if (FIT_BASE_MODEL) {
  message("\n--- Fitting Base Model - Log-Transformed (", length(vars_base), " variables) ---")
  start_time <- Sys.time()

  fh_model_base <- emdi::fh(
    fixed = formula_base,
    vardir = "var_hth",
    combined_data = df_fh_base,
    domains = "ags5_hvm",
    method = "ml",
    transformation = "log",
    backtransformation = "bc_sm",
    MSE = COMPUTE_MSE
  )

  elapsed <- round(difftime(Sys.time(), start_time, units = "mins"), 2)
  message("Completed in ", elapsed, " minutes")

  fh_models$base <- fh_model_base
  diagnostics$base <- calc_diagnostics(fh_model_base, "Base Model (Log)")
}

# --- Fit Base Model (Non-Log) for Transformation Comparison ---
if (FIT_BASE_NOLOG && !is.null(vars_base_nolog)) {
  message("\n--- Fitting Base Model - Non-Log (", length(vars_base_nolog), " variables) ---")
  message("Note: This model is for log vs non-log transformation comparison only")
  start_time <- Sys.time()

  fh_model_base_nolog <- emdi::fh(
    fixed = formula_base_nolog,
    vardir = "var_hth",
    combined_data = df_fh_base,
    domains = "ags5_hvm",
    method = "ml",
    transformation = "no",       # Key difference: no transformation
    MSE = COMPUTE_MSE
  )

  elapsed <- round(difftime(Sys.time(), start_time, units = "mins"), 2)
  message("Completed in ", elapsed, " minutes")

  fh_models$base_nolog <- fh_model_base_nolog
  diagnostics$base_nolog <- calc_diagnostics(fh_model_base_nolog, "Base Model (Non-Log)")
}

# --- Fit Baseline Lambda.min Model ---
if (FIT_BASELINE_MIN) {
  message("\n--- Fitting Baseline Lambda.min Model (", length(vars_baseline_min), " variables) ---")
  message("Note: poly2, mode × covariate interactions only")
  start_time <- Sys.time()

  fh_model_baseline_min <- emdi::fh(
    fixed = formula_baseline_min,
    vardir = "var_hth",
    combined_data = df_fh_baseline,
    domains = "ags5_hvm",
    method = "ml",
    transformation = "log",
    backtransformation = "bc_sm",
    MSE = COMPUTE_MSE
  )

  elapsed <- round(difftime(Sys.time(), start_time, units = "mins"), 2)
  message("Completed in ", elapsed, " minutes")

  fh_models$baseline_min <- fh_model_baseline_min
  diagnostics$baseline_min <- calc_diagnostics(fh_model_baseline_min, "Baseline Lambda.min Model")
}

# --- Fit Baseline Lambda.1se Model ---
if (FIT_BASELINE_1SE) {
  message("\n--- Fitting Baseline Lambda.1se Model (", length(vars_baseline_1se), " variables) ---")
  message("Note: poly2, mode × covariate interactions only (parsimonious)")
  start_time <- Sys.time()

  fh_model_baseline_1se <- emdi::fh(
    fixed = formula_baseline_1se,
    vardir = "var_hth",
    combined_data = df_fh_baseline,
    domains = "ags5_hvm",
    method = "ml",
    transformation = "log",
    backtransformation = "bc_sm",
    MSE = COMPUTE_MSE
  )

  elapsed <- round(difftime(Sys.time(), start_time, units = "mins"), 2)
  message("Completed in ", elapsed, " minutes")

  fh_models$baseline_1se <- fh_model_baseline_1se
  diagnostics$baseline_1se <- calc_diagnostics(fh_model_baseline_1se, "Baseline Lambda.1se Model")
}

# --- Fit Maximal Lambda.min Model ---
if (FIT_MAXIMAL_MIN) {
  message("\n--- Fitting Maximal Lambda.min Model (", length(vars_maximal_min), " variables) ---")
  message("Note: poly3, mode × covariate + covariate × covariate interactions")
  start_time <- Sys.time()

  fh_model_maximal_min <- emdi::fh(
    fixed = formula_maximal_min,
    vardir = "var_hth",
    combined_data = df_fh_maximal,
    domains = "ags5_hvm",
    method = "ml",
    transformation = "log",
    backtransformation = "bc_sm",
    MSE = COMPUTE_MSE
  )

  elapsed <- round(difftime(Sys.time(), start_time, units = "mins"), 2)
  message("Completed in ", elapsed, " minutes")

  fh_models$maximal_min <- fh_model_maximal_min
  diagnostics$maximal_min <- calc_diagnostics(fh_model_maximal_min, "Maximal Lambda.min Model")
}

# --- Fit Maximal Lambda.1se Model ---
if (FIT_MAXIMAL_1SE) {
  message("\n--- Fitting Maximal Lambda.1se Model (", length(vars_maximal_1se), " variables) ---")
  message("Note: poly3, mode × covariate + covariate × covariate interactions (parsimonious)")
  start_time <- Sys.time()

  fh_model_maximal_1se <- emdi::fh(
    fixed = formula_maximal_1se,
    vardir = "var_hth",
    combined_data = df_fh_maximal,
    domains = "ags5_hvm",
    method = "ml",
    transformation = "log",
    backtransformation = "bc_sm",
    MSE = COMPUTE_MSE
  )

  elapsed <- round(difftime(Sys.time(), start_time, units = "mins"), 2)
  message("Completed in ", elapsed, " minutes")

  fh_models$maximal_1se <- fh_model_maximal_1se
  diagnostics$maximal_1se <- calc_diagnostics(fh_model_maximal_1se, "Maximal Lambda.1se Model")
}

# ==============================================================================
# Model Comparison Summary
# ==============================================================================

message("\n", strrep("=", 70))
message("  Model Comparison Summary")
message(strrep("=", 70), "\n")

comparison_df <- data.frame(
  Model = c("Base", "Baseline_min", "Baseline_1se", "Maximal_min", "Maximal_1se"),
  Config = c("Phase 1 LASSO", "poly2, mode×covar", "poly2, mode×covar",
             "poly3, +covar×covar", "poly3, +covar×covar"),
  Variables = c(
    if (FIT_BASE_MODEL) diagnostics$base$n_vars else NA,
    if (FIT_BASELINE_MIN) diagnostics$baseline_min$n_vars else NA,
    if (FIT_BASELINE_1SE) diagnostics$baseline_1se$n_vars else NA,
    if (FIT_MAXIMAL_MIN) diagnostics$maximal_min$n_vars else NA,
    if (FIT_MAXIMAL_1SE) diagnostics$maximal_1se$n_vars else NA
  ),
  Sigma_u = c(
    if (FIT_BASE_MODEL) round(diagnostics$base$variance, 4) else NA,
    if (FIT_BASELINE_MIN) round(diagnostics$baseline_min$variance, 4) else NA,
    if (FIT_BASELINE_1SE) round(diagnostics$baseline_1se$variance, 4) else NA,
    if (FIT_MAXIMAL_MIN) round(diagnostics$maximal_min$variance, 4) else NA,
    if (FIT_MAXIMAL_1SE) round(diagnostics$maximal_1se$variance, 4) else NA
  ),
  Kurtosis = c(
    if (FIT_BASE_MODEL) round(diagnostics$base$kurtosis, 2) else NA,
    if (FIT_BASELINE_MIN) round(diagnostics$baseline_min$kurtosis, 2) else NA,
    if (FIT_BASELINE_1SE) round(diagnostics$baseline_1se$kurtosis, 2) else NA,
    if (FIT_MAXIMAL_MIN) round(diagnostics$maximal_min$kurtosis, 2) else NA,
    if (FIT_MAXIMAL_1SE) round(diagnostics$maximal_1se$kurtosis, 2) else NA
  ),
  Skewness = c(
    if (FIT_BASE_MODEL) round(diagnostics$base$skewness, 2) else NA,
    if (FIT_BASELINE_MIN) round(diagnostics$baseline_min$skewness, 2) else NA,
    if (FIT_BASELINE_1SE) round(diagnostics$baseline_1se$skewness, 2) else NA,
    if (FIT_MAXIMAL_MIN) round(diagnostics$maximal_min$skewness, 2) else NA,
    if (FIT_MAXIMAL_1SE) round(diagnostics$maximal_1se$skewness, 2) else NA
  )
)

# Add CV winner indicator
cv_winner <- phase3_data$winner
comparison_df$CV_Winner <- c(
  cv_winner == "base",
  cv_winner == "baseline_min",
  cv_winner == "baseline_1se",
  cv_winner == "maximal_min",
  cv_winner == "maximal_1se"
)

print(comparison_df)

message("\nNote: Lower kurtosis is better (target: 3.0)")
message("Note: Lower sigma_u indicates better explanatory power")
message("Note: CV_Winner indicates the model selected by cross-validation")

# ==============================================================================
# Save Results
# ==============================================================================

message("\n=== Saving results ===")

output_dir <- file.path(data_path, "output/models")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Save individual models
if (FIT_BASE_MODEL) {
  saveRDS(fh_models$base, file.path(output_dir, "fh_model_base.rds"))
  message("Saved: fh_model_base.rds (log-transformed)")
}

if (FIT_BASE_NOLOG && !is.null(fh_models$base_nolog)) {
  saveRDS(fh_models$base_nolog, file.path(output_dir, "fh_model_base_nolog.rds"))
  message("Saved: fh_model_base_nolog.rds (non-log, for transformation comparison)")
}

if (FIT_BASELINE_MIN) {
  saveRDS(fh_models$baseline_min, file.path(output_dir, "fh_model_baseline_min.rds"))
  message("Saved: fh_model_baseline_min.rds")
}

if (FIT_BASELINE_1SE) {
  saveRDS(fh_models$baseline_1se, file.path(output_dir, "fh_model_baseline_1se.rds"))
  message("Saved: fh_model_baseline_1se.rds")
}

if (FIT_MAXIMAL_MIN) {
  saveRDS(fh_models$maximal_min, file.path(output_dir, "fh_model_maximal_min.rds"))
  message("Saved: fh_model_maximal_min.rds")
}

if (FIT_MAXIMAL_1SE) {
  saveRDS(fh_models$maximal_1se, file.path(output_dir, "fh_model_maximal_1se.rds"))
  message("Saved: fh_model_maximal_1se.rds")
}

# Save model data for downstream scripts (backwards compatibility)
model_data_base <- list(
  model = fh_models$base,
  model_nolog = if (FIT_BASE_NOLOG) fh_models$base_nolog else NULL,
  combined_data = df_fh_base,
  formula = formula_base,
  formula_nolog = if (FIT_BASE_NOLOG && exists("formula_base_nolog")) formula_base_nolog else NULL,
  selected_vars = vars_base,
  selected_vars_nolog = if (FIT_BASE_NOLOG) vars_base_nolog else NULL,
  diagnostics = diagnostics$base,
  diagnostics_nolog = if (FIT_BASE_NOLOG) diagnostics$base_nolog else NULL,
  timestamp = Sys.time()
)
saveRDS(model_data_base, file.path(output_dir, "model_data_base.rds"))
message("Saved: model_data_base.rds (includes non-log model if fitted)")

# Save baseline sparseR data
model_data_baseline <- list(
  model_min = fh_models$baseline_min,
  model_1se = fh_models$baseline_1se,
  combined_data = df_fh_baseline,
  formula_min = formula_baseline_min,
  formula_1se = formula_baseline_1se,
  vars_min = vars_baseline_min,
  vars_1se = vars_baseline_1se,
  vars_min_raw = phase3_data$baseline_data$vars_min_raw,
  vars_1se_raw = phase3_data$baseline_data$vars_1se_raw,
  diagnostics_min = diagnostics$baseline_min,
  diagnostics_1se = diagnostics$baseline_1se,
  config = "poly2, mode × covariate interactions only",
  timestamp = Sys.time()
)
saveRDS(model_data_baseline, file.path(output_dir, "model_data_baseline.rds"))
message("Saved: model_data_baseline.rds")

# Save maximal sparseR data
model_data_maximal <- list(
  model_min = fh_models$maximal_min,
  model_1se = fh_models$maximal_1se,
  combined_data = df_fh_maximal,
  formula_min = formula_maximal_min,
  formula_1se = formula_maximal_1se,
  vars_min = vars_maximal_min,
  vars_1se = vars_maximal_1se,
  vars_min_raw = phase3_data$maximal_data$vars_min_raw,
  vars_1se_raw = phase3_data$maximal_data$vars_1se_raw,
  diagnostics_min = diagnostics$maximal_min,
  diagnostics_1se = diagnostics$maximal_1se,
  config = "poly3, mode × covariate + covariate × covariate interactions",
  timestamp = Sys.time()
)
saveRDS(model_data_maximal, file.path(output_dir, "model_data_maximal.rds"))
message("Saved: model_data_maximal.rds")

# Also save backwards-compatible model_data_sparseR.rds pointing to baseline (CV winner config)
# This maintains compatibility with existing plotting scripts
model_data_sparseR <- model_data_baseline
saveRDS(model_data_sparseR, file.path(output_dir, "model_data_sparseR.rds"))
message("Saved: model_data_sparseR.rds (symlink to baseline, for backwards compatibility)")

# Save comparison summary
comparison_summary <- list(
  comparison_df = comparison_df,
  diagnostics = diagnostics,
  formulas = list(
    base = formula_base,
    baseline_min = formula_baseline_min,
    baseline_1se = formula_baseline_1se,
    maximal_min = formula_maximal_min,
    maximal_1se = formula_maximal_1se
  ),
  cv_winner = cv_winner,
  timestamp = Sys.time()
)
saveRDS(comparison_summary, file.path(output_dir, "model_comparison_summary.rds"))
message("Saved: model_comparison_summary.rds")

# ==============================================================================
# Summary
# ==============================================================================

message("\n", strrep("=", 70))
message("  Model Fitting Complete!")
message(strrep("=", 70), "\n")

message("Summary:")
if (FIT_BASE_MODEL) {
  message("  Base (log):       ", diagnostics$base$n_vars, " vars, kurtosis = ",
          round(diagnostics$base$kurtosis, 2), ", sigma_u = ",
          round(diagnostics$base$variance, 4))
}
if (FIT_BASE_NOLOG && !is.null(diagnostics$base_nolog)) {
  message("  Base (non-log):   ", diagnostics$base_nolog$n_vars, " vars, kurtosis = ",
          round(diagnostics$base_nolog$kurtosis, 2), ", sigma_u = ",
          round(diagnostics$base_nolog$variance, 4))
}
if (FIT_BASELINE_MIN) {
  message("  Baseline min:     ", diagnostics$baseline_min$n_vars, " vars, kurtosis = ",
          round(diagnostics$baseline_min$kurtosis, 2), ", sigma_u = ",
          round(diagnostics$baseline_min$variance, 4))
}
if (FIT_BASELINE_1SE) {
  message("  Baseline 1se:     ", diagnostics$baseline_1se$n_vars, " vars, kurtosis = ",
          round(diagnostics$baseline_1se$kurtosis, 2), ", sigma_u = ",
          round(diagnostics$baseline_1se$variance, 4))
}
if (FIT_MAXIMAL_MIN) {
  message("  Maximal min:      ", diagnostics$maximal_min$n_vars, " vars, kurtosis = ",
          round(diagnostics$maximal_min$kurtosis, 2), ", sigma_u = ",
          round(diagnostics$maximal_min$variance, 4))
}
if (FIT_MAXIMAL_1SE) {
  message("  Maximal 1se:      ", diagnostics$maximal_1se$n_vars, " vars, kurtosis = ",
          round(diagnostics$maximal_1se$kurtosis, 2), ", sigma_u = ",
          round(diagnostics$maximal_1se$variance, 4))
}

message("\nCV Winner: ", cv_winner)

# Log vs Non-Log comparison note
if (FIT_BASE_NOLOG && !is.null(diagnostics$base_nolog)) {
  message("\n--- Log vs Non-Log Transformation Comparison ---")
  message("  Log model kurtosis:     ", round(diagnostics$base$kurtosis, 2))
  message("  Non-log model kurtosis: ", round(diagnostics$base_nolog$kurtosis, 2))
  if (diagnostics$base$kurtosis < diagnostics$base_nolog$kurtosis) {
    message("  -> Log transformation IMPROVES normality (lower kurtosis)")
  } else {
    message("  -> Non-log model has better kurtosis (unusual)")
  }
}

message("\nOutputs:")
message("  - fh_model_base.rds, fh_model_base_nolog.rds")
message("  - fh_model_baseline_min.rds, fh_model_baseline_1se.rds")
message("  - fh_model_maximal_min.rds, fh_model_maximal_1se.rds")
message("  - model_data_base.rds, model_data_baseline.rds, model_data_maximal.rds")
message("  - model_data_sparseR.rds (backwards compat, points to baseline)")
message("  - model_comparison_summary.rds (comparison table)")
message("\nNext: Run plotting scripts for diagnostic figures")
