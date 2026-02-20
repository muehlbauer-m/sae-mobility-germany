# ==============================================================================
# Phase 2: Hierarchical LASSO with sparseR (Ranked Sparsity)
# ==============================================================================
# Purpose: Variable selection with built-in hierarchy for interactions and polynomials
# Author: Michael Mühlbauer
# Date: 2026-01-21, Updated: 2026-02-04
#
# Uses sparseR package which implements "ranked sparsity" - a principled approach that:
# - Automatically penalizes higher-order terms (x², x³) more than main effects
# - Handles interactions with soft hierarchy preferences via differential penalties
# - Scales well to 1,200+ covariates × 1,600 observations
#
# NOTE: sparseR implements "soft" hierarchy via ranked penalties, NOT strict heredity.
# Higher-order terms get larger penalties (making them harder to select), but the
# selection of x² does NOT require x to be selected. This is by design.
#
# This script runs TWO configurations:
#   1. BASELINE: poly2, mode × covariate interactions only
#   2. MAXIMAL: poly3, mode × covariate + covariate × covariate interactions
#
# Pipeline structure:
#   Phase 1: Main Effect LASSO (phase1_lasso_base_covariates.r)
#            → ~79 base covariates from ~1,200 candidates
#   Phase 2: sparseR Hierarchical LASSO (THIS SCRIPT)
#            → Two configurations: baseline and maximal
#   Phase 3: Cross-Validation (phase3_cross_validation.r)
#            → Compare all candidates, select best model
#
# Input:
#   - data/output/models/phase1_prepared_data.rds (scaled data from Phase 1)
#   - data/output/variables/phase1_lasso_results.rds (selected variables)
#
# Output:
#   - data/output/variables/phase2_sparseR_results_baseline.rds
#   - data/output/variables/phase2_sparseR_results_maximal.rds
#
# ==============================================================================

rm(list = ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, caret, glmnet, sparseR)

# Check sparseR is installed
if (!requireNamespace("sparseR", quietly = TRUE)) {
  message("Installing sparseR package...")
  install.packages("sparseR")
  library(sparseR)
}

data_path <- "./data"

message("\n", strrep("=", 70))
message("  Phase 2: Hierarchical LASSO with sparseR (Ranked Sparsity)")
message(strrep("=", 70), "\n")

# ==============================================================================
# Configuration
# ==============================================================================

USE_PHASE1_LASSO_RESULTS <- TRUE
GAMMA <- 0.5            # Ranked sparsity weight (default)
CUMULATIVE_K <- TRUE    # Cumulative penalty by interaction order
CUMULATIVE_P <- TRUE    # Cumulative penalty by polynomial degree
PENALTY <- "lasso"      # Penalty type: "lasso", "MCP", or "SCAD"
RANDOM_SEED <- 42

# Define the two configurations to run
CONFIGURATIONS <- list(
  baseline = list(
    name = "baseline",
    poly_degree = 2,
    include_covar_covar = FALSE,
    description = "poly2, mode × covariate interactions only"
  ),
  maximal = list(
    name = "maximal",
    poly_degree = 3,
    include_covar_covar = TRUE,
    description = "poly3, mode × covariate + covariate × covariate interactions"
  )
)

message("Will run ", length(CONFIGURATIONS), " configurations:")
for (cfg in CONFIGURATIONS) {
  message("  - ", cfg$name, ": ", cfg$description)
}

# ==============================================================================
# Load data from Phase 1 LASSO results
# ==============================================================================

message("\n=== Loading Phase 1 LASSO results ===")

prepared_data_path <- file.path(data_path, "output/models/phase1_prepared_data.rds")
lasso_results_path <- file.path(data_path, "output/variables/phase1_lasso_results.rds")

if (!file.exists(prepared_data_path)) {
  stop("Phase 1 prepared data not found at: ", prepared_data_path,
       "\nPlease run phase1_lasso_base_covariates.r first")
}

if (!file.exists(lasso_results_path)) {
  stop("Phase 1 LASSO results not found at: ", lasso_results_path,
       "\nPlease run phase1_lasso_base_covariates.r first")
}

# Load prepared data (scaled/filtered covariate matrix)
prepared_data <- readRDS(prepared_data_path)
df_combined <- prepared_data$combined_data

# Load LASSO results (selected variables)
lasso_results <- readRDS(lasso_results_path)
selected_vars <- lasso_results$varlist  # Variables selected by LASSO

message("Loaded Phase 1 prepared data:")
message("  Observations: ", nrow(df_combined))
message("  Selected covariates (Phase 1 LASSO): ", length(selected_vars))

# Identify column types
id_cols <- c("ags5", "hvm_label", "ags5_hvm")
outcome_cols <- c("perskm_hth", "var_hth", "perskm_hth_log")
mode_contrast_cols <- c("mode_C1", "mode_C2", "mode_C3")

# Add hvm_factor if present (used for FH model)
if ("hvm_factor" %in% names(df_combined)) {
  id_cols <- c(id_cols, "hvm_factor")
}

# Covariate columns = selected vars minus mode contrasts (handled separately)
covar_cols <- setdiff(selected_vars, mode_contrast_cols)

message("  Base covariates (excl. mode contrasts): ", length(covar_cols))
message("  Mode contrasts: ", length(intersect(selected_vars, mode_contrast_cols)))

# Data is already scaled from Phase 1 LASSO pipeline
df_scaled <- df_combined

# Create log outcome if not present
if (!"perskm_hth_log" %in% names(df_scaled)) {
  message("  Creating perskm_hth_log = log(perskm_hth)")
  df_scaled <- df_scaled %>%
    mutate(perskm_hth_log = log(perskm_hth))
}

# ==============================================================================
# Prepare base data (shared across configurations)
# ==============================================================================

message("\n=== Preparing base data ===")

df_base <- df_scaled %>%
  select(perskm_hth_log, all_of(covar_cols), all_of(mode_contrast_cols)) %>%
  drop_na()

message("Base data: ", nrow(df_base), " observations")
message("  - Continuous covariates: ", length(covar_cols))
message("  - Mode contrasts: ", length(mode_contrast_cols))

# Extract outcome (shared)
y <- df_base$perskm_hth_log

# Extract matrices (shared)
X_main <- as.matrix(df_base %>% select(all_of(c(covar_cols, mode_contrast_cols))))
X_continuous <- as.matrix(df_base %>% select(all_of(covar_cols)))
X_mode <- as.matrix(df_base %>% select(all_of(mode_contrast_cols)))

# ==============================================================================
# Function to build model matrix for a given configuration
# ==============================================================================

build_model_matrix <- function(config, covar_cols, mode_contrast_cols,
                                X_main, X_continuous, X_mode) {

  message("\n--- Building model matrix for: ", config$name, " ---")
  message("  Polynomial degree: ", config$poly_degree)
  message("  Covariate × covariate interactions: ", config$include_covar_covar)

  # 1. Main effects (always included)
  model_matrix <- X_main
  main_effect_cols <- c(covar_cols, mode_contrast_cols)
  colnames(model_matrix) <- paste0(main_effect_cols, "_poly_1")
  message("  Main effects: ", ncol(model_matrix), " terms")

  # 2. Polynomials
  if (config$poly_degree >= 2) {
    X_poly2 <- X_continuous^2
    colnames(X_poly2) <- paste0(covar_cols, "_poly_2")
    model_matrix <- cbind(model_matrix, X_poly2)
    message("  Polynomial terms (degree 2): ", ncol(X_poly2), " terms")

    if (config$poly_degree >= 3) {
      X_poly3 <- X_continuous^3
      colnames(X_poly3) <- paste0(covar_cols, "_poly_3")
      model_matrix <- cbind(model_matrix, X_poly3)
      message("  Polynomial terms (degree 3): ", ncol(X_poly3), " terms")
    }
  }

  # 3. Mode × covariate interactions (always included)
  mode_covar_interactions <- list()
  for (m in mode_contrast_cols) {
    for (c in covar_cols) {
      int_name <- paste0(m, ":", c)
      mode_covar_interactions[[int_name]] <- X_mode[, m] * X_continuous[, c]
    }
  }
  X_mode_covar <- do.call(cbind, mode_covar_interactions)
  colnames(X_mode_covar) <- names(mode_covar_interactions)
  model_matrix <- cbind(model_matrix, X_mode_covar)
  message("  Mode × covariate interactions: ", ncol(X_mode_covar), " terms")

  # 4. Covariate × covariate interactions (only for maximal)
  if (config$include_covar_covar) {
    covar_covar_interactions <- list()
    for (i in 1:(length(covar_cols) - 1)) {
      for (j in (i + 1):length(covar_cols)) {
        int_name <- paste0(covar_cols[i], ":", covar_cols[j])
        covar_covar_interactions[[int_name]] <- X_continuous[, i] * X_continuous[, j]
      }
    }
    X_covar_covar <- do.call(cbind, covar_covar_interactions)
    colnames(X_covar_covar) <- names(covar_covar_interactions)
    model_matrix <- cbind(model_matrix, X_covar_covar)
    message("  Covariate × covariate interactions: ", ncol(X_covar_covar), " terms")
  }

  message("  Final model matrix: ", nrow(model_matrix), " obs × ", ncol(model_matrix), " terms")

  # Check for issues
  if (any(is.na(model_matrix))) {
    stop("NAs in model matrix!")
  }
  if (any(is.infinite(model_matrix))) {
    warning("Infinite values in model matrix - setting to NA")
    model_matrix[is.infinite(model_matrix)] <- NA
    complete_rows <- complete.cases(model_matrix)
    model_matrix <- model_matrix[complete_rows, ]
    message("  Removed ", sum(!complete_rows), " rows with infinite values")
  }

  return(model_matrix)
}

# ==============================================================================
# Function to fit sparseR and extract results
# ==============================================================================

fit_sparseR_config <- function(config, model_matrix, y, gamma, cumulative_k,
                                cumulative_p, penalty, seed) {

  message("\n", strrep("=", 70))
  message("  Fitting sparseR: ", config$name)
  message(strrep("=", 70))

  model_matrix_df <- as.data.frame(model_matrix)

  message("Start time: ", Sys.time())
  set.seed(seed)

  fit_sparse <- sparseR(
    formula = NULL,
    data = NULL,
    pre_process = FALSE,
    model_matrix = model_matrix_df,
    y = y,
    poly_prefix = "_poly_",
    int_sep = ":",
    gamma = gamma,
    cumulative_k = cumulative_k,
    cumulative_poly = cumulative_p,
    penalty = penalty
  )

  message("End time: ", Sys.time())

  # Extract selected variables
  summ_min <- summary(fit_sparse, at = "cvmin")
  coef_table_min <- summ_min$table
  selected_min <- rownames(coef_table_min)
  selected_min <- selected_min[selected_min != "(Intercept)"]

  summ_1se <- summary(fit_sparse, at = "cv1se")
  coef_table_1se <- summ_1se$table
  selected_1se <- rownames(coef_table_1se)
  selected_1se <- selected_1se[selected_1se != "(Intercept)"]

  message("Variables selected at lambda.min: ", length(selected_min))
  message("Variables selected at lambda.1se: ", length(selected_1se))

  # Categorize selected variables
  categorize_vars <- function(vars) {
    interactions <- vars[grepl(":", vars)]
    non_interactions <- vars[!grepl(":", vars)]
    main_effects <- non_interactions[grepl("_poly_1$", non_interactions)]
    polynomials <- non_interactions[grepl("_poly_[2-9]$", non_interactions)]
    other <- setdiff(non_interactions, c(main_effects, polynomials))

    # Further categorize interactions
    mode_interactions <- interactions[grepl("mode_C[123]:", interactions)]
    covar_interactions <- interactions[!grepl("mode_C[123]:", interactions)]

    list(
      main_effects = main_effects,
      polynomials = polynomials,
      mode_interactions = mode_interactions,
      covar_interactions = covar_interactions,
      interactions = interactions,
      other = other
    )
  }

  cats_min <- categorize_vars(selected_min)
  cats_1se <- categorize_vars(selected_1se)

  message("\nlambda.min breakdown:")
  message("  Main effects: ", length(cats_min$main_effects))
  message("  Polynomials: ", length(cats_min$polynomials))
  message("  Mode × covar interactions: ", length(cats_min$mode_interactions))
  message("  Covar × covar interactions: ", length(cats_min$covar_interactions))

  message("\nlambda.1se breakdown:")
  message("  Main effects: ", length(cats_1se$main_effects))
  message("  Polynomials: ", length(cats_1se$polynomials))
  message("  Mode × covar interactions: ", length(cats_1se$mode_interactions))
  message("  Covar × covar interactions: ", length(cats_1se$covar_interactions))

  list(
    model = fit_sparse,
    selected_lambda_min = selected_min,
    selected_lambda_1se = selected_1se,
    categories_min = cats_min,
    categories_1se = cats_1se,
    n_expanded_terms = ncol(model_matrix),
    model_matrix = model_matrix_df
  )
}

# ==============================================================================
# Run both configurations
# ==============================================================================

output_dir <- file.path(data_path, "output/variables")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

all_results <- list()

for (config_name in names(CONFIGURATIONS)) {
  config <- CONFIGURATIONS[[config_name]]

  tryCatch({
    # Build model matrix
    model_matrix <- build_model_matrix(
      config = config,
      covar_cols = covar_cols,
      mode_contrast_cols = mode_contrast_cols,
      X_main = X_main,
      X_continuous = X_continuous,
      X_mode = X_mode
    )

    # Fit sparseR
    fit_results <- fit_sparseR_config(
      config = config,
      model_matrix = model_matrix,
      y = y,
      gamma = GAMMA,
      cumulative_k = CUMULATIVE_K,
      cumulative_p = CUMULATIVE_P,
      penalty = PENALTY,
      seed = RANDOM_SEED
    )

    # Compile full results
    results <- list(
      config_name = config$name,
      config = config,
      model = fit_results$model,
      selected_lambda_min = fit_results$selected_lambda_min,
      selected_lambda_1se = fit_results$selected_lambda_1se,
      categories_min = fit_results$categories_min,
      categories_1se = fit_results$categories_1se,
      n_base_covariates = length(covar_cols) + length(mode_contrast_cols),
      n_expanded_terms = fit_results$n_expanded_terms,
      n_observations = nrow(model_matrix),
      sparseR_config = list(
        gamma = GAMMA,
        cumulative_k = CUMULATIVE_K,
        cumulative_p = CUMULATIVE_P,
        penalty = PENALTY
      ),
      model_matrix = fit_results$model_matrix,
      outcome = y,
      covar_cols = covar_cols,
      mode_contrast_cols = mode_contrast_cols,
      timestamp = Sys.time()
    )

    # Save results
    output_file <- paste0("phase2_sparseR_results_", config$name, ".rds")
    saveRDS(results, file.path(output_dir, output_file))
    message("\nSaved: ", output_file)

    all_results[[config_name]] <- results

  }, error = function(e) {
    message("\n!!! ERROR in sparseR fitting for ", config$name, " !!!")
    message("Error message: ", e$message)
    message("\nThis may be due to:")
    message("  1. Too many predictors for pairwise interactions")
    message("  2. Memory constraints")
    message("  3. sparseR package issues")
    stop(e)
  })
}

# ==============================================================================
# Summary
# ==============================================================================

message("\n", strrep("=", 70))
message("  Phase 2 Complete: Both Configurations Fitted")
message(strrep("=", 70), "\n")

message("Summary of results:\n")

for (config_name in names(all_results)) {
  res <- all_results[[config_name]]
  message("--- ", toupper(config_name), " (", res$config$description, ") ---")
  message("  Expanded terms: ", res$n_expanded_terms)
  message("  lambda.min: ", length(res$selected_lambda_min), " variables")
  message("    - Main effects: ", length(res$categories_min$main_effects))
  message("    - Polynomials: ", length(res$categories_min$polynomials))
  message("    - Mode × covar: ", length(res$categories_min$mode_interactions))
  message("    - Covar × covar: ", length(res$categories_min$covar_interactions))
  message("  lambda.1se: ", length(res$selected_lambda_1se), " variables")
  message("    - Main effects: ", length(res$categories_1se$main_effects))
  message("    - Polynomials: ", length(res$categories_1se$polynomials))
  message("    - Mode × covar: ", length(res$categories_1se$mode_interactions))
  message("    - Covar × covar: ", length(res$categories_1se$covar_interactions))
  message("")
}

message("Outputs saved to: ", output_dir)
message("  - phase2_sparseR_results_baseline.rds")
message("  - phase2_sparseR_results_maximal.rds")

message("\n=== Done! ===")
