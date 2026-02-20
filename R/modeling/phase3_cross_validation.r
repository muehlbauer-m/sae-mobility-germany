# ==============================================================================
# Phase 3: Cross-Validation for Variable Set Selection
# ==============================================================================
# Purpose: Compare all candidate variable sets via k-fold CV
#          following Ren et al. (2022) methodology
#
# Author: Michael Mühlbauer
# Date: 2026-01-21, Updated: 2026-02-04
#
# Pipeline structure:
#   Phase 1: Main Effect LASSO (phase1_lasso_base_covariates.r)
#            → ~79 base covariates from ~1,200 candidates
#   Phase 2: sparseR Hierarchical LASSO (phase2_sparseR_hierarchical.r)
#            → Two configurations: baseline and maximal
#   Phase 3: Cross-Validation (THIS SCRIPT)
#            → Compares 5 candidates: base, baseline_min, baseline_1se, maximal_min, maximal_1se
#            → Fits final winning model
#
# Candidates:
#   1. base: Phase 1 LASSO main effects only (82 vars)
#   2. baseline_min: Phase 2 baseline λ_min (poly2, mode×covar)
#   3. baseline_1se: Phase 2 baseline λ_1se (poly2, mode×covar)
#   4. maximal_min: Phase 2 maximal λ_min (poly3, all interactions)
#   5. maximal_1se: Phase 2 maximal λ_1se (poly3, all interactions)
#
# Input:
#   - data/output/models/phase1_prepared_data.rds (scaled data)
#   - data/output/variables/phase1_lasso_results.rds (base covariates)
#   - data/output/variables/phase2_sparseR_results_baseline.rds
#   - data/output/variables/phase2_sparseR_results_maximal.rds
#
# Output:
#   - data/output/models/phase3_cv_results.rds (CV comparison)
#   - data/output/models/fh_model_final.rds (winning model)
#   - data/output/models/phase3_model_data_final.rds (model data for plotting)
#
# ==============================================================================

rm(list = ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, tidyr, emdi, doParallel, foreach)

# ==============================================================================
# Configuration
# ==============================================================================

K_FOLDS <- 10                    # Number of CV folds
CV_THRESHOLD <- 0.20             # CV threshold for "reliable" domains (20%)
N_CORES <- min(60, parallel::detectCores() - 1)
RANDOM_SEED <- 42

data_path <- "./data"

message("\n", strrep("=", 70))
message("  Phase 3: Cross-Validation for Variable Set Selection")
message(strrep("=", 70), "\n")

message("Configuration:")
message("  K-folds: ", K_FOLDS)
message("  CV threshold: ", CV_THRESHOLD * 100, "%")
message("  Parallel cores: ", N_CORES)

# ==============================================================================
# STEP 1: Load Phase 1 and Phase 2 results
# ==============================================================================

message("\n=== Step 1: Loading Phase 1 and Phase 2 results ===")

# Phase 1: Prepared data (scaled/filtered covariate matrix)
prepared_data_path <- file.path(data_path, "output/models/phase1_prepared_data.rds")
if (!file.exists(prepared_data_path)) {
  stop("Phase 1 prepared data not found at: ", prepared_data_path,
       "\nPlease run phase1_lasso_base_covariates.r first")
}
prepared_data <- readRDS(prepared_data_path)
message("Loaded Phase 1 prepared data:")
message("  - Observations: ", nrow(prepared_data$combined_data))
message("  - Covariates: ", length(prepared_data$covar_cols))

# Phase 1: LASSO results (selected variables)
lasso_results_path <- file.path(data_path, "output/variables/phase1_lasso_results.rds")
if (!file.exists(lasso_results_path)) {
  stop("Phase 1 LASSO results not found at: ", lasso_results_path,
       "\nPlease run phase1_lasso_base_covariates.r first")
}
phase1_results <- readRDS(lasso_results_path)
message("Loaded Phase 1 LASSO: ", length(phase1_results$varlist), " selected variables")

# Phase 2: sparseR results - BASELINE
sparseR_baseline_path <- file.path(data_path, "output/variables/phase2_sparseR_results_baseline.rds")
if (!file.exists(sparseR_baseline_path)) {
  stop("Phase 2 baseline results not found at: ", sparseR_baseline_path,
       "\nPlease run phase2_sparseR_hierarchical.r first")
}
sparseR_baseline <- readRDS(sparseR_baseline_path)
message("Loaded Phase 2 BASELINE (", sparseR_baseline$config$description, "):")
message("  - lambda.min: ", length(sparseR_baseline$selected_lambda_min), " variables")
message("  - lambda.1se: ", length(sparseR_baseline$selected_lambda_1se), " variables")

# Phase 2: sparseR results - MAXIMAL
sparseR_maximal_path <- file.path(data_path, "output/variables/phase2_sparseR_results_maximal.rds")
if (!file.exists(sparseR_maximal_path)) {
  stop("Phase 2 maximal results not found at: ", sparseR_maximal_path,
       "\nPlease run phase2_sparseR_hierarchical.r first")
}
sparseR_maximal <- readRDS(sparseR_maximal_path)
message("Loaded Phase 2 MAXIMAL (", sparseR_maximal$config$description, "):")
message("  - lambda.min: ", length(sparseR_maximal$selected_lambda_min), " variables")
message("  - lambda.1se: ", length(sparseR_maximal$selected_lambda_1se), " variables")

# ==============================================================================
# STEP 2: Prepare base model data
# ==============================================================================

message("\n=== Step 2: Preparing base model data ===")

# Extract prepared data
df_combined <- prepared_data$combined_data
id_cols <- prepared_data$id_cols
outcome_cols <- prepared_data$outcome_cols
mode_contrast_vars <- prepared_data$mode_contrast_cols

# Extract LASSO-selected variable names for base model
vars_base_raw <- phase1_results$varlist

# Remove known multicollinearity issues
vars_to_exclude <- c("mid_hvm_imp_prop_1", "mid_vm_kombi_prop_2")
vars_base_raw <- setdiff(vars_base_raw, vars_to_exclude)

# Check availability and add mode contrasts
available_vars_base <- intersect(vars_base_raw, names(df_combined))
vars_base <- c(available_vars_base, mode_contrast_vars)

message("Base model variables: ", length(vars_base))
message("  - LASSO-selected: ", length(available_vars_base))
message("  - Mode contrasts: ", length(mode_contrast_vars))

# Filter data for FH model
df_fh_base <- df_combined %>%
  filter(perskm_hth > 0, var_hth > 0) %>%
  as.data.frame()

message("Base model data: ", nrow(df_fh_base), " domains")

# ==============================================================================
# STEP 3: Prepare sparseR model data (baseline and maximal)
# ==============================================================================

message("\n=== Step 3: Preparing sparseR model data ===")

# Function to rename sparseR variables for emdi (replace : with _X_)
rename_for_emdi <- function(names) {
  gsub(":", "_X_", names)
}

# --- BASELINE ---
model_matrix_baseline <- sparseR_baseline$model_matrix
vars_baseline_min_raw <- sparseR_baseline$selected_lambda_min
vars_baseline_1se_raw <- sparseR_baseline$selected_lambda_1se

model_matrix_baseline_renamed <- model_matrix_baseline
colnames(model_matrix_baseline_renamed) <- rename_for_emdi(colnames(model_matrix_baseline_renamed))

vars_baseline_min <- rename_for_emdi(vars_baseline_min_raw)
vars_baseline_1se <- rename_for_emdi(vars_baseline_1se_raw)

message("BASELINE sparseR variables:")
message("  - lambda.min: ", length(vars_baseline_min), " variables")
message("  - lambda.1se: ", length(vars_baseline_1se), " variables")

df_fh_baseline <- data.frame(
  ags5_hvm = df_fh_base$ags5_hvm,
  ags5 = df_fh_base$ags5,
  perskm_hth = df_fh_base$perskm_hth,
  var_hth = df_fh_base$var_hth
)
df_fh_baseline <- cbind(df_fh_baseline, model_matrix_baseline_renamed)

message("BASELINE model data: ", nrow(df_fh_baseline), " domains x ", ncol(df_fh_baseline), " columns")

# --- MAXIMAL ---
model_matrix_maximal <- sparseR_maximal$model_matrix
vars_maximal_min_raw <- sparseR_maximal$selected_lambda_min
vars_maximal_1se_raw <- sparseR_maximal$selected_lambda_1se

model_matrix_maximal_renamed <- model_matrix_maximal
colnames(model_matrix_maximal_renamed) <- rename_for_emdi(colnames(model_matrix_maximal_renamed))

vars_maximal_min <- rename_for_emdi(vars_maximal_min_raw)
vars_maximal_1se <- rename_for_emdi(vars_maximal_1se_raw)

message("MAXIMAL sparseR variables:")
message("  - lambda.min: ", length(vars_maximal_min), " variables")
message("  - lambda.1se: ", length(vars_maximal_1se), " variables")

df_fh_maximal <- data.frame(
  ags5_hvm = df_fh_base$ags5_hvm,
  ags5 = df_fh_base$ags5,
  perskm_hth = df_fh_base$perskm_hth,
  var_hth = df_fh_base$var_hth
)
df_fh_maximal <- cbind(df_fh_maximal, model_matrix_maximal_renamed)

message("MAXIMAL model data: ", nrow(df_fh_maximal), " domains x ", ncol(df_fh_maximal), " columns")

# ==============================================================================
# STEP 4: Add CV info and create folds
# ==============================================================================

message("\n=== Step 4: Adding CV info and creating folds ===")

# Load direct estimates for sample sizes and CV
df_direct <- readRDS(file.path(data_path, "output/direct/df_HTH_perskm_analytical.rds"))

# Add n_obs and cv_hth to all datasets
add_cv_info <- function(df) {
  if ("n_obs" %in% names(df_direct) && "cv_hth" %in% names(df_direct)) {
    df <- df %>%
      left_join(
        df_direct %>% select(ags5_hvm, n_obs, cv_hth),
        by = "ags5_hvm"
      )
  } else {
    df <- df %>%
      mutate(
        se_hth = sqrt(var_hth),
        cv_hth = se_hth / perskm_hth,
        n_obs = 1 / var_hth
      )
  }
  return(df)
}

df_fh_base <- add_cv_info(df_fh_base)
df_fh_baseline <- add_cv_info(df_fh_baseline)
df_fh_maximal <- add_cv_info(df_fh_maximal)

# Identify reliable domains
df_fh_base <- df_fh_base %>% mutate(is_reliable = cv_hth < CV_THRESHOLD)
df_fh_baseline <- df_fh_baseline %>% mutate(is_reliable = cv_hth < CV_THRESHOLD)
df_fh_maximal <- df_fh_maximal %>% mutate(is_reliable = cv_hth < CV_THRESHOLD)

n_reliable <- sum(df_fh_base$is_reliable, na.rm = TRUE)
message("Reliable domains (CV < ", CV_THRESHOLD * 100, "%): ", n_reliable, " / ", nrow(df_fh_base))

# Create stratified CV folds
create_stratified_folds <- function(data, n_folds, strat_var = "n_obs", seed = 42) {
  set.seed(seed)
  n <- nrow(data)
  sorted_idx <- order(data[[strat_var]])
  n_groups <- ceiling(n / n_folds)
  fold_assignment <- rep(NA, n)

  for (g in 1:n_groups) {
    start_idx <- (g - 1) * n_folds + 1
    end_idx <- min(g * n_folds, n)
    group_indices <- sorted_idx[start_idx:end_idx]
    n_in_group <- length(group_indices)
    random_folds <- sample(1:n_folds, size = n_in_group, replace = FALSE)
    fold_assignment[group_indices] <- random_folds
  }

  return(fold_assignment)
}

# Create folds for base data (will be transferred to all others)
df_fh_base$fold <- create_stratified_folds(df_fh_base, n_folds = K_FOLDS, strat_var = "n_obs", seed = RANDOM_SEED)

# Transfer fold assignments to other datasets
fold_mapping <- df_fh_base %>% select(ags5_hvm, fold)
df_fh_baseline <- df_fh_baseline %>% left_join(fold_mapping, by = "ags5_hvm")
df_fh_maximal <- df_fh_maximal %>% left_join(fold_mapping, by = "ags5_hvm")

# Verify stratification
fold_summary <- df_fh_base %>%
  group_by(fold) %>%
  summarise(n = n(), .groups = "drop")

message("Fold sizes: ", paste(fold_summary$n, collapse = ", "))

# ==============================================================================
# STEP 5: Define candidate variable sets
# ==============================================================================

message("\n=== Step 5: Candidate variable sets (5 total) ===")

# Store candidate info with data source
candidate_info <- list(
  base = list(
    vars = vars_base,
    data = "base",
    config = "Phase 1 LASSO",
    description = "Main effects only"
  ),
  baseline_min = list(
    vars = vars_baseline_min,
    data = "baseline",
    config = "baseline λ_min",
    description = sparseR_baseline$config$description
  ),
  baseline_1se = list(
    vars = vars_baseline_1se,
    data = "baseline",
    config = "baseline λ_1se",
    description = sparseR_baseline$config$description
  ),
  maximal_min = list(
    vars = vars_maximal_min,
    data = "maximal",
    config = "maximal λ_min",
    description = sparseR_maximal$config$description
  ),
  maximal_1se = list(
    vars = vars_maximal_1se,
    data = "maximal",
    config = "maximal λ_1se",
    description = sparseR_maximal$config$description
  )
)

for (name in names(candidate_info)) {
  info <- candidate_info[[name]]
  message("  ", name, ": ", length(info$vars), " variables (", info$description, ")")
}

# ==============================================================================
# STEP 6: Cross-validation function
# ==============================================================================

message("\n=== Step 6: Defining CV function ===")

fit_and_predict <- function(train_data, valid_data, vars) {
  # Check which variables are available
  vars_available <- vars[vars %in% names(train_data)]

  if (length(vars_available) == 0) {
    return(data.frame(
      ags5_hvm = valid_data$ags5_hvm,
      prediction = NA,
      direct = valid_data$perskm_hth,
      sq_error = NA,
      is_reliable = valid_data$is_reliable,
      converged = FALSE
    ))
  }

  # Build formula
  formula_str <- paste("perskm_hth ~", paste(vars_available, collapse = " + "))
  formula_fh <- as.formula(formula_str)

  tryCatch({
    # Fit FH model on training data
    fh_fit <- emdi::fh(
      fixed = formula_fh,
      vardir = "var_hth",
      combined_data = train_data,
      domains = "ags5_hvm",
      method = "ml",
      transformation = "log",
      backtransformation = "bc_sm",
      MSE = FALSE  # Skip MSE for speed
    )

    # Extract fixed effects coefficients
    beta <- fh_fit$model$coefficients$coefficients

    # Create design matrix for validation data
    X_valid <- model.matrix(formula_fh, data = valid_data)

    # Predict using fixed effects only (Xβ), on log scale
    log_predictions <- X_valid %*% beta

    # Back-transform to original scale
    predictions <- exp(log_predictions)

    # Calculate squared errors
    sq_errors <- (predictions - valid_data$perskm_hth)^2

    return(data.frame(
      ags5_hvm = valid_data$ags5_hvm,
      prediction = as.numeric(predictions),
      direct = valid_data$perskm_hth,
      sq_error = as.numeric(sq_errors),
      is_reliable = valid_data$is_reliable,
      converged = TRUE
    ))

  }, error = function(e) {
    return(data.frame(
      ags5_hvm = valid_data$ags5_hvm,
      prediction = NA,
      direct = valid_data$perskm_hth,
      sq_error = NA,
      is_reliable = valid_data$is_reliable,
      converged = FALSE
    ))
  })
}

# ==============================================================================
# STEP 7: Run cross-validation (parallel)
# ==============================================================================

message("\n=== Step 7: Running cross-validation ===")

# Nested parallelization: outer loop (candidates) AND inner loop (folds)
# Total parallel jobs = 5 candidates × 10 folds = 50 jobs
# Use %:% operator for nested foreach loops

cl <- makeCluster(N_CORES)
registerDoParallel(cl)

# Export all datasets and functions to workers
clusterExport(cl, c("df_fh_base", "df_fh_baseline", "df_fh_maximal",
                    "fit_and_predict", "candidate_info", "K_FOLDS"))
clusterEvalQ(cl, {
  library(emdi)
  library(dplyr)
})

message("Running ", length(candidate_info), " candidates × ", K_FOLDS, " folds = ",
        length(candidate_info) * K_FOLDS, " jobs in parallel...")
start_time_total <- Sys.time()

# Nested parallel loop: outer (candidates) %:% inner (folds)
cv_results_nested <- foreach(
  candidate_name = names(candidate_info),
  .packages = c("emdi", "dplyr"),
  .errorhandling = "pass"
) %:%
  foreach(
    k = 1:K_FOLDS,
    .packages = c("emdi", "dplyr"),
    .combine = rbind,
    .errorhandling = "pass"
  ) %dopar% {

    info <- candidate_info[[candidate_name]]
    vars <- info$vars

    # Select appropriate data source
    if (info$data == "base") {
      df_cv <- df_fh_base
    } else if (info$data == "baseline") {
      df_cv <- df_fh_baseline
    } else {
      df_cv <- df_fh_maximal
    }

    train_data <- df_cv[df_cv$fold != k, ]
    valid_data <- df_cv[df_cv$fold == k, ]

    result <- fit_and_predict(train_data, valid_data, vars)
    result$fold <- k
    result$candidate <- candidate_name

    return(result)
  }

stopCluster(cl)

end_time_total <- Sys.time()
elapsed_total <- round(difftime(end_time_total, start_time_total, units = "secs"), 1)
message("All candidates completed in ", elapsed_total, " seconds")

# Convert nested results to named list
cv_results_all <- list()
for (i in seq_along(cv_results_nested)) {
  candidate_name <- names(candidate_info)[i]
  fold_results <- cv_results_nested[[i]]

  if (is.data.frame(fold_results)) {
    cv_results_all[[candidate_name]] <- fold_results

    # Summary
    total_sse <- sum(fold_results$sq_error, na.rm = TRUE)
    reliable_sse <- sum(fold_results$sq_error[fold_results$is_reliable], na.rm = TRUE)
    n_vars <- length(candidate_info[[candidate_name]]$vars)

    message("  ", candidate_name, " (", n_vars, " vars):")
    message("    Total SSE: ", round(total_sse, 4), ", Reliable SSE: ", round(reliable_sse, 4))
  } else {
    message("  ", candidate_name, ": ERROR - CV failed")
  }
}

# ==============================================================================
# STEP 8: Compare results and select winner
# ==============================================================================

message("\n=== Step 8: Comparing results ===")

# Combine all results
cv_all <- bind_rows(cv_results_all)

# Summary by candidate
cv_summary <- cv_all %>%
  group_by(candidate) %>%
  summarise(
    n_domains = n(),
    n_reliable = sum(is_reliable, na.rm = TRUE),
    total_sse = sum(sq_error, na.rm = TRUE),
    reliable_sse = sum(sq_error[is_reliable], na.rm = TRUE),
    mean_sq_error = mean(sq_error, na.rm = TRUE),
    mean_sq_error_reliable = mean(sq_error[is_reliable], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(reliable_sse)

# Add candidate metadata
cv_summary <- cv_summary %>%
  rowwise() %>%
  mutate(
    n_vars = length(candidate_info[[candidate]]$vars),
    config = candidate_info[[candidate]]$config
  ) %>%
  ungroup()

message("\nCV Summary (sorted by reliable SSE):")
print(cv_summary %>% select(candidate, n_vars, config, reliable_sse, mean_sq_error_reliable))

# Select winner (lowest reliable SSE)
winner <- cv_summary %>%
  filter(reliable_sse == min(reliable_sse)) %>%
  pull(candidate)

message("\n=== Winner: ", winner, " ===")
message("  (Lowest SSE on reliable domains)")

# ==============================================================================
# STEP 9: Fit final model with winning variable set
# ==============================================================================

message("\n=== Step 9: Fitting final model ===")

winning_info <- candidate_info[[winner]]
winning_vars <- winning_info$vars
message("Using ", length(winning_vars), " variables from ", winner)

# Select appropriate data for final model
if (winning_info$data == "base") {
  df_final <- df_fh_base
} else if (winning_info$data == "baseline") {
  df_final <- df_fh_baseline
} else {
  df_final <- df_fh_maximal
}
message("  Data source: ", winning_info$data)

formula_final <- as.formula(paste("perskm_hth ~", paste(winning_vars, collapse = " + ")))

start_time <- Sys.time()

fh_final <- emdi::fh(
  fixed = formula_final,
  vardir = "var_hth",
  combined_data = df_final,
  domains = "ags5_hvm",
  method = "ml",
  transformation = "log",
  backtransformation = "bc_sm",
  MSE = TRUE  # Include MSE for final model
)

end_time <- Sys.time()
elapsed <- round(difftime(end_time, start_time, units = "mins"), 2)
message("Final model fitted in ", elapsed, " minutes")

# Diagnostics
std_re <- fh_final$model$random_effects
n <- length(std_re)
m <- mean(std_re)
s <- sd(std_re)
kurt <- sum((std_re - m)^4) / (n * s^4)
skew <- sum((std_re - m)^3) / (n * s^3)

message("\nFinal model diagnostics:")
message("  Variables: ", nrow(fh_final$model$coefficients) - 1)
message("  sigma_u: ", round(fh_final$model$variance, 4))
message("  Kurtosis: ", round(kurt, 2), " (target: 3.0)")
message("  Skewness: ", round(skew, 2), " (target: 0.0)")

# ==============================================================================
# STEP 10: Save results
# ==============================================================================

message("\n=== Step 10: Saving results ===")

output_dir <- file.path(data_path, "output/models")
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Save CV results
cv_results <- list(
  cv_all = cv_all,
  cv_summary = cv_summary,
  winner = winner,
  candidate_info = candidate_info,
  config = list(
    k_folds = K_FOLDS,
    cv_threshold = CV_THRESHOLD,
    n_cores = N_CORES
  ),
  timestamp = Sys.time()
)

saveRDS(cv_results, file.path(output_dir, "phase3_cv_results.rds"))
message("Saved: phase3_cv_results.rds")

# Save final model
saveRDS(fh_final, file.path(output_dir, "fh_model_final.rds"))
message("Saved: fh_model_final.rds")

# Save model data for downstream scripts (plotting, diagnostics)
final_model_data <- list(
  model = fh_final,
  formula = formula_final,
  selected_vars = winning_vars,
  winner = winner,
  winner_data_source = winning_info$data,
  cv_summary = cv_summary,
  diagnostics = list(
    variance = fh_final$model$variance,
    kurtosis = kurt,
    skewness = skew
  ),
  combined_data = df_final,
  # Also save data for all candidates (needed by fit_fh_models.r)
  base_data = list(
    combined_data = df_fh_base,
    selected_vars = vars_base
  ),
  baseline_data = list(
    combined_data = df_fh_baseline,
    vars_min = vars_baseline_min,
    vars_1se = vars_baseline_1se,
    vars_min_raw = vars_baseline_min_raw,
    vars_1se_raw = vars_baseline_1se_raw,
    config = sparseR_baseline$config
  ),
  maximal_data = list(
    combined_data = df_fh_maximal,
    vars_min = vars_maximal_min,
    vars_1se = vars_maximal_1se,
    vars_min_raw = vars_maximal_min_raw,
    vars_1se_raw = vars_maximal_1se_raw,
    config = sparseR_maximal$config
  ),
  timestamp = Sys.time()
)

saveRDS(final_model_data, file.path(output_dir, "phase3_model_data_final.rds"))
message("Saved: phase3_model_data_final.rds")

# ==============================================================================
# Summary
# ==============================================================================

message("\n", strrep("=", 70))
message("  Phase 3 Complete!")
message(strrep("=", 70), "\n")

message("CV Results (sorted by reliable SSE):")
for (i in 1:nrow(cv_summary)) {
  msg <- paste0("  ", i, ". ", cv_summary$candidate[i], " (",
                cv_summary$n_vars[i], " vars): ",
                round(cv_summary$reliable_sse[i], 2), " SSE")
  if (cv_summary$candidate[i] == winner) msg <- paste0(msg, " <- WINNER")
  message(msg)
}

message("\nFinal model (", winner, "):")
message("  Variables: ", length(winning_vars))
message("  sigma_u: ", round(fh_final$model$variance, 4))
message("  Kurtosis: ", round(kurt, 2))

message("\nOutputs:")
message("  - phase3_cv_results.rds (CV comparison of 5 candidates)")
message("  - fh_model_final.rds (winning model)")
message("  - phase3_model_data_final.rds (data for downstream scripts)")
message("\nNext: Run fit_fh_models.r to fit all candidates for diagnostics")
