# ==============================================================================
# Phase 1: Base LASSO Variable Selection
# ==============================================================================
# Purpose: LASSO-based variable selection for multimodal FH model (log and non-log)
# Author: Michael Mühlbauer
# Date: 2025-12-16
# Updated: 2026-01-14 - Added non-log LASSO for fair comparison
#
# This is Phase 1 of the 3-phase variable selection pipeline:
#   Phase 1: Base LASSO (THIS SCRIPT) - selects ~79 covariates from ~1,200 candidates
#   Phase 2: Hierarchical LASSO with sparseR (phase2_sparseR_hierarchical.r)
#   Phase 3: Cross-Validation & Final Model Selection (phase3_cross_validation.r)
#
# Model specification:
# - Domain: ags5 × hvm (1604 domains) instead of ags5 (401 districts)
# - Outcome: TWO versions for fair comparison
#   1. log(perskm_hth) - log-transformed to match log FH model
#   2. perskm_hth - non-transformed to match non-log FH model
# - Covariates: MiD (mode-specific) + Census (replicated across modes) + INKAR + mode contrasts
# - Variance: Analytical variance from survey package (not bootstrap)
# - Includes sum-to-zero contrast coded mode variables (mode_C1, mode_C2, mode_C3)
#
# Input:
#   - data/output/direct/df_HTH_perskm_analytical.rds (direct estimates)
#   - data/output/aggregates/df_mid_covariates_combined.rds (MiD covariates)
#   - data/raw/census/census_merged_2017.rds (census covariates)
#   - data/temp/inkar_2017_districts.rds (INKAR covariates)
#
# Output:
#   - data/output/variables/phase1_lasso_results.rds (log model, backward compatible)
#   - data/output/variables/phase1_lasso_results_log.rds (log model with metadata)
#   - data/output/variables/phase1_lasso_results_nolog.rds (non-log model)
#   - data/output/variables/phase1_lasso_results_combined.rds (both models + comparison)
#
# Runtime: ~2-3 hours with parallel processing on 40 cores (2 models)
# ==============================================================================
rm(list = ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, patchwork, caret, glmnet, reshape, rsample, viridis, doParallel)

data_path <- "./data"

# Configuration ----------------------------------------------------------------
FILTER_CORRELATED_VARS <- TRUE  # Set to TRUE to remove highly correlated variables (r > 0.7)
INCLUDE_MID_COVARIATES <- FALSE  # Set to FALSE to exclude MiD-derived covariates
# Rationale: MiD covariates introduce measurement error (violates Ybarra-Lohr condition)
# but contribute only ~2% additional R² (91.3% → 89%). Mode contrasts + census/INKAR retained.
# See documentation/notes/plan_remove_mid_covariates.md for details.

# Enable parallel computing ---------------------------------------------------
library(doParallel)

# Stop any existing clusters first
if (exists(".cluster")) {
  try(stopCluster(.cluster), silent = TRUE)
}
try(stopImplicitCluster(), silent = TRUE)

# Use fewer cores to avoid hitting connection limit (125 max)
# Each core uses 2 connections, so limit to ~40 cores
n_cores <- min(40, parallel::detectCores() - 2)
cl <- makeCluster(n_cores)
registerDoParallel(cl)

# Store cluster for cleanup
.cluster <- cl

message("Parallel processing enabled with ", n_cores, " cores")

# Load direct estimates --------------------------------------------------------

message("\n=== Loading direct estimates ===")
df_HTH_perskm <- readRDS(file.path(data_path, "output/direct/df_HTH_perskm_analytical.rds"))

message("Loaded ", nrow(df_HTH_perskm), " direct estimates")
message("Domains: ", length(unique(df_HTH_perskm$ags5_hvm)))
message("Districts: ", length(unique(df_HTH_perskm$ags5)))
message("Modes: ", toString(unique(df_HTH_perskm$hvm_label)))

# Prepare direct estimates
df_direct <- df_HTH_perskm %>%
  select(ags5, hvm_label, ags5_hvm, perskm_hth, var_hth) %>%
  mutate(
    # Log-transform outcome to match FH model specification
    perskm_hth_log = log(perskm_hth),
    # Coefficient of variation for weighted LASSO
    cv = sqrt(var_hth) / perskm_hth,
    # Replace infinite CV (when perskm_hth = 0) with large value
    cv = if_else(is.infinite(cv) | is.na(cv), 10, cv)
  )

# Load MiD covariates (ags5 × mode level) -------------------------------------
# Note: Variables already have mid_ prefix (added in aggregate_mid_covariates.r)

message("\n=== Loading MiD covariates (mode-specific) ===")

if (INCLUDE_MID_COVARIATES) {
  df_mid_covars <- readRDS(file.path(data_path, "output/aggregates/df_mid_covariates_combined.rds"))
  message("Loaded MiD covariates: ", nrow(df_mid_covars), " rows × ", ncol(df_mid_covars), " columns")
} else {
  message("SKIPPING MiD covariates (INCLUDE_MID_COVARIATES = FALSE)")
  message("Using only Census/INKAR covariates + mode contrasts")
  # Load MiD data just for domain structure (excludes zero-value domains),
  # but drop all covariate columns
  df_mid_full <- readRDS(file.path(data_path, "output/aggregates/df_mid_covariates_combined.rds"))
  df_mid_covars <- df_mid_full %>%
    select(ags5, hvm_label, ags5_hvm)
  message("Using MiD domain structure: ", nrow(df_mid_covars), " domains (covariates excluded)")
}

message("Domains in data: ", length(unique(df_mid_covars$ags5_hvm)))

# Check alignment
direct_domains <- sort(unique(df_direct$ags5_hvm))
mid_domains <- sort(unique(df_mid_covars$ags5_hvm))
message("Direct domains: ", length(direct_domains))
message("Covariate domains: ", length(mid_domains))
message("Matching domains: ", sum(direct_domains %in% mid_domains))

# Load Census covariates (ags5 level) -----------------------------------------
# Note: census_merged_2017.rds includes all tables (main + additional)
# generated by census_data_acquisition.r
# Variables already have census_ prefix (added in census_data_acquisition.r)

message("\n=== Loading census covariates (district-level) ===")
df_census <- readRDS(file.path(data_path, "raw/census/census_merged_2017.rds")) %>%
  # Ensure ags5 is 5-digit character (fix potential leading zero issues)
  mutate(ags5 = str_pad(as.character(ags5), width = 5, side = "left", pad = "0"))

message("Loaded census covariates: ", nrow(df_census), " rows × ", ncol(df_census), " columns")
message("Districts in census: ", length(unique(df_census$ags5)))

# Expand census to ags5 × mode level (replicate across modes)
df_census_expanded <- df_census %>%
  crossing(hvm_label = c("fuss", "fahrrad", "miv", "oepv")) %>%
  mutate(ags5_hvm = paste(ags5, hvm_label, sep = "_"))

message("Expanded census to ", nrow(df_census_expanded), " rows (replicated 4× per district)")

# Load INKAR covariates (ags5 level) ------------------------------------------

message("\n=== Loading INKAR covariates (district-level) ===")
inkar_file <- file.path(data_path, "temp/inkar_2017_districts.rds")

if (file.exists(inkar_file)) {
  df_inkar <- readRDS(inkar_file)

  message("Loaded INKAR covariates: ", nrow(df_inkar), " rows × ", ncol(df_inkar), " columns")
  message("Districts in INKAR: ", length(unique(df_inkar$ags5)))

  # Expand INKAR to ags5 × mode level (replicate across modes)
  df_inkar_expanded <- df_inkar %>%
    crossing(hvm_label = c("fuss", "fahrrad", "miv", "oepv")) %>%
    mutate(ags5_hvm = paste(ags5, hvm_label, sep = "_"))

  message("Expanded INKAR to ", nrow(df_inkar_expanded), " rows (replicated 4× per district)")

  # Merge census and INKAR
  df_census_expanded <- df_census_expanded %>%
    left_join(df_inkar_expanded, by = c("ags5", "hvm_label", "ags5_hvm"), suffix = c("", "_inkar"))

  message("Merged census + INKAR: ", ncol(df_census_expanded), " total columns")

} else {
  warning("INKAR file not found: ", inkar_file)
  message("Proceeding without INKAR covariates")
  message("Run R/data_preparation/inkar_data_acquisition.r to download INKAR data")
}

# Create sum-to-zero contrast coded mode variables ----------------------------

message("\n=== Creating sum-to-zero contrast coded mode variables ===")

# Ensure hvm_label is a factor with explicit level order
df_mid_covars <- df_mid_covars %>%
  mutate(hvm_label = factor(hvm_label, levels = c("fuss", "fahrrad", "miv", "oepv")))

# Create sum-to-zero contrast matrix
# Rows = modes (fuss, fahrrad, miv, oepv)
# Columns = contrasts (C1, C2, C3)
# Last category (oepv) gets -1 for all contrasts
contrasts(df_mid_covars$hvm_label) <- contr.sum(4)

# Extract contrast-coded variables as separate columns
mode_contrasts <- model.matrix(~ hvm_label, data = df_mid_covars)[, -1]  # Remove intercept
colnames(mode_contrasts) <- c("mode_C1", "mode_C2", "mode_C3")

# Add to data frame using cbind (more robust than bind_cols)
df_mid_covars <- cbind(df_mid_covars, as.data.frame(mode_contrasts))

message("Created 3 sum-to-zero contrast variables for mode:")
message("  mode_C1: fuss vs. grand mean")
message("  mode_C2: fahrrad vs. grand mean")
message("  mode_C3: miv vs. grand mean")
message("  (oepv is reference: coded as -1 across all contrasts)")

# Merge all covariates ---------------------------------------------------------

message("\n=== Merging covariates ===")

# Merge domain structure (with mode contrasts) with census/INKAR covariates
# When INCLUDE_MID_COVARIATES=TRUE: df_mid_covars contains MiD covariates + mode contrasts
# When INCLUDE_MID_COVARIATES=FALSE: df_mid_covars contains only domain IDs + mode contrasts
df_Covars <- df_mid_covars %>%
  inner_join(df_census_expanded, by = c("ags5", "hvm_label", "ags5_hvm"))

message("Merged covariates: ", nrow(df_Covars), " rows × ", ncol(df_Covars), " columns")
message("Includes mode contrasts: mode_C1, mode_C2, mode_C3")

# Merge with direct estimates
# Use inner_join to exclude domains without MiD covariates (10 domains with dir=0)
df_combined <- df_direct %>%
  inner_join(df_Covars, by = c("ags5", "hvm_label", "ags5_hvm")) %>%
  filter(
    ags5 != "16056"   # Exclude Eisenach (merged into 16063 Wartburgkreis on 2021-07-01, not in INKAR)
  )

message("Combined data: ", nrow(df_combined), " rows × ", ncol(df_combined), " columns")
message("Excluded domains: ", nrow(df_direct) - nrow(df_combined), " (16056=Eisenach)")

# Filter problematic variables BEFORE scaling ---------------------------------

message("\n=== Filtering problematic variables ===")

# Identify covariate columns (exclude IDs, outcome, variance, and mode contrasts)
id_cols <- c("ags5", "hvm_label", "ags5_hvm")
outcome_cols <- c("perskm_hth", "perskm_hth_log", "var_hth", "cv")
mode_contrast_cols <- c("mode_C1", "mode_C2", "mode_C3")

# Separate mode contrasts from other covariates for filtering
# Mode contrasts will be protected from filtering and added back later
covar_cols <- setdiff(names(df_combined), c(id_cols, outcome_cols, mode_contrast_cols))

message("Mode contrasts protected from filtering: ", paste(mode_contrast_cols, collapse = ", "))

message("Initial covariates: ", length(covar_cols))

# Remove all columns with ANY missing values (strict 0% threshold)
# After excluding district 16056, INKAR variables should align properly
# Expected: ~480/540 INKAR variables survive (those without any missing districts)
na_proportion <- colMeans(is.na(df_combined[, covar_cols]))
cols_with_na <- names(na_proportion[na_proportion > 0])

message("Removing ", length(cols_with_na), " columns with ANY missing values")
if (length(cols_with_na) > 0) {
  message("Examples: ", paste(head(cols_with_na, 5), collapse = ", "))
}

# Count INKAR variable survival
inkar_in_covar_cols <- covar_cols[grepl("^inkar_", covar_cols)]
inkar_removed <- sum(grepl("^inkar_", cols_with_na))
inkar_kept <- length(inkar_in_covar_cols) - inkar_removed
message("INKAR variables: ", inkar_kept, " kept, ", inkar_removed, " removed (",
        round(100 * inkar_kept / length(inkar_in_covar_cols), 1), "% retention)")

df_filtered <- df_combined %>%
  select(-all_of(cols_with_na))

# Update covariate list
covar_cols <- setdiff(names(df_filtered), c(id_cols, outcome_cols))
message("After NA filtering: ", length(covar_cols), " covariates remain")

# Verify no NAs remain
if (any(is.na(df_filtered %>% select(all_of(covar_cols))))) {
  stop("ERROR: NAs still present after filtering!")
}

# Remove columns with near-zero variance
nzv_cols <- caret::nearZeroVar(df_filtered %>% select(all_of(covar_cols)))
if (length(nzv_cols) > 0) {
  nzv_names <- covar_cols[nzv_cols]
  message("Removing ", length(nzv_names), " near-zero variance columns")
  df_filtered <- df_filtered %>%
    select(-all_of(nzv_names))
  # Update covariate list
  covar_cols <- setdiff(names(df_filtered), c(id_cols, outcome_cols))
}

message("Final dataset: ", nrow(df_filtered), " observations × ", length(covar_cols), " covariates")

# Scale covariates -------------------------------------------------------------

message("\n=== Scaling covariates ===")

# Scale continuous covariates (MiD means + census + INKAR)
# Do NOT scale proportions (0-1 bounded) or mode contrasts (already centered)
mid_mean_cols <- covar_cols[grepl("^mid_.*_mean$", covar_cols)]
mid_prop_cols <- covar_cols[grepl("^mid_.*_prop_", covar_cols)]
census_cols <- covar_cols[grepl("^census_", covar_cols)]
inkar_cols <- covar_cols[grepl("^inkar_", covar_cols)]

# Exclude mode contrasts from census variables
census_cols <- setdiff(census_cols, mode_contrast_cols)

scale_cols <- c(mid_mean_cols, census_cols, inkar_cols)
message("Scaling ", length(scale_cols), " continuous covariates:")
message("  MiD means: ", length(mid_mean_cols))
message("  Census: ", length(census_cols))
message("  INKAR: ", length(inkar_cols))
message("NOT scaling: ", length(mid_prop_cols), " MiD proportions + 3 mode contrasts")

df_Covars_scaled <- df_filtered %>%
  mutate(across(all_of(scale_cols), scale))

message("After filtering: ", nrow(df_Covars_scaled), " rows × ",
        ncol(df_Covars_scaled), " columns")
message("Mode contrasts still present: ", all(mode_contrast_cols %in% names(df_Covars_scaled)))

# Step 1: Filter highly correlated variables (OPTIONAL) -----------------------

if (FILTER_CORRELATED_VARS) {
  message("\n=== Step 1: Filtering multicollinear variables ===")

  # Calculate correlation matrix (exclude IDs, outcome, AND mode contrasts)
  # Mode contrasts are protected from correlation filtering
  cor_vars <- setdiff(names(df_Covars_scaled), c(id_cols, outcome_cols, mode_contrast_cols))
  cor_matrix <- df_Covars_scaled %>%
    select(all_of(cor_vars)) %>%
    cor(use = "complete.obs")

  # Find highly correlated variables (cutoff = 0.7 from Ren & Li 2022)
  vec_high_Correl_Vars <- caret::findCorrelation(
    cor_matrix,
    cutoff = 0.7,
    exact = TRUE
  ) %>%
    as.vector() %>%
    sort()

  if (length(vec_high_Correl_Vars) > 0) {
    message("Removing ", length(vec_high_Correl_Vars), " highly correlated variables")
    high_cor_names <- cor_vars[vec_high_Correl_Vars]
    message("Examples: ", paste(head(high_cor_names, 5), collapse = ", "))

    df_low_multikor <- df_Covars_scaled %>%
      select(-all_of(high_cor_names))
  } else {
    message("No highly correlated variables found")
    df_low_multikor <- df_Covars_scaled
  }

  message("After correlation filter: ", ncol(df_low_multikor) - length(c(id_cols, outcome_cols, mode_contrast_cols)),
          " candidate covariates (+ 3 protected mode contrasts)")
  message("Mode contrasts still present: ", all(mode_contrast_cols %in% names(df_low_multikor)))
} else {
  message("\n=== Step 1: SKIPPED (correlation filtering disabled) ===")
  df_low_multikor <- df_Covars_scaled
}

# Prepare for LASSO ------------------------------------------------------------

message("\n=== Preparing data for LASSO ===")

df_combined_step1 <- df_low_multikor %>%
  mutate(ags5_hvm = as.factor(ags5_hvm))

# Parameters
nlambda <- 200
maxit <- 10000000

message("LASSO parameters:")
message("  nlambda: ", nlambda)
message("  maxit: ", maxit)

# Step 2: LASSO regression (LOG and NON-LOG) ----------------------------------

message("\n=== Step 2: LASSO variable selection (Log and Non-Log) ===")

set.seed(42)  # For reproducibility

# Train/Test split (80/20) - same split for both models
df_split <- initial_split(df_combined_step1, prop = 0.8)
df_train <- training(df_split) %>%
  select(-all_of(c(id_cols, "var_hth", "cv")))
df_test <- testing(df_split) %>%
  select(-all_of(c(id_cols, "var_hth", "cv")))

message("Training set: ", nrow(df_train), " observations")
message("Test set: ", nrow(df_test), " observations")

# ==============================================================================
# 2a. LOG-TRANSFORMED LASSO
# ==============================================================================

message("\n\n=== Fitting LOG-TRANSFORMED LASSO ===")
message("Outcome: log(perskm_hth)")

# Create model matrices for log-transformed outcome
df_train_x_log <- model.matrix(perskm_hth_log ~ . - perskm_hth, df_train)[, -1]
df_train_y_log <- df_train$perskm_hth_log

df_test_x_log <- model.matrix(perskm_hth_log ~ . - perskm_hth, df_test)[, -1]
df_test_y_log <- df_test$perskm_hth_log

message("Design matrix dimensions: ", nrow(df_train_x_log), " × ", ncol(df_train_x_log))

# Verify mode contrasts are in the design matrix
mode_in_matrix <- mode_contrast_cols %in% colnames(df_train_x_log)
if (all(mode_in_matrix)) {
  message("✓ All 3 mode contrasts included in LASSO: ", paste(mode_contrast_cols, collapse = ", "))
} else {
  warning("⚠ Missing mode contrasts: ", paste(mode_contrast_cols[!mode_in_matrix], collapse = ", "))
}

# Fit LASSO with cross-validation
message("\nFitting log-transformed LASSO with 10-fold CV...")
start_time_log <- Sys.time()

lasso_fit_log <- cv.glmnet(
  x = df_train_x_log,
  y = df_train_y_log,
  alpha = 1,  # LASSO (not ridge)
  nfolds = 10,
  family = gaussian(link = "identity"),
  nlambda = nlambda,
  keep = TRUE,
  type.measure = "deviance",
  intercept = TRUE,
  standardize = FALSE, # Already standardized
  maxit = maxit,
  parallel = TRUE  # Uses registered parallel backend
)

end_time_log <- Sys.time()
elapsed_time_log <- round(difftime(end_time_log, start_time_log, units = "mins"), 2)
message("✓ Log-transformed LASSO complete! Time: ", elapsed_time_log, " minutes")

# ==============================================================================
# 2b. NON-LOG LASSO
# ==============================================================================

message("\n\n=== Fitting NON-TRANSFORMED LASSO ===")
message("Outcome: perskm_hth (original scale)")

# Create model matrices for non-transformed outcome
df_train_x_nolog <- model.matrix(perskm_hth ~ . - perskm_hth_log, df_train)[, -1]
df_train_y_nolog <- df_train$perskm_hth

df_test_x_nolog <- model.matrix(perskm_hth ~ . - perskm_hth_log, df_test)[, -1]
df_test_y_nolog <- df_test$perskm_hth

message("Design matrix dimensions: ", nrow(df_train_x_nolog), " × ", ncol(df_train_x_nolog))

# Fit LASSO with cross-validation
message("\nFitting non-transformed LASSO with 10-fold CV...")
start_time_nolog <- Sys.time()

lasso_fit_nolog <- cv.glmnet(
  x = df_train_x_nolog,
  y = df_train_y_nolog,
  alpha = 1,  # LASSO (not ridge)
  nfolds = 10,
  family = gaussian(link = "identity"),
  nlambda = nlambda,
  keep = TRUE,
  type.measure = "deviance",
  intercept = TRUE,
  standardize = FALSE, # Already standardized
  maxit = maxit,
  parallel = TRUE  # Uses registered parallel backend
)

end_time_nolog <- Sys.time()
elapsed_time_nolog <- round(difftime(end_time_nolog, start_time_nolog, units = "mins"), 2)
message("✓ Non-transformed LASSO complete! Time: ", elapsed_time_nolog, " minutes")

message("\n=== Total LASSO fitting time: ", elapsed_time_log + elapsed_time_nolog, " minutes ===")

# ==============================================================================
# Extract selected variables for BOTH models
# ==============================================================================

message("\n=== Extracting selected variables (Log Model) ===")

# Extract variables for LOG model
index_min_lambda_log <- lasso_fit_log$index[1, ]
index_1SE_lambda_log <- lasso_fit_log$index[2, ]

message("Log model - Lambda min index: ", index_min_lambda_log)
message("Log model - Lambda 1SE index: ", index_1SE_lambda_log)

# Get coefficients at lambda.min and lambda.1se for LOG model
CoefsMin_log <- as.matrix(coef(lasso_fit_log$glmnet.fit))[, index_min_lambda_log]
Coefs1SE_log <- as.matrix(coef(lasso_fit_log$glmnet.fit))[, index_1SE_lambda_log]

lasso_overview_min_log <- as.data.frame(CoefsMin_log) %>%
  filter(. != 0)

lasso_overview_1SE_log <- as.data.frame(Coefs1SE_log) %>%
  filter(. != 0)

message("Log model - Variables selected (lambda.min): ", nrow(lasso_overview_min_log) - 1)
message("Log model - Variables selected (lambda.1se): ", nrow(lasso_overview_1SE_log) - 1)

# Get variable names for LOG model
lasso_vars_min_log <- lasso_overview_min_log %>%
  slice(2:nrow(lasso_overview_min_log)) %>%
  rownames() %>%
  as.character()

lasso_vars_1SE_log <- lasso_overview_1SE_log %>%
  slice(2:nrow(lasso_overview_1SE_log)) %>%
  rownames() %>%
  as.character()

message("\nLog model selected variables (lambda.min):")
message(paste(lasso_vars_min_log, collapse = "\n"))

message("\n=== Extracting selected variables (Non-Log Model) ===")

# Extract variables for NON-LOG model
index_min_lambda_nolog <- lasso_fit_nolog$index[1, ]
index_1SE_lambda_nolog <- lasso_fit_nolog$index[2, ]

message("Non-log model - Lambda min index: ", index_min_lambda_nolog)
message("Non-log model - Lambda 1SE index: ", index_1SE_lambda_nolog)

# Get coefficients at lambda.min and lambda.1se for NON-LOG model
CoefsMin_nolog <- as.matrix(coef(lasso_fit_nolog$glmnet.fit))[, index_min_lambda_nolog]
Coefs1SE_nolog <- as.matrix(coef(lasso_fit_nolog$glmnet.fit))[, index_1SE_lambda_nolog]

lasso_overview_min_nolog <- as.data.frame(CoefsMin_nolog) %>%
  filter(. != 0)

lasso_overview_1SE_nolog <- as.data.frame(Coefs1SE_nolog) %>%
  filter(. != 0)

message("Non-log model - Variables selected (lambda.min): ", nrow(lasso_overview_min_nolog) - 1)
message("Non-log model - Variables selected (lambda.1se): ", nrow(lasso_overview_1SE_nolog) - 1)

# Get variable names for NON-LOG model
lasso_vars_min_nolog <- lasso_overview_min_nolog %>%
  slice(2:nrow(lasso_overview_min_nolog)) %>%
  rownames() %>%
  as.character()

lasso_vars_1SE_nolog <- lasso_overview_1SE_nolog %>%
  slice(2:nrow(lasso_overview_1SE_nolog)) %>%
  rownames() %>%
  as.character()

message("\nNon-log model selected variables (lambda.min):")
message(paste(lasso_vars_min_nolog, collapse = "\n"))

# Store the log version as the default for backward compatibility
lasso_fit <- lasso_fit_log
df_train_x <- df_train_x_log
df_train_y <- df_train_y_log
df_test_x <- df_test_x_log
df_test_y <- df_test_y_log
lasso_vars_min <- lasso_vars_min_log
lasso_vars_1SE <- lasso_vars_1SE_log

# Diagnostic plots -------------------------------------------------------------

message("\n=== Creating diagnostic plots ===")

# Lambda vs deviance plot
lambda_deviance_plot <- ggplot(
  mapping = aes(
    x = log(lasso_fit$lambda),
    y = lasso_fit$cvm
  )
) +
  geom_errorbar(
    aes(
      ymin = lasso_fit$cvlo,
      ymax = lasso_fit$cvup,
      width = .02
    ),
    color = "grey"
  ) +
  geom_vline(
    xintercept = log(lasso_fit$lambda.min),
    color = viridis::viridis(1),
    linewidth = 1,
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = log(lasso_fit$lambda.1se),
    color = viridis::viridis(2)[2],
    linewidth = 1,
    linetype = "dashed"
  ) +
  geom_point(color = "red", size = 0.5) +
  xlab("Log Lambda") +
  ylab("CV Performance Measure (Deviance)") +
  ggtitle("LASSO Cross-Validation") +
  theme_minimal()

# Lambda range
MaxLambda <- paste0("Max Lambda: ", round(max(lasso_fit$lambda)))
MinLambda <- paste0("Min Lambda: ", round(min(lasso_fit$lambda)))
message(MaxLambda)
message(MinLambda)

# Coefficient path plot
beta <- coef(lasso_fit$glmnet.fit)
tmp <- as.data.frame(as.matrix(beta))
tmp$coef <- row.names(tmp)
tmp <- reshape::melt(tmp, id = "coef")
tmp$variable <- as.numeric(gsub("s", "", tmp$variable))
tmp$lambda <- lasso_fit$lambda[tmp$variable + 1]
tmp$norm <- apply(abs(beta[-1, ]), 2, sum)[tmp$variable + 1]

# Calculate reasonable y-axis limits (exclude extreme outliers)
tmp_no_int <- tmp[tmp$coef != "(Intercept)", ]
ylim_values <- quantile(tmp_no_int$value, c(0.05, 0.95), na.rm = TRUE)
ylim_range <- diff(ylim_values)
ylim_final <- c(ylim_values[1] - 0.1 * ylim_range, ylim_values[2] + 0.1 * ylim_range)

# plot_lambda_coef <- ggplot(
#   data = tmp_no_int,
#   aes(lambda, value, color = coef, linetype = coef)
# ) +
#   geom_line(alpha = 0.5) +
#   scale_x_log10() +
#   coord_cartesian(ylim = ylim_final) +  # Focus on 5th-95th percentile + 10% buffer
#   xlab("Lambda (log scale)") +
#   ylab("Coefficient Value") +
#   # guides(
#   #   color = guide_legend(title = ""),
#   #   linetype = guide_legend(title = "")
#   # ) +
#   guides(color = "none", linetype = "none") +  # Hide legend
#   geom_vline(
#     xintercept = lasso_fit$lambda.min,
#     color = viridis::viridis(1),
#     linewidth = 1,
#     linetype = "dashed"
#   ) +
#   geom_vline(
#     xintercept = lasso_fit$lambda.1se,
#     color = viridis::viridis(2)[2],
#     linewidth = 1,
#     linetype = "dashed"
#   ) +
#   theme_bw() +
#   labs(title = "LASSO Coefficient Paths")
#   # + theme(legend.key.width = unit(3, "lines"))  # Commented out with legend

# Model performance for BOTH models -------------------------------------------

message("\n=== Model performance on test set ===")

# Performance for LOG model
df_test_preds_log <- predict(
  lasso_fit_log,
  newx = df_test_x_log,
  s = lasso_fit_log$lambda.min,
  type = "response"
)

Performance_Measures_log <- data.frame(
  Model = "Log",
  RMSE = caret::RMSE(df_test_preds_log, df_test_y_log),
  R2 = cor(df_test_preds_log, df_test_y_log)^2,
  DevianceRatio = lasso_fit_log$glmnet.fit$dev.ratio[index_min_lambda_log]
)

message("Log model - RMSE: ", round(Performance_Measures_log$RMSE, 3))
message("Log model - R²: ", round(Performance_Measures_log$R2, 3))
message("Log model - Deviance Ratio: ", round(Performance_Measures_log$DevianceRatio, 3))

# Performance for NON-LOG model
df_test_preds_nolog <- predict(
  lasso_fit_nolog,
  newx = df_test_x_nolog,
  s = lasso_fit_nolog$lambda.min,
  type = "response"
)

Performance_Measures_nolog <- data.frame(
  Model = "Non-Log",
  RMSE = caret::RMSE(df_test_preds_nolog, df_test_y_nolog),
  R2 = cor(df_test_preds_nolog, df_test_y_nolog)^2,
  DevianceRatio = lasso_fit_nolog$glmnet.fit$dev.ratio[index_min_lambda_nolog]
)

message("Non-log model - RMSE: ", round(Performance_Measures_nolog$RMSE, 3))
message("Non-log model - R²: ", round(Performance_Measures_nolog$R2, 3))
message("Non-log model - Deviance Ratio: ", round(Performance_Measures_nolog$DevianceRatio, 3))

# Combine performance measures
Performance_Measures <- bind_rows(Performance_Measures_log, Performance_Measures_nolog)

message("\n=== Performance Comparison ===")
print(Performance_Measures)

# Store default for backward compatibility
df_test_preds <- df_test_preds_log

# Save results -----------------------------------------------------------------

message("\n=== Saving results ===")

# Prepare LOG model results (exactly as before for backward compatibility)
df_Selection_min <- lasso_overview_min_log
df_Selection_1SE <- lasso_overview_1SE_log

varlist <- df_Selection_min %>%
  slice(-1) %>%  # Remove intercept
  rownames() %>%
  as.character()

# Use log model performance for backward compatibility (single row, not combined)
Var_Selection <- list(
  lambda_min = df_Selection_min,
  lambda_1SE = df_Selection_1SE,
  varlist = varlist,
  performance = Performance_Measures_log,
  lasso_fit = lasso_fit_log
)

# Prepare NON-LOG model results
df_Selection_min_nolog <- lasso_overview_min_nolog
df_Selection_1SE_nolog <- lasso_overview_1SE_nolog

varlist_nolog <- df_Selection_min_nolog %>%
  slice(-1) %>%  # Remove intercept
  rownames() %>%
  as.character()

Var_Selection_nolog <- list(
  model_type = "non-log",
  lambda_min = df_Selection_min_nolog,
  lambda_1SE = df_Selection_1SE_nolog,
  varlist = varlist_nolog,
  performance = Performance_Measures_nolog,
  lasso_fit = lasso_fit_nolog,
  selected_vars_min = lasso_vars_min_nolog,
  selected_vars_1SE = lasso_vars_1SE_nolog
)

# Create output directory
output_dir <- file.path(data_path, "output/variables")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Save LOG model results (backward compatible format)
saveRDS(
  Var_Selection,
  file = file.path(output_dir, "phase1_lasso_results.rds")
)

message("Saved: ", file.path(output_dir, "phase1_lasso_results.rds"))

# Save NON-LOG model results
saveRDS(
  Var_Selection_nolog,
  file = file.path(output_dir, "phase1_lasso_results_nolog.rds")
)

message("Saved: ", file.path(output_dir, "phase1_lasso_results_nolog.rds"))

# Save LOG model results with explicit naming
Var_Selection_log <- Var_Selection
Var_Selection_log$model_type <- "log"
Var_Selection_log$selected_vars_min <- lasso_vars_min_log
Var_Selection_log$selected_vars_1SE <- lasso_vars_1SE_log

saveRDS(
  Var_Selection_log,
  file = file.path(output_dir, "phase1_lasso_results_log.rds")
)

message("Saved: ", file.path(output_dir, "phase1_lasso_results_log.rds"))

# Save combined results for easy comparison
Var_Selection_combined <- list(
  log = Var_Selection_log,
  nolog = Var_Selection_nolog,
  performance_comparison = Performance_Measures
)

saveRDS(
  Var_Selection_combined,
  file = file.path(output_dir, "phase1_lasso_results_combined.rds")
)

message("Saved: ", file.path(output_dir, "phase1_lasso_results_combined.rds"))

# ==============================================================================
# Save prepared data for downstream scripts (Phase 2 sparseR, fit_fh_models.r)
# ==============================================================================
# This data is needed by Phase 2 sparseR which runs BEFORE fit_fh_models.r
# The prepared data includes:
#   - Scaled/filtered covariate matrix
#   - Direct estimates
#   - Mode contrast columns
#   - Variable type classifications

message("\n=== Saving prepared data for downstream scripts ===")

# Create model data directory if needed
models_dir <- file.path(data_path, "output/models")
dir.create(models_dir, recursive = TRUE, showWarnings = FALSE)

# Save prepared data that downstream scripts need
# Prepared data object for downstream phases (Phase 2 and Phase 3)
phase1_prepared_data <- list(
  combined_data = df_combined_step1,       # Scaled/filtered data with all covariates
  direct_estimates = df_direct,             # Original direct estimates
  selected_vars_log = lasso_vars_min_log,   # Log model selected vars
  selected_vars_nolog = lasso_vars_min_nolog, # Non-log model selected vars
  id_cols = id_cols,                        # ID columns
  outcome_cols = outcome_cols,              # Outcome columns
  mode_contrast_cols = mode_contrast_cols,  # Mode contrast columns
  covar_cols = setdiff(names(df_combined_step1),
                       c(id_cols, outcome_cols, "hvm_factor")), # All covariate columns
  scale_cols = scale_cols,                  # Columns that were scaled
  mid_prop_cols = mid_prop_cols,            # MiD proportion columns (not scaled)
  timestamp = Sys.time()
)

saveRDS(
  phase1_prepared_data,
  file = file.path(models_dir, "phase1_prepared_data.rds")
)

message("Saved: ", file.path(models_dir, "phase1_prepared_data.rds"))
message("  This file provides prepared data for Phase 2 sparseR and fit_fh_models.r")

# # Save plots
# plot_dir <- file.path(data_path, "output/plots")
# dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

# ggsave(
#   file.path(plot_dir, "lasso_deviance_perskm.pdf"),
#   lambda_deviance_plot,
#   width = 8,
#   height = 6
# )

# ggsave(
#   file.path(plot_dir, "lasso_coef_paths_perskm.pdf"),
#   plot_lambda_coef,
#   width = 10,
#   height = 6
# )

# message("Saved diagnostic plots to: ", plot_dir)

# Clean up parallel cluster ----------------------------------------------------
stopCluster(cl)
message("\nParallel cluster stopped")

message("\n=== Variable selection complete! ===")
message("Log model: Selected ", length(lasso_vars_min_log), " variables")
message("Non-log model: Selected ", length(lasso_vars_min_nolog), " variables")
message("Results saved to data/output/variables/")
