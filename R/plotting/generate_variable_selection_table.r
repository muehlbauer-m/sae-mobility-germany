################################################################################
# Generate Variable Selection Results Tables
#
# Purpose: Create LaTeX tables summarizing the 3-phase variable selection:
#          1. Phase Pipeline Summary (tbl_varsel_pipeline.tex)
#          2. Cross-Validation Comparison (tbl_varsel_cv.tex)
#
# Updated: 2026-02-04 - Support for 5-candidate structure (baseline + maximal)
#
# Input:
#   - data/output/variables/phase1_lasso_results.rds
#   - data/output/variables/phase2_sparseR_results_baseline.rds
#   - data/output/variables/phase2_sparseR_results_maximal.rds
#   - data/output/models/phase3_cv_results.rds
#   - data/output/models/fh_model_final.rds
#
# Output:
#   - data/Tex/Tables/tbl_varsel_pipeline.tex
#   - data/Tex/Tables/tbl_varsel_cv.tex
#
# Created: 2026-01-23
################################################################################

# Load packages ---------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  stringr,
  moments  # For kurtosis/skewness
)

# Set paths -------------------------------------------------------------------
data_path <- "./data"

cat("\n==================================================\n")
cat("Generate Variable Selection Results Tables\n")
cat("==================================================\n\n")

# ==============================================================================
# STEP 1: Load all phase results
# ==============================================================================
cat("STEP 1: Loading phase results...\n")

# Phase 1: LASSO main effect selection
phase1 <- readRDS(file.path(data_path, "output/variables/phase1_lasso_results.rds"))
n_phase1_vars <- length(phase1$varlist)
phase1_r2 <- phase1$performance$R2
phase1_rmse <- phase1$performance$RMSE

cat("  Phase 1: ", n_phase1_vars, " base covariates selected\n", sep = "")
cat("           R² = ", round(phase1_r2, 3), ", RMSE = ", round(phase1_rmse, 3), "\n", sep = "")

# Phase 2: sparseR ranked sparsity LASSO - BASELINE configuration
phase2_baseline <- readRDS(file.path(data_path, "output/variables/phase2_sparseR_results_baseline.rds"))
n_base_covariates_baseline <- phase2_baseline$n_base_covariates
n_expanded_baseline <- phase2_baseline$n_expanded_terms
n_baseline_min <- length(phase2_baseline$selected_lambda_min)
n_baseline_1se <- length(phase2_baseline$selected_lambda_1se)

cats_baseline_min <- phase2_baseline$categories_min
n_main_baseline_min <- length(cats_baseline_min$main_effects)
n_poly_baseline_min <- length(cats_baseline_min$polynomials)
n_inter_baseline_min <- length(cats_baseline_min$interactions)

cats_baseline_1se <- phase2_baseline$categories_1se
n_main_baseline_1se <- length(cats_baseline_1se$main_effects)
n_poly_baseline_1se <- length(cats_baseline_1se$polynomials)
n_inter_baseline_1se <- length(cats_baseline_1se$interactions)

cat("  Phase 2 Baseline (poly2, mode×covar):\n")
cat("           ", n_expanded_baseline, " expanded terms from ", n_base_covariates_baseline, " base\n", sep = "")
cat("           λ_min: ", n_baseline_min, " vars (", n_main_baseline_min, " main, ", n_poly_baseline_min, " poly, ", n_inter_baseline_min, " inter)\n", sep = "")
cat("           λ_1se: ", n_baseline_1se, " vars (", n_main_baseline_1se, " main, ", n_poly_baseline_1se, " poly, ", n_inter_baseline_1se, " inter)\n", sep = "")

# Phase 2: sparseR ranked sparsity LASSO - MAXIMAL configuration
phase2_maximal <- readRDS(file.path(data_path, "output/variables/phase2_sparseR_results_maximal.rds"))
n_base_covariates_maximal <- phase2_maximal$n_base_covariates
n_expanded_maximal <- phase2_maximal$n_expanded_terms
n_maximal_min <- length(phase2_maximal$selected_lambda_min)
n_maximal_1se <- length(phase2_maximal$selected_lambda_1se)

cats_maximal_min <- phase2_maximal$categories_min
n_main_maximal_min <- length(cats_maximal_min$main_effects)
n_poly_maximal_min <- length(cats_maximal_min$polynomials)
n_inter_maximal_min <- length(cats_maximal_min$interactions)
# For maximal, interactions may include covar×covar - count separately if available
n_covar_covar_min <- if (!is.null(cats_maximal_min$covar_covar)) length(cats_maximal_min$covar_covar) else 0

cats_maximal_1se <- phase2_maximal$categories_1se
n_main_maximal_1se <- length(cats_maximal_1se$main_effects)
n_poly_maximal_1se <- length(cats_maximal_1se$polynomials)
n_inter_maximal_1se <- length(cats_maximal_1se$interactions)
n_covar_covar_1se <- if (!is.null(cats_maximal_1se$covar_covar)) length(cats_maximal_1se$covar_covar) else 0

cat("  Phase 2 Maximal (poly3, +covar×covar):\n")
cat("           ", n_expanded_maximal, " expanded terms from ", n_base_covariates_maximal, " base\n", sep = "")
cat("           λ_min: ", n_maximal_min, " vars (", n_main_maximal_min, " main, ", n_poly_maximal_min, " poly, ", n_inter_maximal_min, " inter)\n", sep = "")
cat("           λ_1se: ", n_maximal_1se, " vars (", n_main_maximal_1se, " main, ", n_poly_maximal_1se, " poly, ", n_inter_maximal_1se, " inter)\n", sep = "")

# Phase 3: Cross-validation results
phase3 <- readRDS(file.path(data_path, "output/models/phase3_cv_results.rds"))
cv_summary <- phase3$cv_summary
winner <- phase3$winner
k_folds <- phase3$config$k_folds
cv_threshold <- phase3$config$cv_threshold

cat("  Phase 3: K = ", k_folds, " folds, CV threshold = ", cv_threshold * 100, "%\n", sep = "")
cat("           Winner: ", winner, "\n", sep = "")

# Final model diagnostics
final_model <- readRDS(file.path(data_path, "output/models/fh_model_final.rds"))
sigma_u_sq <- final_model$model$variance
re <- final_model$model$random_effects
final_kurtosis <- kurtosis(re)
final_skewness <- skewness(re)

cat("  Final model: σ²_u = ", round(sigma_u_sq, 4), ", kurtosis = ", round(final_kurtosis, 2), "\n\n", sep = "")

# ==============================================================================
# STEP 2: Generate Table 1 - Phase Pipeline Summary
# ==============================================================================
cat("STEP 2: Generating Phase Pipeline Summary table...\n")

# Build the pipeline table content
n_base_candidate <- n_phase1_vars  # mode contrasts already included in phase1 count

latex_pipeline <- c(
  "\\begin{table}[ht]",
  "\\centering",
  "\\small",
  "\\begin{tabularx}{\\textwidth}{lXXX}",
  "\\hline",
  "\\textbf{Phase} & \\textbf{Description} & \\textbf{Input} & \\textbf{Output} \\\\",
  "\\hline",
  "\\hline",
  paste0("1 & LASSO main effects & 1,095 candidates & ", n_phase1_vars, " covariates (R$^2$ = ", sprintf("%.2f", phase1_r2), ") \\\\[2pt]"),
  paste0("2a & sparseR baseline & ", format(n_expanded_baseline, big.mark = ","), " terms (poly$^2$, mode$\\times$covar) & $\\lambda_{\\min}$: ", n_baseline_min, " vars, $\\lambda_{1\\text{se}}$: ", n_baseline_1se, " vars \\\\[2pt]"),
  paste0("2b & sparseR maximal & ", format(n_expanded_maximal, big.mark = ","), " terms (+poly$^3$, +covar$\\times$covar) & $\\lambda_{\\min}$: ", n_maximal_min, " vars, $\\lambda_{1\\text{se}}$: ", n_maximal_1se, " vars \\\\[2pt]"),
  paste0("3 & FH cross-validation & 5 candidate sets (K=", k_folds, ") & Winner: baseline $\\lambda_{\\min}$ \\\\"),
  "\\hline",
  "\\hline",
  "\\end{tabularx}",
  paste0("\\caption{Three-phase variable selection pipeline. Phase 1 reduces candidates via LASSO on main effects. ",
         "Phase 2 applies ranked sparsity LASSO (sparseR) with two configurations: ",
         "(a) baseline with quadratic polynomials and mode$\\times$covariate interactions; ",
         "(b) maximal adding cubic polynomials and covariate$\\times$covariate interactions. ",
         "Phase 3 selects the final model via ", k_folds, "-fold cross-validation on the Fay-Herriot model.}"),
  "\\label{tbl:varsel_pipeline}",
  "\\end{table}"
)

cat("  Table 1 generated with ", length(latex_pipeline), " lines\n", sep = "")

# ==============================================================================
# STEP 3: Generate Table 2 - Cross-Validation Comparison
# ==============================================================================
cat("STEP 3: Generating Cross-Validation Comparison table...\n")

# Get SSE values from cv_summary (reliable_sse column)
format_sse <- function(x) format(round(x, 0), big.mark = ",", scientific = FALSE)

# Extract SSE for each candidate
sse_base <- cv_summary %>% filter(candidate == "base") %>% pull(reliable_sse)
sse_baseline_min <- cv_summary %>% filter(candidate == "baseline_min") %>% pull(reliable_sse)
sse_baseline_1se <- cv_summary %>% filter(candidate == "baseline_1se") %>% pull(reliable_sse)
sse_maximal_min <- cv_summary %>% filter(candidate == "maximal_min") %>% pull(reliable_sse)
sse_maximal_1se <- cv_summary %>% filter(candidate == "maximal_1se") %>% pull(reliable_sse)

# Identify winner
mark_winner <- function(sse, cand) {
  if (cand == winner) paste0("\\textbf{", format_sse(sse), "}") else format_sse(sse)
}

latex_cv <- c(
  "\\begin{table}[ht]",
  "\\centering",
  "\\small",
  "\\begin{tabularx}{\\textwidth}{llXXXXX}",
  "\\hline",
  "\\textbf{Config} & \\textbf{Candidate} & \\textbf{Variables} & \\textbf{Main} & \\textbf{Poly} & \\textbf{Inter} & \\textbf{SSE} \\\\",
  "\\hline",
  "\\hline",
  paste0("-- & Base & ", n_base_candidate, " & ", n_base_candidate, " & 0 & 0 & ", mark_winner(sse_base, "base"), " \\\\[2pt]"),
  paste0("Baseline & $\\lambda_{1\\text{se}}$ & ", n_baseline_1se, " & ", n_main_baseline_1se, " & ", n_poly_baseline_1se, " & ", n_inter_baseline_1se, " & ", mark_winner(sse_baseline_1se, "baseline_1se"), " \\\\"),
  paste0("Baseline & $\\lambda_{\\min}$ & ", n_baseline_min, " & ", n_main_baseline_min, " & ", n_poly_baseline_min, " & ", n_inter_baseline_min, " & ", mark_winner(sse_baseline_min, "baseline_min"), " \\\\[2pt]"),
  paste0("Maximal & $\\lambda_{1\\text{se}}$ & ", n_maximal_1se, " & ", n_main_maximal_1se, " & ", n_poly_maximal_1se, " & ", n_inter_maximal_1se, " & ", mark_winner(sse_maximal_1se, "maximal_1se"), " \\\\"),
  paste0("Maximal & $\\lambda_{\\min}$ & ", n_maximal_min, " & ", n_main_maximal_min, " & ", n_poly_maximal_min, " & ", n_inter_maximal_min, " & ", mark_winner(sse_maximal_min, "maximal_min"), " \\\\"),
  "\\hline",
  "\\hline",
  "\\end{tabularx}",
  paste0("\\caption{Cross-validation comparison of five candidate variable sets. ",
         "SSE calculated on reliable domains (CV $<$ ", cv_threshold * 100, "\\%). ",
         "Baseline configuration uses quadratic polynomials and mode$\\times$covariate interactions only. ",
         "Maximal configuration adds cubic polynomials and covariate$\\times$covariate interactions. ",
         "Winner (baseline $\\lambda_{\\min}$) shown in bold.}"),
  "\\label{tbl:varsel_cv}",
  "\\end{table}"
)

cat("  Table 2 generated with ", length(latex_cv), " lines\n\n", sep = "")

# ==============================================================================
# STEP 4: Save output files
# ==============================================================================
cat("STEP 4: Saving LaTeX tables...\n")

output_dir <- "./manuscript"

# Save Table 1
output_file1 <- file.path(output_dir, "tbl_varsel_pipeline.tex")
writeLines(latex_pipeline, output_file1)
cat("  Saved: ", output_file1, "\n", sep = "")

# Save Table 2
output_file2 <- file.path(output_dir, "tbl_varsel_cv.tex")
writeLines(latex_cv, output_file2)
cat("  Saved: ", output_file2, "\n\n", sep = "")

# ==============================================================================
# Summary
# ==============================================================================
cat("==================================================\n")
cat("Variable Selection Tables Generated\n")
cat("==================================================\n\n")

cat("Summary statistics:\n")
cat("  Phase 1: ", n_phase1_vars, " base covariates (from 1,095 candidates)\n", sep = "")
cat("\n  Baseline configuration (poly2, mode×covar):\n")
cat("    λ_min: ", n_baseline_min, " vars (", n_main_baseline_min, " main, ", n_poly_baseline_min, " poly, ", n_inter_baseline_min, " inter)\n", sep = "")
cat("    λ_1se: ", n_baseline_1se, " vars (", n_main_baseline_1se, " main, ", n_poly_baseline_1se, " poly, ", n_inter_baseline_1se, " inter)\n", sep = "")
cat("\n  Maximal configuration (poly3, +covar×covar):\n")
cat("    λ_min: ", n_maximal_min, " vars (", n_main_maximal_min, " main, ", n_poly_maximal_min, " poly, ", n_inter_maximal_min, " inter)\n", sep = "")
cat("    λ_1se: ", n_maximal_1se, " vars (", n_main_maximal_1se, " main, ", n_poly_maximal_1se, " poly, ", n_inter_maximal_1se, " inter)\n", sep = "")
cat("\n  Phase 3 winner: ", winner, "\n", sep = "")
cat("  Final model σ²_u: ", round(sigma_u_sq, 4), "\n", sep = "")
cat("  Final model kurtosis: ", round(final_kurtosis, 2), "\n\n", sep = "")

cat("Output files:\n")
cat("  1. ", output_file1, "\n", sep = "")
cat("  2. ", output_file2, "\n\n", sep = "")

cat("To include in manuscript, add:\n")
cat("  \\input{../data/Tex/Tables/tbl_varsel_pipeline}\n")
cat("  \\input{../data/Tex/Tables/tbl_varsel_cv}\n\n")

################################################################################
# END OF SCRIPT
################################################################################
