# ==============================================================================
# Generate Diagnostic Tables for Manuscript
# ==============================================================================
# Purpose: Generate reproducible LaTeX tables for model diagnostics
#          1. tbl_log_vs_nolog.tex - Log vs Non-log comparison (justifies log transformation)
#          2. tbl_model_comparison.tex - 5-model comparison (Base + Baseline + Maximal)
#
# Author: Michael Mühlbauer
# Date: 2026-01-26
# Updated: 2026-02-04 - Support for 5-candidate structure (baseline + maximal)
#
# Input:
#   - data/output/models/fh_model_base.rds (log-transformed base model)
#   - data/output/models/fh_model_base_nolog.rds (non-log base model)
#   - data/output/models/fh_model_baseline_min.rds (baseline lambda.min model)
#   - data/output/models/fh_model_baseline_1se.rds (baseline lambda.1se model)
#   - data/output/models/fh_model_maximal_min.rds (maximal lambda.min model)
#   - data/output/models/fh_model_maximal_1se.rds (maximal lambda.1se model)
#   - data/output/models/phase3_cv_results.rds (for CV winner indicator)
#
# Output:
#   - data/Tex/Tables/tbl_log_vs_nolog.tex
#   - data/Tex/Tables/tbl_model_comparison.tex
#
# ==============================================================================

rm(list = ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, knitr, kableExtra, emdi)

data_path <- "./data"

output_dir <- "./manuscript"

message("\n", strrep("=", 70))
message("  Generate Diagnostic Tables for Manuscript")
message(strrep("=", 70), "\n")

# ==============================================================================
# Helper Function: Extract Diagnostics from FH Model using summary()
# ==============================================================================

extract_diagnostics <- function(model, name) {
  # Access model internals directly (emdi::fh object structure)
  m <- model$model

  # Number of variables (excluding intercept)
  n_vars <- nrow(m$coefficients) - 1

  # Domain variance
  sigma2_u <- m$variance

  # R² values from model_select
  adj_r2 <- m$model_select$AdjR2
  fh_r2 <- m$model_select$FH_R2

  # Model fit statistics
  loglike <- m$model_select$loglike
  aic <- m$model_select$AIC
  bic <- m$model_select$BIC

  # Transformation info
  transformation <- model$transformation

  # Use emdi's summary() for normality diagnostics (consistent with extract_model_values.r)
  s <- summary(model)
  norm <- s$normality

  skew_res <- norm["Standardized_Residuals", "Skewness"]
  kurt_res <- norm["Standardized_Residuals", "Kurtosis"]
  shapiro_w_res <- norm["Standardized_Residuals", "Shapiro_W"]
  shapiro_p_res <- norm["Standardized_Residuals", "Shapiro_p"]

  skew_re <- norm["Random_effects", "Skewness"]
  kurt_re <- norm["Random_effects", "Kurtosis"]
  shapiro_w_re <- norm["Random_effects", "Shapiro_W"]
  shapiro_p_re <- norm["Random_effects", "Shapiro_p"]

  message("  ", name, ": ", n_vars, " vars, AdjR2 = ", round(adj_r2 * 100, 1), "%, ",
          "FH_R2 = ", round(fh_r2 * 100, 1), "%, ",
          "Kurtosis(RE) = ", round(kurt_re, 2))

  list(
    name = name,
    n_vars = n_vars,
    sigma2_u = sigma2_u,
    adj_r2 = adj_r2,
    fh_r2 = fh_r2,
    loglike = loglike,
    aic = aic,
    bic = bic,
    transformation = transformation,
    skew_res = skew_res,
    skew_re = skew_re,
    kurt_res = kurt_res,
    kurt_re = kurt_re,
    shapiro_w_res = shapiro_w_res,
    shapiro_p_res = shapiro_p_res,
    shapiro_w_re = shapiro_w_re,
    shapiro_p_re = shapiro_p_re
  )
}

# ==============================================================================
# Load Models
# ==============================================================================

message("=== Loading models ===\n")

# Check which models exist (updated for 5-candidate structure)
models_to_load <- list(
  base = file.path(data_path, "output/models/fh_model_base.rds"),
  base_nolog = file.path(data_path, "output/models/fh_model_base_nolog.rds"),
  baseline_min = file.path(data_path, "output/models/fh_model_baseline_min.rds"),
  baseline_1se = file.path(data_path, "output/models/fh_model_baseline_1se.rds"),
  maximal_min = file.path(data_path, "output/models/fh_model_maximal_min.rds"),
  maximal_1se = file.path(data_path, "output/models/fh_model_maximal_1se.rds")
)

models <- list()
diag <- list()

for (name in names(models_to_load)) {
  path <- models_to_load[[name]]
  if (file.exists(path)) {
    models[[name]] <- readRDS(path)
    diag[[name]] <- extract_diagnostics(models[[name]], name)
    message("Loaded: ", basename(path), " (", diag[[name]]$n_vars, " vars)")
  } else {
    message("Not found: ", basename(path))
  }
}

# Load CV results for winner indicator
cv_results_path <- file.path(data_path, "output/models/phase3_cv_results.rds")
cv_winner <- "baseline_min"  # default
if (file.exists(cv_results_path)) {
  cv_results <- readRDS(cv_results_path)
  cv_winner <- cv_results$winner
  message("\nCV Winner: ", cv_winner)
}

# ==============================================================================
# Table 1: Log vs Non-Log Comparison
# ==============================================================================

message("\n=== Generating Table 1: Log vs Non-Log Comparison ===\n")

if (!is.null(diag$base) && !is.null(diag$base_nolog)) {

  # Build comparison dataframe
  tbl_log_vs_nolog <- data.frame(
    Statistic = c(
      "Variables",
      "$R^2$ (adjusted)",
      "$\\hat{\\sigma}^2_u$ (domain variance)",
      "Skewness (Std. Residuals)",
      "Skewness (Std. Random Effects)",
      "Kurtosis (Std. Residuals)",
      "Kurtosis (Std. Random Effects)",
      "Shapiro-Wilk $p$ (Residuals)",
      "Shapiro-Wilk $p$ (Random Effects)"
    ),
    Log = c(
      diag$base$n_vars,
      if (!is.na(diag$base$adj_r2)) sprintf("%.1f\\%%", diag$base$adj_r2 * 100) else "---",
      sprintf("%.4f", diag$base$sigma2_u),
      sprintf("%.4f", diag$base$skew_res),
      sprintf("%.4f", diag$base$skew_re),
      sprintf("%.2f", diag$base$kurt_res),
      sprintf("%.2f", diag$base$kurt_re),
      sprintf("%.2e", diag$base$shapiro_p_res),
      sprintf("%.2e", diag$base$shapiro_p_re)
    ),
    NonLog = c(
      diag$base_nolog$n_vars,
      if (!is.na(diag$base_nolog$adj_r2)) sprintf("%.1f\\%%", diag$base_nolog$adj_r2 * 100) else "---",
      sprintf("%.4f", diag$base_nolog$sigma2_u),
      sprintf("%.4f", diag$base_nolog$skew_res),
      sprintf("%.4f", diag$base_nolog$skew_re),
      sprintf("%.2f", diag$base_nolog$kurt_res),
      sprintf("%.2f", diag$base_nolog$kurt_re),
      sprintf("%.2e", diag$base_nolog$shapiro_p_res),
      sprintf("%.2e", diag$base_nolog$shapiro_p_re)
    )
  )

  # Generate LaTeX table (using tabularx for full width)
  latex_log_vs_nolog <- paste0(
    "\\begin{table}[ht]\n",
    "\\centering\n",
    "\\begin{tabularx}{\\textwidth}{lXX}\n",
    "\\hline\n",
    " & \\textbf{Base (log-transformed)} & \\textbf{Standard} \\\\\n",
    " & (", diag$base$n_vars, " vars) & (", diag$base_nolog$n_vars, " vars) \\\\\n",
    "\\hline\n",
    "\\hline\n"
  )

  for (i in 1:nrow(tbl_log_vs_nolog)) {
    latex_log_vs_nolog <- paste0(
      latex_log_vs_nolog,
      tbl_log_vs_nolog$Statistic[i], " & ",
      tbl_log_vs_nolog$Log[i], " & ",
      tbl_log_vs_nolog$NonLog[i], " \\\\\n"
    )
  }

  latex_log_vs_nolog <- paste0(
    latex_log_vs_nolog,
    "\\hline\n",
    "\\hline\n",
    "\\end{tabularx}\n",
    "\\caption{Comparison of distributional statistics between log-transformed and standard \\ac{FH} models. ",
    "Both models use Phase 1 LASSO-selected variables.}\n",
    "\\label{tbl:log_vs_nolog}\n",
    "\\end{table}\n"
  )

  # Write to file
  writeLines(latex_log_vs_nolog, file.path(output_dir, "tbl_log_vs_nolog.tex"))
  message("Saved: tbl_log_vs_nolog.tex")

  # Print comparison
  message("\nLog vs Non-Log Comparison:")
  message("  Log kurtosis (RE):     ", round(diag$base$kurt_re, 2))
  message("  Non-log kurtosis (RE): ", round(diag$base_nolog$kurt_re, 2))
  if (diag$base$kurt_re < diag$base_nolog$kurt_re) {
    message("  -> Log transformation IMPROVES normality")
  }

} else {
  message("Skipping Table 1: Missing base or base_nolog model")
}

# ==============================================================================
# Table 2: 5-Model Comparison (Log-Transformed Models)
# ==============================================================================

message("\n=== Generating Table 2: 5-Model Comparison ===\n")

# Check which models are available for the comparison
available_models <- c("base", "baseline_min", "baseline_1se", "maximal_min", "maximal_1se")
available_models <- available_models[sapply(available_models, function(x) !is.null(diag[[x]]))]

if (length(available_models) >= 3) {

  # Helper to format values
  fmt_val <- function(d, field, fmt = "%.4f") {
    if (is.null(d)) return("---")
    val <- d[[field]]
    if (is.null(val) || is.na(val)) return("---")
    sprintf(fmt, val)
  }

  fmt_pct <- function(d, field) {
    if (is.null(d)) return("---")
    val <- d[[field]]
    if (is.null(val) || is.na(val)) return("---")
    sprintf("%.1f\\%%", val * 100)
  }

  # Mark CV winner
  mark_winner <- function(cand) {
    if (cand == cv_winner) "\\ding{51}" else "\\ding{55}"
  }

  # Build statistics rows
  stats <- c(
    "Variables",
    "$R^2$ (adjusted)",
    "$\\hat{\\sigma}^2_u$",
    "Skewness (RE)",
    "Kurtosis (RE)",
    "Shapiro-Wilk $p$ (RE)",
    "CV Winner"
  )

  # Build the table manually for flexibility
  latex_model_comparison <- paste0(
    "\\begin{table}[ht]\n",
    "\\centering\n",
    "\\small\n",
    "\\begin{tabularx}{\\textwidth}{lXXXXX}\n",
    "\\hline\n",
    " & \\textbf{Base} & \\multicolumn{2}{c}{\\textbf{Baseline}} & \\multicolumn{2}{c}{\\textbf{Maximal}} \\\\\n",
    " & & $\\boldsymbol{\\lambda_{\\min}}$ & $\\boldsymbol{\\lambda_{1\\text{se}}}$ & $\\boldsymbol{\\lambda_{\\min}}$ & $\\boldsymbol{\\lambda_{1\\text{se}}}$ \\\\\n",
    "\\hline\n",
    "\\hline\n"
  )

  # Add rows
  # Row 1: Variables
  latex_model_comparison <- paste0(
    latex_model_comparison,
    "Variables & ",
    if (!is.null(diag$base)) diag$base$n_vars else "---", " & ",
    if (!is.null(diag$baseline_min)) diag$baseline_min$n_vars else "---", " & ",
    if (!is.null(diag$baseline_1se)) diag$baseline_1se$n_vars else "---", " & ",
    if (!is.null(diag$maximal_min)) diag$maximal_min$n_vars else "---", " & ",
    if (!is.null(diag$maximal_1se)) diag$maximal_1se$n_vars else "---", " \\\\\n"
  )

  # Row 2: R² (adjusted)
  latex_model_comparison <- paste0(
    latex_model_comparison,
    "$R^2$ (adj.) & ",
    fmt_pct(diag$base, "adj_r2"), " & ",
    fmt_pct(diag$baseline_min, "adj_r2"), " & ",
    fmt_pct(diag$baseline_1se, "adj_r2"), " & ",
    fmt_pct(diag$maximal_min, "adj_r2"), " & ",
    fmt_pct(diag$maximal_1se, "adj_r2"), " \\\\\n"
  )

  # Row 3: sigma2_u
  latex_model_comparison <- paste0(
    latex_model_comparison,
    "$\\hat{\\sigma}^2_u$ & ",
    fmt_val(diag$base, "sigma2_u"), " & ",
    fmt_val(diag$baseline_min, "sigma2_u"), " & ",
    fmt_val(diag$baseline_1se, "sigma2_u"), " & ",
    fmt_val(diag$maximal_min, "sigma2_u"), " & ",
    fmt_val(diag$maximal_1se, "sigma2_u"), " \\\\\n"
  )

  # Row 4: Skewness (Residuals)
  latex_model_comparison <- paste0(
    latex_model_comparison,
    "Skewness (Res.) & ",
    fmt_val(diag$base, "skew_res"), " & ",
    fmt_val(diag$baseline_min, "skew_res"), " & ",
    fmt_val(diag$baseline_1se, "skew_res"), " & ",
    fmt_val(diag$maximal_min, "skew_res"), " & ",
    fmt_val(diag$maximal_1se, "skew_res"), " \\\\\n"
  )

  # Row 5: Skewness (RE)
  latex_model_comparison <- paste0(
    latex_model_comparison,
    "Skewness (RE) & ",
    fmt_val(diag$base, "skew_re"), " & ",
    fmt_val(diag$baseline_min, "skew_re"), " & ",
    fmt_val(diag$baseline_1se, "skew_re"), " & ",
    fmt_val(diag$maximal_min, "skew_re"), " & ",
    fmt_val(diag$maximal_1se, "skew_re"), " \\\\\n"
  )

  # Row 6: Kurtosis (Residuals)
  latex_model_comparison <- paste0(
    latex_model_comparison,
    "Kurtosis (Res.) & ",
    fmt_val(diag$base, "kurt_res", "%.2f"), " & ",
    fmt_val(diag$baseline_min, "kurt_res", "%.2f"), " & ",
    fmt_val(diag$baseline_1se, "kurt_res", "%.2f"), " & ",
    fmt_val(diag$maximal_min, "kurt_res", "%.2f"), " & ",
    fmt_val(diag$maximal_1se, "kurt_res", "%.2f"), " \\\\\n"
  )

  # Row 7: Kurtosis (RE)
  latex_model_comparison <- paste0(
    latex_model_comparison,
    "Kurtosis (RE) & ",
    fmt_val(diag$base, "kurt_re", "%.2f"), " & ",
    fmt_val(diag$baseline_min, "kurt_re", "%.2f"), " & ",
    fmt_val(diag$baseline_1se, "kurt_re", "%.2f"), " & ",
    fmt_val(diag$maximal_min, "kurt_re", "%.2f"), " & ",
    fmt_val(diag$maximal_1se, "kurt_re", "%.2f"), " \\\\\n"
  )

  # Row 8: Shapiro-Wilk p (Residuals)
  latex_model_comparison <- paste0(
    latex_model_comparison,
    "Shapiro-Wilk $p$ (Res.) & ",
    fmt_val(diag$base, "shapiro_p_res", "%.2e"), " & ",
    fmt_val(diag$baseline_min, "shapiro_p_res", "%.2e"), " & ",
    fmt_val(diag$baseline_1se, "shapiro_p_res", "%.2e"), " & ",
    fmt_val(diag$maximal_min, "shapiro_p_res", "%.2e"), " & ",
    fmt_val(diag$maximal_1se, "shapiro_p_res", "%.2e"), " \\\\\n"
  )

  # Row 9: Shapiro-Wilk p (RE)
  latex_model_comparison <- paste0(
    latex_model_comparison,
    "Shapiro-Wilk $p$ (RE) & ",
    fmt_val(diag$base, "shapiro_p_re", "%.2e"), " & ",
    fmt_val(diag$baseline_min, "shapiro_p_re", "%.2e"), " & ",
    fmt_val(diag$baseline_1se, "shapiro_p_re", "%.2e"), " & ",
    fmt_val(diag$maximal_min, "shapiro_p_re", "%.2e"), " & ",
    fmt_val(diag$maximal_1se, "shapiro_p_re", "%.2e"), " \\\\\n"
  )

  # Row 10: CV Winner
  latex_model_comparison <- paste0(
    latex_model_comparison,
    "CV Winner & ",
    mark_winner("base"), " & ",
    mark_winner("baseline_min"), " & ",
    mark_winner("baseline_1se"), " & ",
    mark_winner("maximal_min"), " & ",
    mark_winner("maximal_1se"), " \\\\\n"
  )

  latex_model_comparison <- paste0(
    latex_model_comparison,
    "\\hline\n",
    "\\hline\n",
    "\\end{tabularx}\n",
    "\\caption{Comparison of distributional statistics across five candidate variable sets. ",
    "Baseline configuration uses quadratic polynomials and mode$\\times$covariate interactions. ",
    "Maximal configuration adds cubic polynomials and covariate$\\times$covariate interactions. ",
    "All models are log-transformed. ",
    "The winning model (baseline $\\lambda_{\\min}$) was selected via 10-fold cross-validation based on ",
    "lowest SSE on reliable domains (CV $< 20\\%$). ",
    "Target values: kurtosis = 3.0, skewness = 0.0.}\n",
    "\\label{tbl:model_comparison}\n",
    "\\end{table}\n"
  )

  # Write to file
  writeLines(latex_model_comparison, file.path(output_dir, "tbl_model_comparison.tex"))
  message("Saved: tbl_model_comparison.tex")

  # Print summary
  message("\n5-Model Comparison:")
  for (m in available_models) {
    message("  ", m, ": ", diag[[m]]$n_vars, " vars, kurtosis = ", round(diag[[m]]$kurt_re, 2))
  }
  message("  CV Winner: ", cv_winner)

} else {
  message("Skipping Table 2: Need at least 3 models for comparison")
}

# ==============================================================================
# Summary
# ==============================================================================

message("\n", strrep("=", 70))
message("  Table Generation Complete!")
message(strrep("=", 70), "\n")

message("Generated tables:")
if (file.exists(file.path(output_dir, "tbl_log_vs_nolog.tex"))) {
  message("  - tbl_log_vs_nolog.tex (log vs non-log transformation comparison)")
}
if (file.exists(file.path(output_dir, "tbl_model_comparison.tex"))) {
  message("  - tbl_model_comparison.tex (5-model comparison)")
}

message("\nUse in manuscript:")
message("  \\input{data/Tex/Tables/tbl_log_vs_nolog.tex}")
message("  \\input{data/Tex/Tables/tbl_model_comparison.tex}")
