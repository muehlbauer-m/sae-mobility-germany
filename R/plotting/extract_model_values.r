# Extract all key numerical values from current model outputs
# Run from MiD_ANALYSIS directory
#
# Purpose: Generate reference values for manuscript verification after
#          removing MiD covariates from the model.
#
# Output: Tibble with models as columns and diagnostics as rows
#
# Created: 2026-02-05

library(tidyverse)
library(emdi)

data_path <- "./data"

cat("\n=======================================================\n")
cat("CURRENT MODEL VALUES (without MiD covariates)\n")
cat("=======================================================\n\n")

# Function to extract all diagnostics from an emdi fh model
extract_diagnostics <- function(model_path, model_name) {
  m <- tryCatch(readRDS(model_path), error = function(e) NULL)

  if (is.null(m)) {
    cat("  [MISSING]", model_name, "\n")
    return(NULL)
  }

  cat("  [OK]", model_name, "\n")

  # Get emdi summary for normality diagnostics
  s <- summary(m)

  # Extract components
  re <- as.vector(m$model$random_effects)
  resid <- m$model$real_residuals
  gamma_vals <- m$model$gamma$Gamma
  mse_fh <- m$MSE$FH
  mse_direct <- m$MSE$Direct

  # Model fit diagnostics from model_select
  model_select <- m$model$model_select

  # Normality diagnostics from emdi summary
  norm <- s$normality

  # Build diagnostics tibble
  tibble(
    diagnostic = c(
      # Model specification
      "n_coefficients",
      "n_domains",
      # Variance components
      "sigma_u_sq",
      # Model fit (from model_select)
      "loglike",
      "AIC",
      "BIC",
      "AdjR2",
      "FH_R2",
      # Random effects diagnostics (from emdi summary)
      "re_skewness",
      "re_kurtosis",
      "re_shapiro_W",
      "re_shapiro_p",
      # Residuals diagnostics (from emdi summary)
      "resid_skewness",
      "resid_kurtosis",
      "resid_shapiro_W",
      "resid_shapiro_p",
      # Random effects distribution
      "re_mean",
      "re_sd",
      # Gamma (shrinkage) statistics
      "gamma_mean",
      "gamma_median",
      "gamma_min",
      "gamma_max",
      "gamma_sd",
      # MSE statistics (FH)
      "mse_fh_mean",
      "mse_fh_median",
      "mse_fh_min",
      "mse_fh_max",
      # MSE statistics (Direct) - for comparison
      "mse_direct_mean",
      "mse_direct_median"
    ),
    !!model_name := c(
      # Model specification
      nrow(m$model$coefficients),
      length(re),
      # Variance components
      m$model$variance,
      # Model fit
      model_select$loglike,
      model_select$AIC,
      model_select$BIC,
      model_select$AdjR2,
      model_select$FH_R2,
      # Random effects (from emdi)
      norm["Random_effects", "Skewness"],
      norm["Random_effects", "Kurtosis"],
      norm["Random_effects", "Shapiro_W"],
      norm["Random_effects", "Shapiro_p"],
      # Residuals (from emdi)
      norm["Standardized_Residuals", "Skewness"],
      norm["Standardized_Residuals", "Kurtosis"],
      norm["Standardized_Residuals", "Shapiro_W"],
      norm["Standardized_Residuals", "Shapiro_p"],
      # Random effects distribution
      mean(re),
      sd(re),
      # Gamma
      mean(gamma_vals, na.rm = TRUE),
      median(gamma_vals, na.rm = TRUE),
      min(gamma_vals, na.rm = TRUE),
      max(gamma_vals, na.rm = TRUE),
      sd(gamma_vals, na.rm = TRUE),
      # MSE FH
      mean(mse_fh, na.rm = TRUE),
      median(mse_fh, na.rm = TRUE),
      min(mse_fh, na.rm = TRUE),
      max(mse_fh, na.rm = TRUE),
      # MSE Direct
      mean(mse_direct, na.rm = TRUE),
      median(mse_direct, na.rm = TRUE)
    )
  )
}

# Define model paths
model_files <- list(
  base_nolog = file.path(data_path, "output/models/fh_model_base_nolog.rds"),
  base = file.path(data_path, "output/models/fh_model_base.rds"),
  baseline_min = file.path(data_path, "output/models/fh_model_baseline_min.rds"),
  baseline_1se = file.path(data_path, "output/models/fh_model_baseline_1se.rds"),
  maximal_min = file.path(data_path, "output/models/fh_model_maximal_min.rds"),
  maximal_1se = file.path(data_path, "output/models/fh_model_maximal_1se.rds")
)

cat("Loading models...\n")

# Extract diagnostics for each model
diagnostics_list <- imap(model_files, extract_diagnostics)

# Remove NULLs (missing models)
diagnostics_list <- compact(diagnostics_list)
1
# Combine into single tibble
if (length(diagnostics_list) > 0) {
  model_diagnostics <- reduce(diagnostics_list, full_join, by = "diagnostic")
} else {
  stop("No models found!")
}

# Print formatted output
cat("\n=======================================================\n")
cat("Model Diagnostics Summary\n")
cat("=======================================================\n\n")

# Round for display
model_diagnostics_display <- model_diagnostics %>%
  mutate(across(where(is.numeric), ~ round(., 4)))

print(model_diagnostics_display, n = Inf, width = Inf)

# Save as RDS
output_file <- file.path(data_path, "output/models/model_diagnostics_summary.rds")
saveRDS(model_diagnostics, output_file)
cat("\n\nSaved to:", output_file, "\n")

# Also save as CSV for easy viewing
csv_file <- file.path(data_path, "output/models/model_diagnostics_summary.csv")
write_csv(model_diagnostics_display, csv_file)
cat("Saved to:", csv_file, "\n")

cat("\n=======================================================\n")
cat("END\n")
cat("=======================================================\n")
