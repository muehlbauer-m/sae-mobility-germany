#### District-level FH Model Diagnostics Visualization ####
# ==============================================================================
# Purpose: Create diagnostic plots for district-level FH model
# Generates:
#   1. Fig 6: Log vs Non-Log comparison (justifies log transformation)
#   2. Fig 7: Normality diagnostics for log-transformed model (final model)
#   3. Comparative diagnostics: Base vs Extended model
#
# Updated: 2026-01-26 - Load individual model files instead of phase2_model_data.rds
#
# Input:
#   - data/output/models/fh_model_base.rds (log-transformed base model)
#   - data/output/models/fh_model_base_nolog.rds (non-log base model)
#   - data/output/models/fh_model_final.rds (CV-selected final model)
#   - data/output/models/model_data_base.rds (backwards compatible)
# ==============================================================================

rm(list = ls())
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggpubr, viridis, latex2exp)

data_path <- "./data"
manuscript_path <- "./manuscript"  # EPS figures go here for LaTeX compilation

# Load auxiliaries
source(paste0(getwd(),"/R/auxiliaries/00_themes.r"))
source(paste0(getwd(),"/R/auxiliaries/01_plot_functions.r"))

#### Load Saved Model Objects ####

message("\n=== Loading model objects ===\n")

# Try to load individual model files first (new structure)
# Fall back to model_data_base.rds if individual files don't exist

fh_model <- NULL
fh_model_nolog <- NULL

# Load log-transformed base model
if (file.exists(file.path(data_path, "output/models/fh_model_base.rds"))) {
  fh_model <- readRDS(file.path(data_path, "output/models/fh_model_base.rds"))
  message("Loaded: fh_model_base.rds (log-transformed)")
}

# Load non-log base model
if (file.exists(file.path(data_path, "output/models/fh_model_base_nolog.rds"))) {
  fh_model_nolog <- readRDS(file.path(data_path, "output/models/fh_model_base_nolog.rds"))
  message("Loaded: fh_model_base_nolog.rds (non-log)")
}

# Backwards compatibility: try model_data_base.rds
if (is.null(fh_model) || is.null(fh_model_nolog)) {
  model_data_path <- file.path(data_path, "output/models/model_data_base.rds")
  if (file.exists(model_data_path)) {
    model_data <- readRDS(model_data_path)
    if (is.null(fh_model) && !is.null(model_data$model)) {
      fh_model <- model_data$model
      message("Loaded: model_data_base.rds$model (log-transformed)")
    }
    if (is.null(fh_model_nolog) && !is.null(model_data$model_nolog)) {
      fh_model_nolog <- model_data$model_nolog
      message("Loaded: model_data_base.rds$model_nolog (non-log)")
    }
  }
}

# Final check
if (is.null(fh_model)) {
  stop("Could not load log-transformed base model. Run fit_fh_models.r first.")
}
if (is.null(fh_model_nolog)) {
  warning("Non-log model not found. Fig 6 (log vs non-log comparison) will be skipped.")
}

#### Visualization Parameters ####
pointshape <- 21
pointsize <- 2
textwidth = 15.99773
windowsFonts(Times=windowsFont("Times"))
scal_ch = 0.8

Theme <- General_theme()

# ==============================================================================
# Consistent Color Scheme
# ==============================================================================
# Use same colors for same concepts across all figures:
# - Purple tones for residuals
# - Green tones for random effects
# - Lighter shades = standard/baseline, darker shades = log/improved

color_residuals <- c(
  "standard" = "#9E9AC8",  # Light purple (standard FH)
  "log" = "#6A51A3"        # Dark purple (log-transformed FH)
)

color_random_effects <- c(
  "standard" = "#ADDD8E",  # Light green (standard FH)
  "log" = "#41AB5D"        # Dark green (log-transformed FH)
)

# Single-model colors (for FigNormDiagnostics)
color_single <- c(
  "residuals" = "#6A51A3",      # Purple for residuals
  "random_effects" = "#41AB5D"  # Green for random effects
)

# ==============================================================================
# FigLogComparison: Log vs Non-Log Model Comparison
# ==============================================================================
# This figure justifies the use of log transformation by comparing
# KDE plots of standardized residuals and random effects

if (!is.null(fh_model_nolog)) {
  message("\n=== Generating FigLogComparison: Log vs Non-Log Comparison ===")

  # Use Compare_Density_plots to create comparison
  # color1 = residuals panel, color2 = random effects panel
  # Each vector: c(standard model, log model)
  # Note: unname() removes vector names to avoid ggplot2 color mapping issues
  Log_vs_NonLog_Comparison <- Compare_Density_plots(
    model1 = fh_model_nolog,
    model2 = fh_model,
    Modnames = c("Standard FH", "Log-transformed FH"),
    color1 = unname(c(color_residuals["standard"], color_residuals["log"])),
    color2 = unname(c(color_random_effects["standard"], color_random_effects["log"])),
    heading = "",
    alpha = 0.6,
    legend.key.size = unit(0.5, 'cm'),
    Theme = Theme)

  # 2-panel plot (a) residuals, (b) random effects
  FigLogComparison <- Log_vs_NonLog_Comparison$comb

  # Save FigLogComparison
  ggsave(filename = file.path(manuscript_path, "FigLogComparison.eps"),
         plot = FigLogComparison,
         width = textwidth,
         device = cairo_ps,
         height = textwidth * 0.55,
         units = "cm")

  message("Saved: FigLogComparison.eps (Log vs Non-Log comparison)")

  # Calculate and print normality statistics
  calc_kurtosis <- function(model) {
    re <- model$model$random_effects
    re_std <- (re - mean(re)) / sd(re)
    n <- length(re)
    sum(re_std^4) / n
  }

  calc_kurtosis_resid <- function(model) {
    resid <- model$model$std_real_residuals
    resid_std <- (resid - mean(resid)) / sd(resid)
    n <- length(resid)
    sum(resid_std^4) / n
  }

  message("\nNormality Statistics:")
  message("  Non-log model:")
  message("    Std. Residuals - Kurtosis: ", round(calc_kurtosis_resid(fh_model_nolog), 2))
  message("    Std. Random Effects - Kurtosis: ", round(calc_kurtosis(fh_model_nolog), 2))
  message("  Log model:")
  message("    Std. Residuals - Kurtosis: ", round(calc_kurtosis_resid(fh_model), 2))
  message("    Std. Random Effects - Kurtosis: ", round(calc_kurtosis(fh_model), 2))

} else {
  message("\n=== Skipping FigLogComparison: Non-log model not available ===")
  message("Run fit_fh_models.r with FIT_BASE_NOLOG = TRUE to generate this figure")
}

# ==============================================================================
# FigNormDiagnostics: Normality Diagnostics for Final Model (CV-selected lambda.min)
# ==============================================================================

message("\n=== Generating FigNormDiagnostics: Final Model Normality Diagnostics ===")

# Load final model (CV-selected lambda.min) for Fig 7
fh_model_final <- NULL
final_model_path <- file.path(data_path, "output/models/fh_model_final.rds")
lambda_min_path <- file.path(data_path, "output/models/fh_model_lambda_min.rds")

if (file.exists(final_model_path)) {
  fh_model_final <- readRDS(final_model_path)
  message("Loaded: fh_model_final.rds (CV-selected)")
} else if (file.exists(lambda_min_path)) {
  fh_model_final <- readRDS(lambda_min_path)
  message("Loaded: fh_model_lambda_min.rds")
} else {
  # Fall back to base model
  fh_model_final <- fh_model
  message("Using base model for Fig 7 (final model not found)")
}

#### Plot Normality Diagnostics ####

# Density plots for residuals and random effects
# Use consistent colors: purple for residuals, green for random effects
# Note: unname() removes vector names to avoid ggplot2 color mapping issues
Norm_Dens <- Density_plots(model = fh_model_final,
              heading = "",
              color = unname(c(color_single["residuals"], color_single["random_effects"])),
              alpha = 0.6,
              Theme = Theme)

# QQ plots for residuals and random effects
Norm_QQ <- QQ_plots(model = fh_model_final,
         heading = "",
         color = unname(c(color_single["residuals"], color_single["random_effects"])),
         pointshape,
         pointsize,
         alpha = 0.6,
         restitle = ggtitle("QQ - Std. Residuals"),
         rantitle = ggtitle("QQ - Std. Random Effects"),
         Theme = Theme)

#### Merge Diagnostic Plots ####

row_spacing <- theme(plot.margin = ggplot2::margin(t = 0.3, r = 0, b = 0.3, l = 0, unit = "cm"))
Norm_Diagnostics <- ggpubr::ggarrange(
  Norm_Dens[[1]] + ggtitle("(a) Density - Std. Residuals") + row_spacing,
  Norm_QQ[[1]] + ggtitle("(b) QQ - Std. Residuals") + row_spacing,
  Norm_Dens[[2]] + ggtitle("(c) Density - Std. Random Effects") + row_spacing,
  Norm_QQ[[2]] + ggtitle("(d) QQ - Std. Random Effects") + row_spacing,
  ncol = 2, nrow = 2)

#### Save Diagnostic Plot ####

ggsave(filename = file.path(manuscript_path, "FigNormDiagnostics.eps"),
       plot = Norm_Diagnostics,
       width = textwidth,
       device = cairo_ps,
       height = textwidth*1.0,
       units = "cm")

message("Saved: FigNormDiagnostics.eps (Final model normality diagnostics)")

#### Comparative Diagnostics: Base vs Final (Lambda.min) Models ####

message("\n=== Generating Comparative Diagnostics: Base vs Final ===")

# Comparative density plots: Base vs Final (Lambda.min)
# Use consistent color scheme: lighter = base, darker = final
# Note: Pass model objects directly (not wrapped in list), as Compare_Density_plots
# accesses model$model$std_real_residuals internally
# Note: unname() removes vector names to avoid ggplot2 color mapping issues
Comparative_Dens <- Compare_Density_plots(
  model1 = fh_model,
  model2 = fh_model_final,
  Modnames = c("Base Model", "Final Model (CV-selected)"),
  color1 = unname(c(color_residuals["standard"], color_residuals["log"])),
  color2 = unname(c(color_random_effects["standard"], color_random_effects["log"])),
  heading = "",
  alpha = 0.6,
  legend.key.size = unit(0.5, 'cm'),
  Theme = Theme)

#### Save Comparative Diagnostic Plot ####

ggsave(filename = file.path(manuscript_path, "FigDiagnosticsComparison.eps"),
       plot = Comparative_Dens$comb,
       width = textwidth,
       device = cairo_ps,
       height = textwidth*0.6,
       units = "cm")

message("Saved: FigDiagnosticsComparison.eps (Base vs Final model)")

message("\n=== Diagnostic Plots Complete ===\n")



