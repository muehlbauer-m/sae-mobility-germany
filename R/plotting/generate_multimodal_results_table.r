################################################################################
# Generate Multimodal Results Tables
#
# Purpose: Create LaTeX tables showing HJ (direct) and FH (EBLUP) estimates
#          with CVs for the multimodal analysis (401 districts x 4 modes).
#
#          Generates two tables:
#          1. tbl_comparison_summary.tex - Compact summary for main text
#          2. tbl_results_appendix.tex - Full district-level results for appendix
#
# Input:
#   - data/output/direct/df_HTH_perskm_analytical.rds (direct estimates)
#   - data/output/models/fh_model_final.rds or fh_model_lambda_min.rds (FH model)
#   - data/raw/shapes/vg2500_01-01.gk3.shape/vg2500/VG2500_KRS.shp (district names)
#
# Output:
#   - data/Tex/Tables/tbl_comparison_summary.tex
#   - data/Tex/Tables/tbl_results_appendix.tex
#
# Created: 2026-01-26
################################################################################

# Load packages ---------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf,
  stringr
)

# Set paths -------------------------------------------------------------------
data_path <- "./data"

output_dir <- file.path(data_path, "Tex", "Tables")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

cat("\n==================================================\n")
cat("Generate Multimodal Results Tables\n")
cat("==================================================\n\n")

# ==============================================================================
# STEP 1: Load Data
# ==============================================================================
cat("STEP 1: Loading data...\n")

# --- Load direct estimates ---
direct_file <- file.path(data_path, "output/direct/df_HTH_perskm_analytical.rds")
if (!file.exists(direct_file)) {
  stop("Direct estimates not found: ", direct_file)
}
df_direct <- readRDS(direct_file)
cat("  Direct estimates: ", nrow(df_direct), " rows\n", sep = "")

# --- Load FH model ---
# Try fh_model_final.rds first, then fh_model_lambda_min.rds
fh_file <- file.path(data_path, "output/models/fh_model_final.rds")
if (!file.exists(fh_file)) {
  fh_file <- file.path(data_path, "output/models/fh_model_lambda_min.rds")
}
if (!file.exists(fh_file)) {
  stop("FH model not found. Expected fh_model_final.rds or fh_model_lambda_min.rds")
}
fh_model <- readRDS(fh_file)
cat("  FH model: ", basename(fh_file), "\n", sep = "")

# Extract FH estimates
df_fh <- data.frame(
  ags5_hvm = fh_model$ind$Domain,
  FH = fh_model$ind$FH,
  MSE_FH = fh_model$MSE$FH
) %>%
  mutate(
    ags5 = sub("_.*", "", ags5_hvm),
    hvm_label = sub(".*_", "", ags5_hvm),
    CV_FH = sqrt(MSE_FH) / FH
  )
cat("  FH estimates: ", nrow(df_fh), " rows\n", sep = "")

# --- Load district shapefile for names and types ---
shp_file <- file.path(data_path, "raw/shapes/vg2500_01-01.gk3.shape/vg2500/VG2500_KRS.shp")
if (!file.exists(shp_file)) {
  stop("District shapefile not found: ", shp_file)
}
shp_districts <- st_read(shp_file, quiet = TRUE) %>%
  st_drop_geometry() %>%
  transmute(
    ags5 = ARS,
    district_name = GEN,
    district_type = BEZ
  ) %>%
  distinct()
cat("  District metadata: ", nrow(shp_districts), " districts\n\n", sep = "")

# ==============================================================================
# STEP 2: Merge and Prepare Data
# ==============================================================================
cat("STEP 2: Merging data...\n")

# Merge direct and FH estimates
df_combined <- df_direct %>%
  select(ags5_hvm, ags5, hvm_label, perskm_hth, cv_hth) %>%
  left_join(
    df_fh %>% select(ags5_hvm, FH, CV_FH),
    by = "ags5_hvm"
  ) %>%
  left_join(shp_districts, by = "ags5")

# Check for missing values
n_missing_fh <- sum(is.na(df_combined$FH))
n_missing_names <- sum(is.na(df_combined$district_name))
if (n_missing_fh > 0) {
  cat("  WARNING: ", n_missing_fh, " domains missing FH estimates\n", sep = "")
}
if (n_missing_names > 0) {
  cat("  WARNING: ", n_missing_names, " domains missing district names\n", sep = "")
}

# Create English mode labels and define order
mode_order <- c("fuss" = 1, "fahrrad" = 2, "miv" = 3, "oepv" = 4)
mode_labels <- c(
  "fuss" = "Walking",
  "fahrrad" = "Cycling",
  "miv" = "Car",
  "oepv" = "Transit"
)

df_combined <- df_combined %>%
  mutate(
    mode_order = mode_order[hvm_label],
    mode_label_en = mode_labels[hvm_label]
  ) %>%
  arrange(ags5, mode_order)

cat("  Combined dataset: ", nrow(df_combined), " rows\n", sep = "")
cat("  Unique districts: ", n_distinct(df_combined$ags5), "\n\n", sep = "")

# ==============================================================================
# STEP 3: Helper Functions
# ==============================================================================

# Escape special LaTeX characters (including German)
escape_latex <- function(x) {
  x <- str_replace_all(x, fixed("&"), "\\&")
  x <- str_replace_all(x, fixed("%"), "\\%")
  x <- str_replace_all(x, fixed("#"), "\\#")
  x <- str_replace_all(x, fixed("_"), "\\_")
  x <- str_replace_all(x, fixed("ä"), "\\\"a")
  x <- str_replace_all(x, fixed("ö"), "\\\"o")
  x <- str_replace_all(x, fixed("ü"), "\\\"u")
  x <- str_replace_all(x, fixed("Ä"), "\\\"A")
  x <- str_replace_all(x, fixed("Ö"), "\\\"O")
  x <- str_replace_all(x, fixed("Ü"), "\\\"U")
  x <- str_replace_all(x, fixed("ß"), "\\ss{}")
  return(x)
}

# Format estimate with CV in parentheses
format_est_cv <- function(est, cv, est_digits = 2, cv_digits = 2) {
  # Handle NA values
  if (is.na(est) || is.na(cv)) {
    return("---")
  }
  sprintf(paste0("%.", est_digits, "f (", "%.", cv_digits, "f)"), est, cv)
}

# ==============================================================================
# STEP 4: Generate Summary Table (Main Text)
# ==============================================================================
cat("STEP 4: Generating summary table for main text...\n")

# Calculate summary statistics by mode
df_summary <- df_combined %>%
  group_by(hvm_label, mode_label_en, mode_order) %>%
  summarise(
    n = n(),
    # Direct (HJ) statistics
    hj_mean = mean(perskm_hth, na.rm = TRUE),
    hj_cv_min = min(cv_hth, na.rm = TRUE),
    hj_cv_max = max(cv_hth, na.rm = TRUE),
    # FH statistics
    fh_mean = mean(FH, na.rm = TRUE),
    fh_cv_min = min(CV_FH, na.rm = TRUE),
    fh_cv_max = max(CV_FH, na.rm = TRUE),
    # Relative variance (RV = MSE_FH / Var_HJ)
    median_rv = median(CV_FH^2 / cv_hth^2, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(mode_order)

# Format for display
df_summary <- df_summary %>%
  mutate(
    hj_display = sprintf("%.2f (%.2f--%.2f)", hj_mean, hj_cv_min, hj_cv_max),
    fh_display = sprintf("%.2f (%.2f--%.2f)", fh_mean, fh_cv_min, fh_cv_max),
    rv_display = sprintf("%.2f", median_rv)
  )

# Build LaTeX table
latex_summary <- c(
  "\\begin{table}[ht]",
  "\\centering",
  "\\caption{Comparison of direct (Hajek) and model-based (FH) estimates by transport mode. ",
  "Values show mean estimate with CV range (min--max) in parentheses. ",
  "RV = median relative variance (MSE\\textsubscript{FH} / Var\\textsubscript{HJ}).}",
  "\\label{tbl:comparison_summary}",
  "\\begin{tabularx}{\\textwidth}{lrXXr}",
  "\\hline",
  "Mode & $n$ & Direct (HJ) & FH (EBLUP) & Median RV \\\\",
  "\\hline",
  "\\hline"
)

for (i in 1:nrow(df_summary)) {
  row <- df_summary[i, ]
  latex_summary <- c(latex_summary,
    paste0(row$mode_label_en, " & ",
           row$n, " & ",
           row$hj_display, " & ",
           row$fh_display, " & ",
           row$rv_display, " \\\\")
  )
}

latex_summary <- c(latex_summary,
  "\\hline",
  "\\hline",
  "\\end{tabularx}",
  "\\end{table}"
)

# Save summary table
summary_file <- file.path(output_dir, "tbl_comparison_summary.tex")
writeLines(latex_summary, summary_file)
cat("  Saved: ", summary_file, "\n\n", sep = "")

# ==============================================================================
# STEP 5: Generate Full Appendix Table (Grouped by District)
# ==============================================================================
cat("STEP 5: Generating full appendix table...\n")

# Mark first row of each district group
df_appendix <- df_combined %>%
  arrange(ags5, mode_order) %>%
  group_by(ags5) %>%
  mutate(
    is_first_row = row_number() == 1,
    row_in_group = row_number()
  ) %>%
  ungroup()

# Format values
df_appendix <- df_appendix %>%
  mutate(
    hj_formatted = mapply(format_est_cv, perskm_hth, cv_hth),
    fh_formatted = mapply(format_est_cv, FH, CV_FH),
    # Escape district names for LaTeX
    district_name_tex = escape_latex(district_name)
  )

# Build LaTeX longtable (without Type column to save space)
latex_appendix <- c(
  "\\begin{longtable}{@{}p{1.2cm}p{4.5cm}p{1.5cm}p{3.2cm}p{3.2cm}@{}}",
  "\\caption{District-level estimates by transport mode. HJ = H\\'ajek direct estimate, ",
  "FH = Fay-Herriot EBLUP. Values show estimate with coefficient of variation (CV) in parentheses.}",
  "\\label{tbl:results_appendix} \\\\",
  "\\hline",
  "\\textbf{AGS5} & \\textbf{District} & \\textbf{Mode} & \\textbf{HJ (CV)} & \\textbf{FH (CV)} \\\\",
  "\\hline",
  "\\endfirsthead",
  "\\multicolumn{5}{c}{\\tablename\\ \\thetable{} -- continued from previous page} \\\\",
  "\\hline",
  "\\textbf{AGS5} & \\textbf{District} & \\textbf{Mode} & \\textbf{HJ (CV)} & \\textbf{FH (CV)} \\\\",
  "\\hline",
  "\\endhead",
  "\\hline",
  "\\multicolumn{5}{r}{Continued on next page} \\\\",
  "\\endfoot",
  "\\hline",
  "\\endlastfoot"
)

# Generate data rows (without Type column)
current_ags5 <- ""
for (i in 1:nrow(df_appendix)) {
  row <- df_appendix[i, ]

  # District info only on first row of group
  if (row$is_first_row) {
    ags5_cell <- row$ags5
    district_cell <- row$district_name_tex
  } else {
    ags5_cell <- ""
    district_cell <- ""
  }

  # Add row (5 columns: AGS5, District, Mode, HJ, FH)
  latex_appendix <- c(latex_appendix,
    paste0(ags5_cell, " & ",
           district_cell, " & ",
           row$mode_label_en, " & ",
           row$hj_formatted, " & ",
           row$fh_formatted, " \\\\")
  )

  # Add separator after last mode of each district (every 4th row)
  if (row$row_in_group == 4) {
    latex_appendix <- c(latex_appendix, "\\hline")
  }
}

# Close table
latex_appendix <- c(latex_appendix, "\\end{longtable}")

# Save appendix table
appendix_file <- file.path(output_dir, "tbl_results_appendix.tex")
writeLines(latex_appendix, appendix_file)
cat("  Saved: ", appendix_file, "\n", sep = "")
cat("  Total rows: ", nrow(df_appendix), " (", n_distinct(df_appendix$ags5), " districts x 4 modes)\n\n", sep = "")

# ==============================================================================
# STEP 6: Summary
# ==============================================================================
cat("==================================================\n")
cat("Multimodal Results Tables Generated\n")
cat("==================================================\n\n")

cat("Summary statistics by mode:\n")
print(df_summary %>% select(mode_label_en, n, hj_mean, fh_mean, median_rv))

cat("\n\nOutput files:\n")
cat("  1. Summary table (main text): ", summary_file, "\n", sep = "")
cat("  2. Full appendix table: ", appendix_file, "\n\n", sep = "")

cat("To include in manuscript:\n")
cat("  Main text:  \\input{../data/Tex/Tables/tbl_comparison_summary}\n")
cat("  Appendix:   \\input{../data/Tex/Tables/tbl_results_appendix}\n\n")

cat("Replace the hardcoded table in Manuscript.tex (lines 812-1221) with these \\input{} commands.\n\n")

################################################################################
# END OF SCRIPT
################################################################################
