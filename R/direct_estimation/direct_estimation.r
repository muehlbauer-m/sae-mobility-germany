# ==============================================================================
# Direct Estimation: Multimodal Person-Level Analysis (ags5 × hvm domain)
# ==============================================================================
# Purpose: Calculate HTH estimates for perskm1 (person km/day) by district × mode
# Date: 2025-11-25
#
# This script:
# 1. Loads person-level data with trip counts by mode
# 2. Creates person × mode dataset with 4 rows per person
# 3. Calculates direct HTH estimates using survey package (analytical variance)
# 4. Uses Taylor linearization for fast, design-based variance estimation
#
# Model domain: ags5 (401 districts) × hvm (4 modes) = ~1600 observations
# Modes: fuss, fahrrad, miv (driver + passenger), oepv
# Outcome: perskm_mode - person km/day by mode
#
# For bootstrap variance estimation, see: direct_estimation_bootstrap.r
# ==============================================================================

# Setup ------------------------------------------------------------------------
rm(list = ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, survey)

# Data path (local git-tracked)
data_path <- "./data"

output_dir <- file.path(data_path, "output/direct")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# STEP 1: Load Cleaned Data
# ==============================================================================

# Load trip-level data with person characteristics
df_trips_with_persons <- readRDS(
  file.path(data_path, "data_cleaning/df_trips_with_persons.rds")
)

# Load person-level data (for calibration)
# Note: df_persons already includes mid_gemeindetyp_code from data_cleaning.r
df_persons <- readRDS(
  file.path(data_path, "data_cleaning/df_persons.rds")
)

# Verify MiD-Gemeindetyp coverage
message("\n=== MiD-Gemeindetyp Coverage ===")
message("Total persons: ", nrow(df_persons))
message("Persons with mid_gemeindetyp_code: ", sum(!is.na(df_persons$mid_gemeindetyp_code)))
message("Coverage: ", round(100 * sum(!is.na(df_persons$mid_gemeindetyp_code)) / nrow(df_persons), 2), "%")
message("\nDistribution of MiD-Gemeindetyp:")
print(table(df_persons$mid_gemeindetyp_code, useNA = "ifany"))

# ==============================================================================
# STEP 2: Create Person × Mode Dataset
# ==============================================================================

# Calculate person × mode level trip statistics
# This aggregates trip data to person-mode level to get perskm1 by mode

trips_per_person_mode <- df_trips_with_persons %>%
  group_by(HP_ID_Reg, hvm_label) %>%
  summarise(
    # Distance by mode (sum of all trips of this mode for this person)
    perskm_mode = sum(wegkm_imp, na.rm = TRUE),
    n_trips = n(),
    .groups = "drop"
  )

# Create complete grid: all persons × all modes
all_modes <- c("fuss", "fahrrad", "miv", "oepv")

# Get person characteristics (take from trip data or merge from df_persons)
df_persons_for_merge <- df_persons %>%
  select(
    HP_ID_Reg,
    H_ID_Reg,       # Household ID for clustering in survey design
    ags5,
    BLAND,
    mid_gemeindetyp,      # MiD-Gemeindetyp for stratification
    mid_gemeindetyp_code, # Numeric code (1-10)
    age_group,
    HP_SEX,
    hhgr_gr2,
    ST_WOTAG,
    ST_QUARTAL,
    P_HOCH
  )

df_persons_modes <- df_persons_for_merge %>%
  crossing(hvm_label = all_modes) %>%
  relocate(hvm_label, .after = ags5) %>%
  # Merge trip statistics by person × mode
  left_join(
    trips_per_person_mode,
    by = c("HP_ID_Reg", "hvm_label")
  ) %>%
  # For person-mode combinations with no trips, set distance to 0
  mutate(
    perskm_mode = replace_na(perskm_mode, 0),
    n_trips = replace_na(n_trips, 0)
  ) %>%
  # Create mode as factor
  mutate(
    hvm_label = factor(hvm_label, levels = all_modes)
  ) %>%
  # CRITICAL: Divide weights by 4 to preserve population total
  # Each person appears 4 times (once per mode), so P_HOCH must be divided by 4
  # This ensures sum(weights) in df_persons_modes equals sum(weights) in df_persons
  mutate(
    P_HOCH = P_HOCH / 4
  )

# Verify weight adjustment
message("\n=== Weight Adjustment Verification ===")
message("Sum of weights in df_persons: ", format(sum(df_persons$P_HOCH), big.mark = ","))
message("Sum of weights in df_persons_modes: ", format(sum(df_persons_modes$P_HOCH), big.mark = ","))
message("Ratio (should be ≈ 1.0): ", round(sum(df_persons_modes$P_HOCH) / sum(df_persons$P_HOCH), 4))

# ==============================================================================
# STEP 3: Create Domain Variable (ags5 × hvm)
# ==============================================================================

# Create combined domain variable: ags5_hvm
df_persons_modes <- df_persons_modes %>%
  mutate(
    ags5_hvm = paste(ags5, hvm_label, sep = "_")
  )

# ==============================================================================
# STEP 4: Calculate Direct Estimates with survey Package (Analytical Variance)
# ==============================================================================

# This approach uses Taylor linearization for variance estimation, which is much
# faster than bootstrap and accounts for the complex survey design:
# - Clustering by household (ids = ~H_ID)
# - Stratification by state (strata = ~BLAND)
# - Calibration to known population totals

message("\n=== ANALYTICAL VARIANCE ESTIMATION (survey package) ===")
message("Number of domains: ", length(unique(df_persons_modes$ags5_hvm)))
message("Number of observations: ", nrow(df_persons_modes))

# Step 1: Define base survey design
# Following MiD methodology:
# - PSU (Primary Sampling Unit): Household ID (H_ID)
#   Rationale: All persons in household interviewed together, creates clustering
# - Strata: Federal State (BLAND)
#   Rationale: MiD stratified by BLAND and community type
# - Weights: P_HOCH (already divided by 4 for person-mode level)

message("\nDefining base survey design...")
message("  - PSU: H_ID_Reg (Household ID)")
message("  - Strata: BLAND + mid_gemeindetyp_code (Federal State + Regional Type)")
message("  - Weights: P_HOCH (scaled for person-mode level)")

message("\n  Stratification structure:")
message("    - Federal states (BLAND): ", length(unique(df_persons_modes$BLAND)))
message("    - Regional types (MiD-Gemeindetyp): ", length(unique(df_persons_modes$mid_gemeindetyp_code)))

# Calibration formula (person-level variables only)
# NOTE: This tells survey package which variables were used for calibration
#       It does NOT re-calibrate - P_HOCH is already calibrated by MiD
calib_formula <- ~BLAND + HP_SEX + age_group + ST_QUARTAL + ST_WOTAG

mid_design_base <- survey::svydesign(
  ids = ~H_ID_Reg,                        # Household clustering
  strata = ~BLAND + mid_gemeindetyp_code, # Federal State + Regional Type stratification
  weights = ~P_HOCH,                      # Scaled person-mode weights
  calibrate.formula = calib_formula,      # Calibration variables (person-level)
  data = df_persons_modes,
  nest = TRUE                             # IDs nested within strata
)

# Step 2: Use the design with pre-calibrated weights
# IMPORTANT: P_HOCH weights are ALREADY calibrated by MiD using Deville-Särndal raking
# We do NOT need to re-calibrate them
# The survey design already accounts for:
# - Clustering (H_ID_Reg)
# - Stratification (BLAND)
# - Calibrated weights (P_HOCH)

message("\nUsing pre-calibrated MiD weights...")
message("NOTE: P_HOCH weights are already calibrated to:")
message("  - BLAND (16 states)")
message("  - HP_SEX (2 categories)")
message("  - age_group (5 groups)")
message("  - ST_QUARTAL (4 quarters)")
message("  - ST_WOTAG (7 weekdays)")
message("  - Plus: Erwerbstätigkeit, Schulabschluss, Elementargebiet, Wohnsituation")
message("\nNo re-calibration needed - using weights as-is")

# Use the base design (already has clustering, stratification, and calibrated weights)
mid_design_calibrated <- mid_design_base

message("✓ Survey design ready")

# Step 3: Calculate direct estimates by domain (ags5 × hvm)
message("\nCalculating direct estimates by domain...")

# Use svyby to calculate mean and variance for each domain
# This respects the complex design and calibration

library(parallel)  # For multicore processing

# Set number of cores for parallel processing
# On Linux: mclapply will use this; on Windows: falls back to serial (mc.cores ignored)
n_cores <- parallel::detectCores() - 1  # Leave 1 core for system
options(mc.cores = n_cores)
message(sprintf("Using %d cores for parallel processing (total available: %d)",
                n_cores, parallel::detectCores()))

# Note: multicore = TRUE uses parallel::mclapply internally
# - On Linux/Unix: will use mc.cores option set above
# - On Windows: silently falls back to serial processing (mclapply limitation)
direct_estimates_survey <- survey::svyby(
  formula = ~perskm_mode,
  by = ~ags5_hvm,
  design = mid_design_calibrated,
  multicore = TRUE,
  FUN = survey::svymean,
  vartype = "var"
)

# Calculate sample sizes per domain (ags5 × hvm)
# Note: Within each ags5_hvm group, each person appears exactly once
# (e.g., in "01001_fuss" group, only the fuss rows are counted)
domain_sample_sizes <- df_persons_modes %>%
  group_by(ags5_hvm) %>%
  summarise(
    n_obs = n(),  # Count persons per district-mode (no division needed)
    n_with_trips = sum(n_trips > 0),
    .groups = "drop"
  )

# Clean up column names for compatibility
df_results_analytical <- direct_estimates_survey %>%
  as_tibble() %>%
  rename(
    perskm_hth = perskm_mode,
    var_hth = var  # Note: svyby adds var() wrapper
  ) %>%
  # Split ags5_hvm back into components
  separate(ags5_hvm, into = c("ags5", "hvm_label"), sep = "_", remove = FALSE) %>%
  mutate(
    hvm_label = factor(hvm_label, levels = all_modes),
    se_hth = sqrt(var_hth)
  ) %>%
  # Add sample sizes per domain
  left_join(domain_sample_sizes, by = "ags5_hvm") %>%
  # Calculate CV = SE / estimate
  mutate(
    cv_hth = se_hth / perskm_hth
  )

message("✓ Direct estimation complete")
message("\nSummary statistics:")
message("  Domains estimated: ", nrow(df_results_analytical))
message("  Mean perskm_hth: ", round(mean(df_results_analytical$perskm_hth, na.rm = TRUE), 2))
message("  Mean SE: ", round(mean(df_results_analytical$se_hth, na.rm = TRUE), 2))

# Save analytical results
saveRDS(
  df_results_analytical,
  file.path(output_dir, "df_HTH_perskm_analytical.rds")
)

saveRDS(
  mid_design_calibrated,
  file.path(output_dir, "mid_design_calibrated.rds")
)

message("\nAnalytical results saved to:")
message("  ", file.path(output_dir, "df_HTH_perskm_analytical.rds"))

# ==============================================================================
# STEP 5: Summary Statistics and Completion
# ==============================================================================

# Generate summary statistics from analytical results
summary_stats <- list(
  n_domains = nrow(df_results_analytical),
  n_districts = length(unique(df_results_analytical$ags5)),
  n_modes = length(unique(df_results_analytical$hvm_label)),

  # Sample sizes by mode
  sample_sizes = df_persons_modes %>%
    group_by(hvm_label) %>%
    summarise(
      n_persons = n() / 4,  # Divide by 4 since each person appears 4 times
      n_with_trips = sum(n_trips > 0) / 4,
      .groups = "drop"
    ),

  # Mean estimates by mode
  mean_by_mode = df_results_analytical %>%
    group_by(hvm_label) %>%
    summarise(
      mean_perskm = mean(perskm_hth, na.rm = TRUE),
      sd_perskm = sd(perskm_hth, na.rm = TRUE),
      .groups = "drop"
    ),

  # Variance statistics
  variance_stats = df_results_analytical %>%
    summarise(
      mean_var = mean(var_hth, na.rm = TRUE),
      median_var = median(var_hth, na.rm = TRUE),
      min_var = min(var_hth, na.rm = TRUE),
      max_var = max(var_hth, na.rm = TRUE)
    )
)

saveRDS(
  summary_stats,
  file.path(output_dir, "direct_estimation_summary_stats.rds")
)

# Print summary
message("\n=== DIRECT ESTIMATION COMPLETE ===")
message("Results saved to: ", output_dir)
message("\nSummary:")
message("  Total domains: ", nrow(df_results_analytical))
message("  Districts: ", length(unique(df_results_analytical$ags5)))
message("  Modes: ", paste(levels(df_results_analytical$hvm_label), collapse = ", "))
message("\nMean estimates by mode:")
print(summary_stats$mean_by_mode)
message("\nNext steps:")
message("  - For bootstrap variance, run: direct_estimation_bootstrap.r")
message("  - To validate calibration, run: validate_mid_calibration.r")
message("  - Proceed to variable selection and FH modeling")

# ==============================================================================
# END OF SCRIPT
# ==============================================================================
