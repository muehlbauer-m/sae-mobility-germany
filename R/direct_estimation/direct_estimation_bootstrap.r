# ==============================================================================
# Direct Estimation: Bootstrap Variance Estimation (emdipar package)
# ==============================================================================
# Purpose: Calculate bootstrap variance estimates for direct HTH estimators
# Date: 2025-01-28
#
# This script provides THREE bootstrap variance estimation methods:
# 1. Naive bootstrap - uses sampling weights, no recalibration
# 2. MiD-calibrated bootstrap - recalibrates to MiD-weighted population totals
# 3. Census-calibrated bootstrap - recalibrates to external census totals
#
# IMPORTANT:
# - This script is OPTIONAL - analytical variance from direct_estimation.r is sufficient
# - Runtime: ~3-4 hours per method on Linux cluster with 200 cores
# - Windows: Falls back to sequential (~12 hours per method) - use Linux cluster!
#
# Prerequisites:
# - Run direct_estimation.r first to generate analytical estimates
# - emdipar package must be installed (see R/auxiliaries/install_emdipar.r)
# ==============================================================================

# Setup ------------------------------------------------------------------------
rm(list = ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, laeken)

# Data path (local git-tracked)
data_path <- "./data"

output_dir <- file.path(data_path, "output/direct")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# Configuration
# ==============================================================================

# Toggle each bootstrap method on/off
RUN_NAIVE_BOOTSTRAP <- FALSE          # 1. Naive bootstrap (no calibration)
RUN_MID_CALIB_BOOTSTRAP <- FALSE      # 2. Calibrated to MiD-weighted totals
RUN_CENSUS_CALIB_BOOTSTRAP <- TRUE    # 3. Calibrated to census totals

B_REPS <- 500                         # Number of bootstrap replications
N_CORES <- 500                    # Number of cores for parallel processing

message("\n=== BOOTSTRAP VARIANCE ESTIMATION ===")
message("Configuration:")
message("  - Naive bootstrap:            ", ifelse(RUN_NAIVE_BOOTSTRAP, "ENABLED", "DISABLED"))
message("  - MiD-calibrated bootstrap:   ", ifelse(RUN_MID_CALIB_BOOTSTRAP, "ENABLED", "DISABLED"))
message("  - Census-calibrated bootstrap: ", ifelse(RUN_CENSUS_CALIB_BOOTSTRAP, "ENABLED", "DISABLED"))
message("  - Bootstrap replications: B = ", B_REPS)
message("  - Parallel cores: ", N_CORES)

# ==============================================================================
# STEP 1: Load Cleaned Data
# ==============================================================================

message("\n=== Loading Data ===")

# Load trip-level data with person characteristics
df_trips_with_persons <- readRDS(
  file.path(data_path, "data_cleaning/df_trips_with_persons.rds")
)

# Load person-level data
df_persons <- readRDS(
  file.path(data_path, "data_cleaning/df_persons.rds")
)

message("Loaded ", nrow(df_persons), " persons")
message("Loaded ", nrow(df_trips_with_persons), " trips")

# ==============================================================================
# STEP 2: Create Person x Mode Dataset
# ==============================================================================

message("\n=== Creating Person x Mode Dataset ===")

# Calculate person x mode level trip statistics
trips_per_person_mode <- df_trips_with_persons %>%
  group_by(HP_ID_Reg, hvm_label) %>%
  summarise(
    perskm_mode = sum(wegkm_imp, na.rm = TRUE),
    n_trips = n(),
    .groups = "drop"
  )

# Create complete grid: all persons x all modes
all_modes <- c("fuss", "fahrrad", "miv", "oepv")

df_persons_for_merge <- df_persons %>%
  select(
    HP_ID_Reg,
    H_ID_Reg,
    ags5,
    BLAND,
    mid_gemeindetyp,
    mid_gemeindetyp_code,
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
  left_join(
    trips_per_person_mode,
    by = c("HP_ID_Reg", "hvm_label")
  ) %>%
  mutate(
    perskm_mode = replace_na(perskm_mode, 0),
    n_trips = replace_na(n_trips, 0)
  ) %>%
  mutate(
    hvm_label = factor(hvm_label, levels = all_modes)
  ) %>%
  # CRITICAL: Divide weights by 4 to preserve population total
  mutate(
    P_HOCH = P_HOCH / 4
  )

# Create domain variable
df_persons_modes <- df_persons_modes %>%
  mutate(
    ags5_hvm = paste(ags5, hvm_label, sep = "_")
  )

message("Created ", nrow(df_persons_modes), " person-mode observations")
message("Number of domains: ", length(unique(df_persons_modes$ags5_hvm)))

# ==============================================================================
# STEP 3: Create Calibration Matrix
# ==============================================================================

message("\n=== Creating Calibration Matrix ===")

# Create calibration matrices from df_persons (1 row per person)
# Using laeken::calibVars() to create dummy variables
#
# NOTE: Household size (hhgr_gr2) is EXCLUDED from calibration because:
# - MiD calibrated to HOUSEHOLD COUNTS at household level
# - NOT to PERSON TOTALS by household size at person level
# - For 1-person HHs this aligns (1 HH = 1 person), but for 2+ person HHs
#   the HH count does not directly translate to person count

# BLAND (16 states)
mat_calib_bland <- laeken::calibVars(df_persons$BLAND)

# Sex (2 categories)
mat_calib_sex <- laeken::calibVars(df_persons$HP_SEX)

# Age groups (5 categories)
mat_calib_age <- laeken::calibVars(df_persons$age_group)

# Temporal variables
mat_calib_quartal <- laeken::calibVars(df_persons$ST_QUARTAL)
mat_calib_wotag <- laeken::calibVars(df_persons$ST_WOTAG)

# Combine calibration matrices
# Total dimensions: 16 (BLAND) + 2 (Sex) + 5 (Age) + 4 (Quarter) + 7 (Weekday) = 34
mat_calib <- cbind(
  mat_calib_bland,
  mat_calib_sex,
  mat_calib_age,
  mat_calib_quartal,
  mat_calib_wotag
)

message("Calibration matrix dimensions: ", nrow(mat_calib), " rows x ", ncol(mat_calib), " cols")

# Replicate calibration matrix to match df_persons_modes structure
# (4 rows per person - one per mode)

message("\nExpanding calibration matrix for person-mode level...")
message("Original dimensions: ", nrow(mat_calib), " rows")
message("Target dimensions: ", nrow(df_persons_modes), " rows")

# Verify person ordering alignment
person_ids_from_persons <- df_persons$HP_ID_Reg
person_ids_from_modes <- df_persons_modes %>%
  slice(seq(1, n(), by = 4)) %>%
  pull(HP_ID_Reg)

if (!identical(person_ids_from_persons, person_ids_from_modes)) {
  stop("ERROR: Person ordering mismatch between df_persons and df_persons_modes!")
}

message("Person ordering verified")

# Replicate each row 4 times
mat_calib <- mat_calib[rep(1:nrow(mat_calib), each = 4), ]

message("Expanded dimensions: ", nrow(mat_calib), " rows x ", ncol(mat_calib), " cols")
stopifnot(nrow(mat_calib) == nrow(df_persons_modes))

# ==============================================================================
# STEP 4: Create Census Totals Vector (for census-calibrated bootstrap)
# ==============================================================================

if (RUN_CENSUS_CALIB_BOOTSTRAP) {
  message("\n=== Creating Census Totals Vector ===")

  # Load census data
  df_census <- readRDS(
    file.path(data_path, "raw/census/census_merged_2017.rds")
  )

  # Extract BLAND from ags5
  df_census <- df_census %>%
    mutate(BLAND = substr(ags5, 1, 2))

  # BLAND totals (16 values)
  # Note: Census columns have "census_" prefix
  census_totals_bland <- df_census %>%
    group_by(BLAND) %>%
    summarise(total = sum(census_bevoelkerungsstand_total, na.rm = TRUE), .groups = "drop") %>%
    arrange(BLAND) %>%
    pull(total)

  # Sex totals (2 values)
  census_totals_sex <- c(
    sum(df_census$census_bevoelkerungsstand_maennlich, na.rm = TRUE),  # HP_SEX = 1
    sum(df_census$census_bevoelkerungsstand_weiblich, na.rm = TRUE)    # HP_SEX = 2
  )

  # Age totals (5 values, matching MiD age_group categories)
  # Note: Census uses 5-year bands, need to aggregate to MiD categories
  # Using 0.6/0.4 split for 15-20 age band (approximate)
  census_totals_age <- c(
    # 0-17
    sum(df_census$census_bevoelkerungsstand_total_unter_5_jahre, na.rm = TRUE) +
      sum(df_census$census_bevoelkerungsstand_total_5_bis_unter_10_jahre, na.rm = TRUE) +
      sum(df_census$census_bevoelkerungsstand_total_10_bis_unter_15_jahre, na.rm = TRUE) +
      0.6 * sum(df_census$census_bevoelkerungsstand_total_15_bis_unter_20_jahre, na.rm = TRUE),
    # 18-34
    0.4 * sum(df_census$census_bevoelkerungsstand_total_15_bis_unter_20_jahre, na.rm = TRUE) +
      sum(df_census$census_bevoelkerungsstand_total_20_bis_unter_25_jahre, na.rm = TRUE) +
      sum(df_census$census_bevoelkerungsstand_total_25_bis_unter_30_jahre, na.rm = TRUE) +
      sum(df_census$census_bevoelkerungsstand_total_30_bis_unter_35_jahre, na.rm = TRUE),
    # 35-49
    sum(df_census$census_bevoelkerungsstand_total_35_bis_unter_40_jahre, na.rm = TRUE) +
      sum(df_census$census_bevoelkerungsstand_total_40_bis_unter_45_jahre, na.rm = TRUE) +
      sum(df_census$census_bevoelkerungsstand_total_45_bis_unter_50_jahre, na.rm = TRUE),
    # 50-64
    sum(df_census$census_bevoelkerungsstand_total_50_bis_unter_55_jahre, na.rm = TRUE) +
      sum(df_census$census_bevoelkerungsstand_total_55_bis_unter_60_jahre, na.rm = TRUE) +
      sum(df_census$census_bevoelkerungsstand_total_60_bis_unter_65_jahre, na.rm = TRUE),
    # 65+
    sum(df_census$census_bevoelkerungsstand_total_65_bis_unter_70_jahre, na.rm = TRUE) +
      sum(df_census$census_bevoelkerungsstand_total_70_bis_unter_75_jahre, na.rm = TRUE) +
      sum(df_census$census_bevoelkerungsstand_total_75_bis_unter_80_jahre, na.rm = TRUE) +
      sum(df_census$census_bevoelkerungsstand_total_80_bis_unter_85_jahre, na.rm = TRUE) +
      sum(df_census$census_bevoelkerungsstand_total_85_bis_unter_90_jahre, na.rm = TRUE) +
      sum(df_census$census_bevoelkerungsstand_total_90_bis_unter_95_jahre, na.rm = TRUE) +
      sum(df_census$census_bevoelkerungsstand_total_95_jahre_und_mehr, na.rm = TRUE)
  )

  # Temporal totals: uniform distribution (census doesn't have temporal info)
  # Use total population / 4 for quarters, / 7 for weekdays
  total_population_census <- sum(df_census$census_bevoelkerungsstand_total, na.rm = TRUE)

  census_totals_quartal <- rep(total_population_census / 4, 4)
  census_totals_wotag <- rep(total_population_census / 7, 7)

  # Combine into vector (must match mat_calib column order!)
  totals_census <- c(
    census_totals_bland,    # 16 values
    census_totals_sex,      # 2 values
    census_totals_age,      # 5 values
    census_totals_quartal,  # 4 values
    census_totals_wotag     # 7 values
  )

  # Verify totals vector matches calibration matrix
 stopifnot(length(totals_census) == ncol(mat_calib))

  message("Census totals vector length: ", length(totals_census))
  message("Total population (census): ", format(total_population_census, big.mark = ","))
}

# ==============================================================================
# STEP 5: Load emdipar Package
# ==============================================================================

if (RUN_NAIVE_BOOTSTRAP || RUN_MID_CALIB_BOOTSTRAP || RUN_CENSUS_CALIB_BOOTSTRAP) {
  message("\n=== Loading emdipar Package ===")

  if (!require("emdipar")) {
    message("emdipar package not installed. Installing from local source...")
    source("./R/auxiliaries/install_emdipar.r")
    library(emdipar)
  }

  # Set number of cores for parallel processing
  options(mc.cores = N_CORES)
  message("Using ", getOption("mc.cores"), " cores for parallel bootstrap")
}

# ==============================================================================
# STEP 6: Naive Bootstrap
# ==============================================================================

if (RUN_NAIVE_BOOTSTRAP) {
  message("\n")
  message("==============================================================================")
  message("  NAIVE BOOTSTRAP VARIANCE ESTIMATION")
  message("==============================================================================")
  message("NOTE: Naive bootstrap uses sampling weights but does NOT recalibrate")
  message("      each bootstrap sample. Variances will typically be LARGER than")
  message("      calibrated methods because calibration reduces variance.")
  message("Bootstrap replications: B = ", B_REPS)

  start_time <- Sys.time()

  df_HTH_perskm_emdi_naive <- emdipar::direct(
    y = "perskm_mode",
    weights = "P_HOCH",
    boot_type = "naive",
    smp_data = as.data.frame(df_persons_modes),
    smp_domains = "ags5_hvm",
    B = B_REPS,
    var = TRUE
  )

  end_time <- Sys.time()
  runtime <- difftime(end_time, start_time, units = "hours")

  message("\nNaive bootstrap complete")
  message("Runtime: ", round(runtime, 2), " hours")

  # Save results
  saveRDS(
    df_HTH_perskm_emdi_naive,
    file.path(output_dir, "df_HTH_perskm_emdi_naive.rds")
  )

  message("Results saved to: ", file.path(output_dir, "df_HTH_perskm_emdi_naive.rds"))

} else {
  message("\n=== NAIVE BOOTSTRAP SKIPPED ===")
}

# ==============================================================================
# STEP 7: MiD-Calibrated Bootstrap
# ==============================================================================

if (RUN_MID_CALIB_BOOTSTRAP) {
  message("\n")
  message("==============================================================================")
  message("  MiD-CALIBRATED BOOTSTRAP VARIANCE ESTIMATION")
  message("==============================================================================")
  message("NOTE: Each bootstrap sample is recalibrated to match MiD-weighted")
  message("      population totals (BLAND, Sex, Age, Quarter, Weekday).")
  message("      totals = NULL means emdi uses weighted sample totals as targets.")
  message("Bootstrap replications: B = ", B_REPS)

  start_time <- Sys.time()

  df_HTH_perskm_emdi_mid_calib <- emdipar::direct(
    y = "perskm_mode",
    weights = "P_HOCH",
    X_calib = mat_calib,
    totals = NULL,  # Use MiD-weighted sample totals
    boot_type = "calibrate",
    smp_data = as.data.frame(df_persons_modes),
    smp_domains = "ags5_hvm",
    B = B_REPS,
    var = TRUE
  )

  end_time <- Sys.time()
  runtime <- difftime(end_time, start_time, units = "hours")

  message("\nMiD-calibrated bootstrap complete")
  message("Runtime: ", round(runtime, 2), " hours")

  # Save results
  saveRDS(
    df_HTH_perskm_emdi_mid_calib,
    file.path(output_dir, "df_HTH_perskm_emdi_mid_calib.rds")
  )

  message("Results saved to: ", file.path(output_dir, "df_HTH_perskm_emdi_mid_calib.rds"))

} else {
  message("\n=== MiD-CALIBRATED BOOTSTRAP SKIPPED ===")
}

# ==============================================================================
# STEP 8: Census-Calibrated Bootstrap
# ==============================================================================

if (RUN_CENSUS_CALIB_BOOTSTRAP) {
  message("\n")
  message("==============================================================================")
  message("  CENSUS-CALIBRATED BOOTSTRAP VARIANCE ESTIMATION")
  message("==============================================================================")
  message("NOTE: Each bootstrap sample is recalibrated to match CENSUS")
  message("      population totals from Destatis (BLAND, Sex, Age).")
  message("      Temporal variables use uniform distribution.")
  message("Bootstrap replications: B = ", B_REPS)

  start_time <- Sys.time()

  df_HTH_perskm_emdi_census_calib <- emdipar::direct(
    y = "perskm_mode",
    weights = "P_HOCH",
    X_calib = mat_calib,
    totals = totals_census,
    boot_type = "calibrate",
    smp_data = as.data.frame(df_persons_modes),
    smp_domains = "ags5_hvm",
    B = B_REPS,
    var = TRUE
  )

  end_time <- Sys.time()
  runtime <- difftime(end_time, start_time, units = "hours")

  message("\nCensus-calibrated bootstrap complete")
  message("Runtime: ", round(runtime, 2), " hours")

  # Save results
  saveRDS(
    df_HTH_perskm_emdi_census_calib,
    file.path(output_dir, "df_HTH_perskm_emdi_census_calib.rds")
  )

  message("Results saved to: ", file.path(output_dir, "df_HTH_perskm_emdi_census_calib.rds"))

} else {
  message("\n=== CENSUS-CALIBRATED BOOTSTRAP SKIPPED ===")
}

# ==============================================================================
# STEP 9: Summary
# ==============================================================================

message("\n")
message("==============================================================================")
message("  BOOTSTRAP ESTIMATION COMPLETE")
message("==============================================================================")
message("Output directory: ", output_dir)

if (RUN_NAIVE_BOOTSTRAP) {
  message("\nNaive bootstrap results:")
  message("  - File: df_HTH_perskm_emdi_naive.rds")
  message("  - B = ", B_REPS, " replications")
  message("  - Method: No recalibration")
}

if (RUN_MID_CALIB_BOOTSTRAP) {
  message("\nMiD-calibrated bootstrap results:")
  message("  - File: df_HTH_perskm_emdi_mid_calib.rds")
  message("  - B = ", B_REPS, " replications")
  message("  - Method: Recalibrated to MiD-weighted totals")
}

if (RUN_CENSUS_CALIB_BOOTSTRAP) {
  message("\nCensus-calibrated bootstrap results:")
  message("  - File: df_HTH_perskm_emdi_census_calib.rds")
  message("  - B = ", B_REPS, " replications")
  message("  - Method: Recalibrated to census totals")
}

message("\nNote: Analytical variance estimates (from direct_estimation.r) are")
message("      sufficient for most purposes. Bootstrap is for validation.")

# ==============================================================================
# END OF SCRIPT
# ==============================================================================
