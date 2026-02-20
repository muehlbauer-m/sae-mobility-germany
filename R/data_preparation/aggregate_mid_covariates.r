# ==============================================================================
# MiD Covariate Aggregation for Multimodal FH Model
# ==============================================================================
# Purpose: Aggregate MiD trip-level AND person-level data to ags5 × mode level
#          for use as covariates in the multimodal Fay-Herriot model
# Author: Michael Mühlbauer
# Date: 2025-12-05
#
# Strategy:
#   1. Aggregate trip-level variables by district × mode (mode-specific)
#   2. Aggregate person-level variables by district (person characteristics)
#   3. Merge both into single covariate dataset
#
# Input:
#   - data/data_cleaning/df_trips_with_persons.rds (trip-level data)
#   - data/data_cleaning/df_persons.rds (person-level data)
#   - data/temp/VarCoding_MiD_Trips.xlsx (trip variable specification)
#   - data/temp/VarCoding_MiD_Persons.xlsx (person variable specification)
#
# Output:
#   - data/output/aggregates/df_mid_covariates_trips_mode.rds (trip: ags5 × mode)
#   - data/output/aggregates/df_mid_covariates_persons_ags5.rds (person: ags5)
#   - data/output/aggregates/df_mid_covariates_combined.rds (merged: ags5 × mode)
#
# Domain structure: 401 districts × 4 modes = ~1604 observations
# ==============================================================================

# Setup ------------------------------------------------------------------------
rm(list = ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, srvyr, readxl)

# Source aggregation functions
source("./R/auxiliaries/07_aggregation_functions.r")

# Define data path (local git-tracked)
data_path <- "./data"

message("Using data path: ", data_path)

# Load Data --------------------------------------------------------------------

message("\n=== Loading datasets ===")
df_trips_raw <- readRDS(file.path(data_path, "data_cleaning/df_trips_with_persons.rds"))
df_persons_raw <- readRDS(file.path(data_path, "data_cleaning/df_persons.rds"))

message("Loaded ", nrow(df_trips_raw), " trips")
message("Loaded ", nrow(df_persons_raw), " persons")

# ==============================================================================
# PART 1: TRIP-LEVEL AGGREGATION (ags5 × mode)
# ==============================================================================

message("\n" , paste(rep("=", 80), collapse = ""))
message("PART 1: TRIP-LEVEL AGGREGATION (ags5 × mode)")
message(paste(rep("=", 80), collapse = ""))

df_trips <- df_trips_raw

# Rename columns for consistency with VarCoding
# The issue: df_trips has ags5_trip, ags5_so, ags5_zo, ags5_person
# We want: ags5 (from ags5_person, person's residence district)
# And keep ags5_trip renamed as ags5_trip (trip start district) -> remove _trip to become just "ags5"
# But wait - that creates duplicate ags5! Solution: drop ags5_trip entirely, use ags5_person

df_trips <- df_trips %>%
  # First, remove _trip suffix from variables, excluding ags5_trip and survey design vars
  rename_with(
    ~ gsub("_trip$", "", .x),
    .cols = ends_with("_trip") &
      !matches("^ags5_trip$|^H_ID_Reg_trip$|^BLAND_trip$|^BLAND_GEO_trip$")
  ) %>%
  # Now rename survey design variables
  rename(
    H_ID_Reg = H_ID_Reg_trip,
    BLAND = BLAND_trip,
    BLAND_GEO = BLAND_GEO_trip
  ) %>%
  # Finally, drop ags5_trip and rename ags5_person to ags5
  select(-ags5_trip) %>%
  rename(ags5 = ags5_person)

message("Loaded ", nrow(df_trips), " trips")
message("Unique districts (ags5): ", length(unique(df_trips$ags5)))
message("Mode distribution:")
print(table(df_trips$hvm_label, useNA = "ifany"))

# Load Trip VarCoding ----------------------------------------------------------

message("\n=== Loading trip variable coding specification ===")
VarCoding_MiD_Trips <- readxl::read_xlsx(file.path(data_path, "variable_codings/VarCoding_MiD_Trips.xlsx")) %>%
  replace_na(list(FilterVar = "none", PseudoMetric = "no"))

message("Trip VarCoding loaded: ", nrow(VarCoding_MiD_Trips), " variables")
message("Trip variables to aggregate (VarCode != 0): ",
        sum(VarCoding_MiD_Trips$VarCode != 0))

# Filter VarCoding to only include variables present in df_trips
vars_in_data <- names(df_trips)
vars_to_aggregate <- VarCoding_MiD_Trips %>%
  filter(VarCode != 0) %>%
  pull(Variable)

missing_vars <- setdiff(vars_to_aggregate, vars_in_data)
if (length(missing_vars) > 0) {
  message("\nWARNING: ", length(missing_vars), " trip variables in VarCoding are missing in df_trips")
  message("These will be skipped:")
  message(paste("  -", head(missing_vars, 10), collapse = "\n"))
  if (length(missing_vars) > 10) {
    message("  ... and ", length(missing_vars) - 10, " more")
  }

  # Filter VarCoding to only include available variables
  VarCoding_MiD_Trips <- VarCoding_MiD_Trips %>%
    filter(Variable %in% c(vars_in_data, NA) | VarCode == 0)

  message("\nFiltered Trip VarCoding: ", sum(VarCoding_MiD_Trips$VarCode != 0), " variables to aggregate")
}

# Calculate number of aggregated columns
n_means_trips <- sum(VarCoding_MiD_Trips$VarCode == 1)
n_props_trips <- VarCoding_MiD_Trips %>%
  filter(VarCode == 2) %>%
  pull(Variable) %>%
  sapply(function(var) {
    if (var %in% names(df_trips)) {
      length(unique(df_trips[[var]]))
    } else {
      0
    }
  }) %>%
  sum()

message("Expected trip output columns: ~", n_means_trips + n_props_trips + 2,
        " (", n_means_trips, " means + ", n_props_trips, " proportions + 2 IDs)")

# Prepare Data for Aggregation -------------------------------------------------

message("\n=== Preparing data for mode-specific aggregation ===")

# Create ags5_hvm domain identifier
df_trips <- df_trips %>%
  mutate(ags5_hvm = paste(ags5, hvm_label, sep = "_"))

message("Created ags5_hvm domain identifier")
message("Unique domains: ", length(unique(df_trips$ags5_hvm)))

# Verify all modes are present
mode_counts <- df_trips %>%
  count(hvm_label) %>%
  arrange(hvm_label)

message("\nTrips by mode:")
print(mode_counts)

# Check for missing combinations (domains with no trips in certain modes)
all_combinations <- expand_grid(
  ags5 = unique(df_trips$ags5),
  hvm_label = c("fuss", "fahrrad", "miv", "oepv")
) %>%
  mutate(ags5_hvm = paste(ags5, hvm_label, sep = "_"))

missing_combinations <- anti_join(
  all_combinations,
  df_trips %>% distinct(ags5_hvm),
  by = "ags5_hvm"
)

message("\nMissing ags5 × mode combinations (will be filled with 0): ",
        nrow(missing_combinations))

# Create Survey Design ---------------------------------------------------------

message("\n=== Creating survey design ===")

# Verify required columns
required_cols <- c("ags5_hvm", "W_HOCH", "H_ID_Reg", "BLAND", "mid_gemeindetyp_code")
missing_cols <- setdiff(required_cols, names(df_trips))
if (length(missing_cols) > 0) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
}

# Check for missing values in stratification variables
missing_bland <- sum(is.na(df_trips$BLAND))
missing_gemeindetyp <- sum(is.na(df_trips$mid_gemeindetyp_code))

if (missing_bland > 0 || missing_gemeindetyp > 0) {
  message("WARNING: Missing values in stratification variables:")
  message("  BLAND: ", missing_bland, " missing")
  message("  mid_gemeindetyp_code: ", missing_gemeindetyp, " missing")
  message("  Dropping rows with missing strata values")

  df_trips <- df_trips %>%
    filter(!is.na(BLAND), !is.na(mid_gemeindetyp_code))

  message("  Remaining trips: ", nrow(df_trips))
}

# Create survey design (stratified by BLAND × mid_gemeindetyp_code)
trip_design <- df_trips %>%
  srvyr::as_survey_design(
    ids = H_ID_Reg,
    strata = c(BLAND, mid_gemeindetyp_code),
    weights = W_HOCH,
    nest = TRUE
  )


# Aggregate Trip Variables -----------------------------------------------------

message("\n=== Starting trip aggregation (this may take 10-30 minutes) ===")
message("Start time: ", Sys.time())

df_mid_covariates_trips <- aggregator_fun(
  data = trip_design,
  VarCoding = VarCoding_MiD_Trips,
  ags = "ags5_hvm"
)

message("Trip aggregation complete!")
message("End time: ", Sys.time())

# Post-Process Trip Results ----------------------------------------------------

message("\n=== Post-processing trip results ===")

# Split ags5_hvm back into separate columns
df_mid_covariates_trips <- df_mid_covariates_trips %>%
  separate(ags5_hvm, into = c("ags5", "hvm_label"), sep = "_", remove = FALSE)

# Reorder columns: ags5_hvm, ags5, hvm_label, then alphabetical covariates
df_mid_covariates_trips <- df_mid_covariates_trips %>%
  select(ags5_hvm, ags5, hvm_label, everything())

message("Trip dataset dimensions: ", nrow(df_mid_covariates_trips), " rows × ",
        ncol(df_mid_covariates_trips), " columns")

# Verify trip coverage
message("\nTrip domain coverage:")
message("  Expected: ", nrow(all_combinations), " (401 districts × 4 modes)")
message("  Actual: ", nrow(df_mid_covariates_trips))
message("  Coverage: ",
        round(100 * nrow(df_mid_covariates_trips) / nrow(all_combinations), 1), "%")

# Check for missing values in trip data
n_missing_trips <- sum(!complete.cases(df_mid_covariates_trips))
if (n_missing_trips > 0) {
  warning("Trip dataset has ", n_missing_trips, " rows with missing values")
  message("Columns with NAs:")
  na_cols <- df_mid_covariates_trips %>%
    summarise(across(everything(), ~ sum(is.na(.)))) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "n_na") %>%
    filter(n_na > 0) %>%
    arrange(desc(n_na))
  print(na_cols)
}

# ==============================================================================
# PART 2: PERSON-LEVEL AGGREGATION (ags5 only)
# ==============================================================================

message("\n", paste(rep("=", 80), collapse = ""))
message("PART 2: PERSON-LEVEL AGGREGATION (ags5 only)")
message(paste(rep("=", 80), collapse = ""))

df_persons <- df_persons_raw

# Load Person VarCoding --------------------------------------------------------

message("\n=== Loading person variable coding specification ===")
VarCoding_MiD_Persons <- readxl::read_xlsx(file.path(data_path, "variable_codings/VarCoding_MiD_Persons.xlsx")) %>%
  replace_na(list(FilterVar = "none", PseudoMetric = "no"))

message("Person VarCoding loaded: ", nrow(VarCoding_MiD_Persons), " variables")
message("Person variables to aggregate (VarCode != 0): ",
        sum(VarCoding_MiD_Persons$VarCode != 0))

# Filter VarCoding to only include variables present in df_persons
vars_in_persons <- names(df_persons)
vars_to_aggregate_persons <- VarCoding_MiD_Persons %>%
  filter(VarCode != 0) %>%
  pull(Variable)

missing_vars_persons <- setdiff(vars_to_aggregate_persons, vars_in_persons)
if (length(missing_vars_persons) > 0) {
  message("\nWARNING: ", length(missing_vars_persons), " person variables in VarCoding are missing in df_persons")
  message("These will be skipped:")
  message(paste("  -", head(missing_vars_persons, 10), collapse = "\n"))
  if (length(missing_vars_persons) > 10) {
    message("  ... and ", length(missing_vars_persons) - 10, " more")
  }

  # Filter VarCoding to only include available variables
  VarCoding_MiD_Persons <- VarCoding_MiD_Persons %>%
    filter(Variable %in% c(vars_in_persons, NA) | VarCode == 0)

  message("\nFiltered Person VarCoding: ", sum(VarCoding_MiD_Persons$VarCode != 0), " variables to aggregate")
}

# Create Person Survey Design --------------------------------------------------

message("\n=== Creating person survey design ===")

# Check for missing values in stratification variables
missing_bland_persons <- sum(is.na(df_persons$BLAND))
missing_gemeindetyp_persons <- sum(is.na(df_persons$mid_gemeindetyp_code))

if (missing_bland_persons > 0 || missing_gemeindetyp_persons > 0) {
  message("WARNING: Missing values in stratification variables:")
  message("  BLAND: ", missing_bland_persons, " missing")
  message("  mid_gemeindetyp_code: ", missing_gemeindetyp_persons, " missing")
  message("  Dropping rows with missing strata values")

  df_persons <- df_persons %>%
    filter(!is.na(BLAND), !is.na(mid_gemeindetyp_code))

  message("  Remaining persons: ", nrow(df_persons))
}

# Create survey design (stratified by BLAND × mid_gemeindetyp_code)
person_design <- df_persons %>%
  srvyr::as_survey_design(
    ids = H_ID_Reg,
    strata = c(BLAND, mid_gemeindetyp_code),
    weights = P_HOCH,
    nest = TRUE
  )

message("Person survey design created:")
message("  PSU: H_ID_Reg (household)")
message("  Strata: BLAND × mid_gemeindetyp_code")
message("  Weights: P_HOCH")
message("  Domains: ags5 (", length(unique(df_persons$ags5)), " unique)")

# Aggregate Person Variables ---------------------------------------------------

message("\n=== Starting person aggregation (this may take 5-15 minutes) ===")
message("Start time: ", Sys.time())

df_mid_covariates_persons <- aggregator_fun(
  data = person_design,
  VarCoding = VarCoding_MiD_Persons,
  ags = "ags5"
)

message("Person aggregation complete!")
message("End time: ", Sys.time())

# Post-Process Person Results --------------------------------------------------

message("\n=== Post-processing person results ===")

message("Person dataset dimensions: ", nrow(df_mid_covariates_persons), " rows × ",
        ncol(df_mid_covariates_persons), " columns")

# Verify person coverage
expected_ags5 <- unique(df_persons$ags5)
message("\nPerson domain coverage:")
message("  Expected: ", length(expected_ags5), " districts")
message("  Actual: ", nrow(df_mid_covariates_persons))
message("  Coverage: ",
        round(100 * nrow(df_mid_covariates_persons) / length(expected_ags5), 1), "%")

# Check for missing values in person data
n_missing_persons <- sum(!complete.cases(df_mid_covariates_persons))
if (n_missing_persons > 0) {
  warning("Person dataset has ", n_missing_persons, " rows with missing values")
  message("Columns with NAs:")
  na_cols_persons <- df_mid_covariates_persons %>%
    summarise(across(everything(), ~ sum(is.na(.)))) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "n_na") %>%
    filter(n_na > 0) %>%
    arrange(desc(n_na))
  print(na_cols_persons)
}

# ==============================================================================
# PART 3: MERGE TRIP AND PERSON COVARIATES
# ==============================================================================

message("\n", paste(rep("=", 80), collapse = ""))
message("PART 3: MERGE TRIP AND PERSON COVARIATES")
message(paste(rep("=", 80), collapse = ""))

message("\n=== Merging trip and person covariates ===")

# Merge person-level covariates (ags5 level) to trip-level (ags5 × mode level)
# Person covariates will be replicated across all 4 modes within each district
df_mid_covariates_combined <- df_mid_covariates_trips %>%
  left_join(df_mid_covariates_persons, by = "ags5", suffix = c("", "_person"))

message("Combined dataset dimensions: ", nrow(df_mid_covariates_combined), " rows × ",
        ncol(df_mid_covariates_combined), " columns")

# Reorder: ags5_hvm, ags5, hvm_label, then all covariates alphabetically
df_mid_covariates_combined <- df_mid_covariates_combined %>%
  select(ags5_hvm, ags5, hvm_label, everything())

# Add mid_ prefix to all MiD variables ----------------------------------------

message("\n=== Adding mid_ prefix to variable names ===")

# Add mid_ prefix to trip-level covariates (exclude IDs)
id_cols_trips <- c("ags5", "hvm_label", "ags5_hvm")
trip_var_cols <- setdiff(names(df_mid_covariates_trips), id_cols_trips)
df_mid_covariates_trips <- df_mid_covariates_trips %>%
  rename_with(~paste0("mid_", .), all_of(trip_var_cols))
message("Added mid_ prefix to ", length(trip_var_cols), " trip-level variables")

# Add mid_ prefix to person-level covariates (exclude ags5)
person_var_cols <- setdiff(names(df_mid_covariates_persons), "ags5")
df_mid_covariates_persons <- df_mid_covariates_persons %>%
  rename_with(~paste0("mid_", .), all_of(person_var_cols))
message("Added mid_ prefix to ", length(person_var_cols), " person-level variables")

# Add mid_ prefix to combined covariates (exclude IDs)
combined_var_cols <- setdiff(names(df_mid_covariates_combined), id_cols_trips)
df_mid_covariates_combined <- df_mid_covariates_combined %>%
  rename_with(~paste0("mid_", .), all_of(combined_var_cols))
message("Added mid_ prefix to ", length(combined_var_cols), " combined variables")

# Save Output ------------------------------------------------------------------

message("\n=== Saving results ===")

output_dir <- file.path(data_path, "output/aggregates")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Save trip-level covariates (ags5 × mode)
output_file_trips <- file.path(output_dir, "df_mid_covariates_trips_mode.rds")
saveRDS(df_mid_covariates_trips, output_file_trips)
message("Saved trip covariates: ", output_file_trips)
message("  File size: ", round(file.size(output_file_trips) / 1024, 1), " KB")

# Save person-level covariates (ags5)
output_file_persons <- file.path(output_dir, "df_mid_covariates_persons_ags5.rds")
saveRDS(df_mid_covariates_persons, output_file_persons)
message("Saved person covariates: ", output_file_persons)
message("  File size: ", round(file.size(output_file_persons) / 1024, 1), " KB")

# Save combined covariates (ags5 × mode with person vars replicated)
output_file_combined <- file.path(output_dir, "df_mid_covariates_combined.rds")
saveRDS(df_mid_covariates_combined, output_file_combined)
message("Saved combined covariates: ", output_file_combined)
message("  File size: ", round(file.size(output_file_combined) / 1024, 1), " KB")

# # Optional: Save combined as CSV for inspection
# csv_file <- file.path(output_dir, "df_mid_covariates_combined.csv")
# write.csv(df_mid_covariates_combined, csv_file, row.names = FALSE)
# message("Also saved CSV: ", csv_file)

# Final Verification -----------------------------------------------------------

message("\n=== Final verification ===")

# Check that all 4 modes are present for each district in combined dataset
mode_balance <- df_mid_covariates_combined %>%
  count(ags5) %>%
  count(n, name = "n_districts")

message("\nDistricts by number of modes (combined dataset):")
print(mode_balance)

expected_4_modes <- all(df_mid_covariates_combined %>%
                         count(ags5) %>%
                         pull(n) == 4)

if (expected_4_modes) {
  message("✓ All districts have exactly 4 modes")
} else {
  warning("✗ Some districts don't have all 4 modes - check data quality")
}

# Check domain alignment with direct estimates
direct_file <- file.path(data_path, "output/direct/df_HTH_perskm_analytical.rds")
if (file.exists(direct_file)) {
  message("\nChecking alignment with direct estimates...")
  df_direct <- readRDS(direct_file)

  # Check if domain structures match
  domains_direct <- paste(df_direct$ags5, df_direct$hvm_label, sep = "_")
  domains_covars <- df_mid_covariates_combined$ags5_hvm

  message("Domains in direct estimates: ", length(domains_direct))
  message("Domains in MiD covariates: ", length(domains_covars))
  message("Domains in both: ", length(intersect(domains_direct, domains_covars)))

  # Identify mismatches
  only_in_direct <- setdiff(domains_direct, domains_covars)
  only_in_covars <- setdiff(domains_covars, domains_direct)

  if (length(only_in_direct) > 0) {
    warning("Domains in direct but not in covariates: ", length(only_in_direct))
    message("First few: ", paste(head(only_in_direct, 5), collapse = ", "))
  }

  if (length(only_in_covars) > 0) {
    warning("Domains in covariates but not in direct: ", length(only_in_covars))
    message("First few: ", paste(head(only_in_covars, 5), collapse = ", "))
  }

  if (length(only_in_direct) == 0 && length(only_in_covars) == 0) {
    message("✓ Perfect alignment with direct estimates!")
  }
} else {
  message("\nDirect estimates file not found - skipping alignment check")
  message("Run R/direct_estimation/direct_estimation.r first")
}

message("\n=== MiD covariate aggregation complete! ===")
message("\nOutput files:")
message("  1. df_mid_covariates_trips_mode.rds - Trip covariates (ags5 × mode)")
message("  2. df_mid_covariates_persons_ags5.rds - Person covariates (ags5)")
message("  3. df_mid_covariates_combined.rds - Combined (ags5 × mode with person vars)")
message("\nNext step: Update variable_selection scripts to use df_mid_covariates_combined.rds")
