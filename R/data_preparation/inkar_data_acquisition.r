################################################################################
# INKAR Data Acquisition via 'bonn' Package
#
# Purpose: Download INKAR indicators at district level (Kreise) for 2017
#          using the bonn R package API interface
#
# Package workflow:
#   1. get_geographies() - List available spatial levels
#   2. get_themes(geography) - List themes for a geography
#   3. get_variables(theme, geography) - List variables in a theme
#   4. get_data(variable, geography) - Download data for a variable
#   5. get_metadata(variable) - Get detailed variable description
#
# Input:  INKAR API (via bonn package)
# Output: data/temp/inkar_2017_districts.rds (401 districts × N indicators)
#
# Created: 2025-12-23
################################################################################

# Load packages ---------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,    # Data manipulation
  remotes       # Install from GitHub
)

# Install bonn package if needed
if (!require("bonn")) {
  cat("Installing bonn package from GitHub...\n")
  remotes::install_github("sumtxt/bonn", force = TRUE)
  library(bonn)
}

# Set paths -------------------------------------------------------------------
data_path <- "./data"

# Configuration ---------------------------------------------------------------
TARGET_YEAR <- 2017  # Match MiD survey year
GEOGRAPHY <- "KRE"   # Kreise (districts)

# Optional: Disable SSL verification if needed on Linux
# Uncomment if SSL errors occur
# httr::set_config(httr::config(ssl_verifypeer = 0L))

cat("\n==================================================\n")
cat("INKAR Data Acquisition - Started", format(Sys.time()), "\n")
cat("==================================================\n\n")

cat("Configuration:\n")
cat("  Target year:", TARGET_YEAR, "\n")
cat("  Geography:", GEOGRAPHY, "(Kreise/Districts)\n\n")

# STEP 1: Get available geographies -------------------------------------------
cat("STEP 1: Checking available geographies...\n")

df_geographies <- bonn::get_geographies()
cat("  Available geographies:", nrow(df_geographies), "\n")

# Find Kreise geography
kre_info <- df_geographies %>% filter(ID == GEOGRAPHY)
if (nrow(kre_info) == 0) {
  stop("Geography '", GEOGRAPHY, "' not found!")
}
cat("  Selected:", kre_info$Kurzname, "(", kre_info$NGeb, "units )\n\n")

# STEP 2: Get all themes for districts ---------------------------------------
cat("STEP 2: Getting themes available for districts...\n")

df_themes <- bonn::get_themes(geography = GEOGRAPHY)
cat("  Found", nrow(df_themes), "themes\n\n")

cat("  Themes:\n")
print(df_themes %>% select(ID, Bereich, Unterbereich))
cat("\n")

# STEP 3: Get all variables for all themes -----------------------------------
cat("STEP 3: Collecting all variables across all themes...\n")
cat("  This will query", nrow(df_themes), "themes...\n\n")

# Loop through all themes and collect variables
list_variables <- list()
n_themes_success <- 0
n_themes_failed <- 0

for (i in 1:nrow(df_themes)) {
  theme_id <- df_themes$ID[i]
  theme_name <- df_themes$Bereich[i]
  theme_sub <- df_themes$Unterbereich[i]

  cat(sprintf("  [%2d/%2d] %s - %s (ID: %s) ... ",
              i, nrow(df_themes), theme_name, theme_sub, theme_id))

  tryCatch({
    # Get variables for this theme
    vars <- bonn::get_variables(theme = theme_id, geography = GEOGRAPHY)

    if (nrow(vars) > 0) {
      # Add theme information to each variable
      vars$theme_id <- theme_id
      vars$theme_name <- theme_name
      vars$theme_sub <- theme_sub

      list_variables[[i]] <- vars
      n_themes_success <- n_themes_success + 1
      cat(nrow(vars), "variables\n")
    } else {
      cat("no variables\n")
    }

  }, error = function(e) {
    n_themes_failed <<- n_themes_failed + 1
    cat("ERROR:", e$message, "\n")
  })
}

cat("\n  Summary:\n")
cat("    Themes queried:", nrow(df_themes), "\n")
cat("    Successful:", n_themes_success, "\n")
cat("    Failed:", n_themes_failed, "\n\n")

# Combine all variables into one dataframe
df_variables_all <- bind_rows(list_variables)

cat("  Total variables collected (with duplicates):", nrow(df_variables_all), "\n")

# Check for variables appearing in multiple themes (same Gruppe ID)
dup_vars_check <- df_variables_all %>%
  count(Gruppe, sort = TRUE) %>%
  filter(n > 1)

if (nrow(dup_vars_check) > 0) {
  cat("  Found", nrow(dup_vars_check), "variables appearing in multiple themes\n")
  cat("  These will be deduplicated (keeping first occurrence)\n\n")

  # Show top duplicates
  cat("  Top duplicated variables:\n")
  print(head(dup_vars_check, 10))
  cat("\n")
}

# Deduplicate by Gruppe (variable ID) - keep first occurrence
df_variables <- df_variables_all %>%
  group_by(Gruppe) %>%
  slice(1) %>%
  ungroup()

cat("  After deduplication:", nrow(df_variables), "unique variables\n")
cat("  Sample:\n")
print(df_variables %>%
        select(Gruppe, KurznamePlus, theme_name, Zeitreihe) %>%
        head(10))
cat("\n")

# Save variable metadata
var_meta_file <- file.path(data_path, "temp", "inkar_variable_metadata.rds")
dir.create(dirname(var_meta_file), recursive = TRUE, showWarnings = FALSE)
saveRDS(df_variables, var_meta_file)
cat("  Saved variable metadata:", var_meta_file, "\n\n")

# STEP 4: Download data for all variables ------------------------------------
cat("STEP 4: Downloading data for all variables...\n")
cat("  Note: This will download", nrow(df_variables), "variables\n")
cat("  Estimated time: 5-15 minutes\n\n")

# Initialize tracking
list_data <- list()
n_success <- 0
n_failed <- 0
failed_variables <- character()

# Progress bar setup
pb <- txtProgressBar(min = 0, max = nrow(df_variables), style = 3)

for (i in 1:nrow(df_variables)) {
  var_id <- df_variables$Gruppe[i]
  var_name <- df_variables$KurznamePlus[i]

  # Update progress bar
  setTxtProgressBar(pb, i)

  tryCatch({
    # Download data for this variable
    # Note: get_data returns data for ALL years, we'll filter later
    df_var <- bonn::get_data(variable = var_id, geography = GEOGRAPHY)

    if (nrow(df_var) > 0) {
      # Convert Zeit to numeric for comparison
      df_var <- df_var %>%
        mutate(Zeit_num = as.numeric(Zeit))

      # Try to get target year, otherwise get closest year
      df_target <- df_var %>% filter(Zeit_num == TARGET_YEAR)

      if (nrow(df_target) == 0) {
        # Target year not available, find closest year
        available_years <- unique(df_var$Zeit_num)
        closest_year <- available_years[which.min(abs(available_years - TARGET_YEAR))]
        df_target <- df_var %>% filter(Zeit_num == closest_year)
      }

      # Clean and format
      df_var_2017 <- df_target %>%
        select(
          ags5 = Schlüssel,
          value = Wert,
          year = Zeit_num
        ) %>%
        mutate(
          # Ensure ags5 is 5-digit string with leading zeros
          ags5 = str_pad(as.character(ags5), width = 5, side = "left", pad = "0"),
          # Store variable identifiers
          var_id = var_id,
          var_name = var_name
        )

      # Only save if we have data for 2017
      if (nrow(df_var_2017) > 0) {
        list_data[[var_id]] <- df_var_2017
        n_success <- n_success + 1
      } else {
        failed_variables <- c(failed_variables,
                             paste0(var_id, " (", var_name, ") - No 2017 data"))
        n_failed <- n_failed + 1
      }
    } else {
      failed_variables <- c(failed_variables,
                           paste0(var_id, " (", var_name, ") - No data returned"))
      n_failed <- n_failed + 1
    }

  }, error = function(e) {
    failed_variables <<- c(failed_variables,
                          paste0(var_id, " (", var_name, ") - Error: ", e$message))
    n_failed <<- n_failed + 1
  })
}

close(pb)

cat("\n\n  Download summary:\n")
cat("    Total variables:", nrow(df_variables), "\n")
cat("    Successfully downloaded:", n_success, "\n")
cat("    Failed:", n_failed, "\n\n")

# Report failed variables
if (length(failed_variables) > 0) {
  cat("  Failed variables (first 20):\n")
  for (fail in head(failed_variables, 20)) {
    cat("    -", fail, "\n")
  }
  if (length(failed_variables) > 20) {
    cat("    ... and", length(failed_variables) - 20, "more\n")
  }
  cat("\n")
}

# STEP 5: Combine into wide format -------------------------------------------
cat("STEP 5: Combining all variables into wide format...\n")

# Combine all data into long format
df_long <- bind_rows(list_data)

cat("  Long format:\n")
cat("    Rows:", nrow(df_long), "\n")
cat("    Unique variables:", n_distinct(df_long$var_id), "\n")
cat("    Unique districts:", n_distinct(df_long$ags5), "\n\n")

# Create clean variable names
df_long <- df_long %>%
  mutate(
    # Clean variable names: lowercase, remove special chars
    var_clean = str_to_lower(var_name),
    var_clean = str_replace_all(var_clean, "ä", "ae"),
    var_clean = str_replace_all(var_clean, "ö", "oe"),
    var_clean = str_replace_all(var_clean, "ü", "ue"),
    var_clean = str_replace_all(var_clean, "ß", "ss"),
    var_clean = str_replace_all(var_clean, "[^a-z0-9_]", "_"),
    var_clean = str_replace_all(var_clean, "_+", "_"),
    var_clean = str_remove(var_clean, "^_|_$"),
    # Add prefix to avoid name conflicts with MiD/census data
    var_clean = paste0("inkar_", var_clean)
  )

# Check for duplicate variable names within districts
dup_check <- df_long %>%
  count(ags5, var_clean) %>%
  filter(n > 1)

if (nrow(dup_check) > 0) {
  cat("  ⚠ Found", n_distinct(dup_check$var_clean),
      "variables with duplicate entries per district\n")

  # Check if duplicates have DIFFERENT values (not just repeated same value)
  vars_with_diff_values <- df_long %>%
    filter(var_clean %in% dup_check$var_clean) %>%
    group_by(ags5, var_clean) %>%
    summarize(
      n_values = n(),
      n_unique_values = n_distinct(value),
      has_diff_values = n_unique_values > 1,
      .groups = "drop"
    ) %>%
    filter(has_diff_values) %>%
    pull(var_clean) %>%
    unique()

  # Check if duplicates have different years
  vars_with_diff_years <- df_long %>%
    filter(var_clean %in% dup_check$var_clean) %>%
    group_by(ags5, var_clean) %>%
    summarize(
      n_years = n_distinct(year),
      has_diff_years = n_years > 1,
      .groups = "drop"
    ) %>%
    filter(has_diff_years) %>%
    pull(var_clean) %>%
    unique()

  if (length(vars_with_diff_years) > 0) {
    cat("  ⚠ WARNING:", length(vars_with_diff_years),
        "variables have DIFFERENT YEARS for the same district!\n")
    cat("  This shouldn't happen - investigating:\n\n")

    for (var in vars_with_diff_years) {
      example_ags5 <- df_long %>%
        filter(var_clean == var) %>%
        group_by(ags5) %>%
        filter(n_distinct(year) > 1) %>%
        pull(ags5) %>%
        first()

      example_data <- df_long %>%
        filter(var_clean == var, ags5 == example_ags5) %>%
        select(year, value)

      cat("    Variable:", var, "\n")
      cat("      District", example_ags5, "has years:", paste(example_data$year, collapse = ", "), "\n")
      cat("      With values:", paste(example_data$value, collapse = ", "), "\n\n")
    }
  }

  if (length(vars_with_diff_values) > 0) {
    cat("  ⚠ WARNING:", length(vars_with_diff_values),
        "variables have DIFFERENT values for the same district\n")
    cat("  These will be averaged:\n\n")

    # Show details for each variable with different values
    for (var in vars_with_diff_values) {
      cat("    Variable:", var, "\n")

      # Get example of differences for first district that has them
      example <- df_long %>%
        filter(var_clean == var) %>%
        group_by(ags5) %>%
        filter(n_distinct(value) > 1) %>%
        slice(1) %>%
        ungroup() %>%
        head(1)

      if (nrow(example) > 0) {
        example_ags5 <- example$ags5[1]
        example_data <- df_long %>%
          filter(var_clean == var, ags5 == example_ags5) %>%
          select(year, value)

        cat("      Example (district", example_ags5, "):\n")
        cat("        Years:", paste(example_data$year, collapse = ", "), "\n")
        cat("        Values:", paste(example_data$value, collapse = ", "), "\n")
        cat("        Will be averaged to:", mean(example_data$value, na.rm = TRUE), "\n\n")
      }
    }
  } else {
    cat("  ✓ All duplicates have identical values (safe to deduplicate)\n\n")
  }

  # Aggregate duplicates
  # If same year: just average values
  # If different years: average across years (shouldn't happen but handle it)
  df_long <- df_long %>%
    group_by(ags5, var_clean) %>%
    summarize(
      value = mean(value, na.rm = TRUE),
      year = first(year),  # Keep first year
      .groups = "drop"
    )

  cat("  After deduplication:", nrow(df_long), "rows\n\n")
}

# Pivot to wide format
cat("  Pivoting to wide format...\n")

df_wide <- df_long %>%
  select(ags5, var_clean, value) %>%
  pivot_wider(
    names_from = var_clean,
    values_from = value,
    values_fill = NA
  ) %>% 
  #filter out any cols with krankenhaus in the name:
  select(-contains("krankenhaus"))

cat("  Wide format:\n")
cat("    Rows (districts):", nrow(df_wide), "\n")
cat("    Columns (variables):", ncol(df_wide) - 1, "\n\n")

# STEP 6: Analyze variable quality -------------------------------------------
cat("STEP 6: Analyzing variable quality...\n")

# Check which variables used non-target years
year_summary <- df_long %>%
  group_by(var_clean) %>%
  filter(!str_detect(var_clean, "krankenhaus")) %>% 
  summarize(
    year_used = first(year),
    n_districts = n(),
    .groups = "drop"
  )

n_non_target <- sum(year_summary$year_used != TARGET_YEAR, na.rm = TRUE)
if (n_non_target > 0) {
  cat("  Note:", n_non_target, "variables used closest available year (not", TARGET_YEAR, ")\n")
  cat("  Year distribution:\n")
  print(year_summary %>% count(year_used, sort = TRUE))
  cat("\n")
}

# Calculate quality metrics for each variable
var_quality <- tibble(
  variable = names(df_wide)[-1],  # Exclude ags5
  n_missing = sapply(df_wide[-1], function(x) sum(is.na(x))),
  pct_missing = round(n_missing / nrow(df_wide) * 100, 2),
  n_unique = sapply(df_wide[-1], function(x) n_distinct(x, na.rm = TRUE)),
  min_val = sapply(df_wide[-1], function(x) min(x, na.rm = TRUE)),
  max_val = sapply(df_wide[-1], function(x) max(x, na.rm = TRUE)),
  mean_val = sapply(df_wide[-1], function(x) mean(x, na.rm = TRUE)),
  sd_val = sapply(df_wide[-1], function(x) sd(x, na.rm = TRUE))
) %>%
  # Add year information
  left_join(year_summary, by = c("variable" = "var_clean")) %>%
  mutate(
    # Coefficient of variation (for variables with non-zero mean)
    cv = ifelse(abs(mean_val) > 1e-10, abs(sd_val / mean_val), NA),
    # Check for variance
    has_variance = sd_val > 0 & !is.na(sd_val),
    # Flag non-target year
    is_target_year = year_used == TARGET_YEAR
  ) %>%
  arrange(pct_missing)

cat("  Variable quality summary:\n")
cat("    Total variables:", nrow(var_quality), "\n")
cat("    Complete (0% missing):", sum(var_quality$pct_missing == 0), "\n")
cat("    High quality (<10% missing):", sum(var_quality$pct_missing < 10), "\n")
cat("    Medium quality (10-50% missing):", sum(var_quality$pct_missing >= 10 & var_quality$pct_missing <= 50), "\n")
cat("    Low quality (>50% missing):", sum(var_quality$pct_missing > 50), "\n")
cat("    Zero variance:", sum(!var_quality$has_variance), "\n\n")

# Show top variables
cat("  Top 20 highest quality variables:\n")
print(var_quality %>%
        head(20) %>%
        select(variable, pct_missing, n_unique, has_variance))
cat("\n")

# STEP 7: Save outputs --------------------------------------------------------
cat("STEP 7: Saving outputs...\n")

# Ensure output directory exists
output_dir <- file.path(data_path, "temp")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Save wide format dataset
output_file <- file.path(output_dir, "inkar_2017_districts.rds")
saveRDS(df_wide, output_file)
cat("  Saved:", output_file, "\n")
cat("    Size:", round(file.size(output_file) / 1024^2, 2), "MB\n")
cat("    Dimensions:", nrow(df_wide), "rows ×", ncol(df_wide), "columns\n")

# Save variable quality metadata
quality_file <- file.path(output_dir, "inkar_variable_quality.rds")
saveRDS(var_quality, quality_file)
cat("  Saved:", quality_file, "\n")

# Save variable quality as CSV for inspection
quality_csv <- file.path(output_dir, "inkar_variable_quality.csv")
write_csv(var_quality, quality_csv)
cat("  Saved:", quality_csv, "\n")

# Save failed variables list if any
if (length(failed_variables) > 0) {
  failed_file <- file.path(output_dir, "inkar_failed_variables.txt")
  writeLines(failed_variables, failed_file)
  cat("  Saved:", failed_file, "\n")
}

cat("\n")

# STEP 8: Validate district coverage -----------------------------------------
cat("STEP 8: Validating district coverage...\n")

# Expected number of districts in Germany
# Note: INKAR 2021 is on 2019 boundaries, but should have ~400 districts
EXPECTED_DISTRICTS <- 401

cat("  Expected districts:", EXPECTED_DISTRICTS, "\n")
cat("  Found districts:", nrow(df_wide), "\n")

coverage_pct <- round(nrow(df_wide) / EXPECTED_DISTRICTS * 100, 1)

if (nrow(df_wide) == EXPECTED_DISTRICTS) {
  cat("  ✓ District count matches expected\n")
} else if (nrow(df_wide) >= EXPECTED_DISTRICTS - 5) {
  cat("  ⚠ Close to expected (", coverage_pct, "% coverage)\n")
  cat("  Note: INKAR uses 2019 boundaries, slight differences expected\n")
} else if (nrow(df_wide) < EXPECTED_DISTRICTS) {
  cat("  ⚠ WARNING: Missing", EXPECTED_DISTRICTS - nrow(df_wide),
      "districts (", coverage_pct, "% coverage)\n")
} else {
  cat("  ⚠ WARNING: Extra", nrow(df_wide) - EXPECTED_DISTRICTS, "districts found\n")
}

# Check for invalid ags5 codes
invalid_ags5 <- df_wide %>%
  filter(is.na(ags5) | ags5 == "" | nchar(ags5) != 5)

if (nrow(invalid_ags5) > 0) {
  cat("  ⚠ WARNING:", nrow(invalid_ags5), "districts have invalid ags5 codes\n")
  print(invalid_ags5 %>% select(ags5))
} else {
  cat("  ✓ All districts have valid 5-digit ags5 codes\n")
}

cat("\n")

# Summary ---------------------------------------------------------------------
cat("==================================================\n")
cat("INKAR Data Acquisition - Completed", format(Sys.time()), "\n")
cat("==================================================\n\n")

cat("Summary:\n")
cat("  Variables attempted:", nrow(df_variables), "\n")
cat("  Successfully downloaded:", n_success, "\n")
cat("  Failed:", n_failed, "\n")
cat("  Output districts:", nrow(df_wide), "\n")
cat("  Output variables:", ncol(df_wide) - 1, "\n")
cat("  Year:", TARGET_YEAR, "\n\n")

cat("Data quality:\n")
cat("  Complete variables:", sum(var_quality$pct_missing == 0), "\n")
cat("  High quality (<10% missing):", sum(var_quality$pct_missing < 10), "\n")
cat("  Variables with variance:", sum(var_quality$has_variance), "\n\n")

cat("Key outputs:\n")
cat("  1. District data:", output_file, "\n")
cat("  2. Variable quality:", quality_file, "\n")
cat("  3. Quality CSV:", quality_csv, "\n")
if (length(failed_variables) > 0) {
  cat("  4. Failed variables:", failed_file, "\n")
}
cat("\n")

cat("Next steps:\n")
cat("  1. Review variable quality:", quality_csv, "\n")
cat("  2. Re-run variable_selection_perskm.r with INKAR covariates\n\n")

# Reset SSL config if it was changed
# httr::reset_config()

################################################################################
# END OF SCRIPT
################################################################################
