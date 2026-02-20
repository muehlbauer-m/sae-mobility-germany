# ==============================================================================
# Data Cleaning: Multimodal Person-Level Analysis (ags5 × hvm domain)
# ==============================================================================
# Purpose: Clean and prepare MiD 2017 data for multimodal FH model
# Date: 2025-11-25
#
# This script:
# 1. Loads and cleans person-level data (MiD2017_Regional_Personen.csv)
# 2. Loads and cleans trip-level data (MiD2017_Regional_Wege.csv)
# 3. Merges MiD-Gemeindetyp stratification variable (VBGEM + RegioStaR17 fallback)
# 4. Adjusts calibrated weights after filtering
# 5. Merges person data to trips
# 6. Creates ags5 × hvm domain structure for multimodal analysis
# 7. Handles error codes and missing values
# 8. Standardizes district codes (AGS5)
#
# Model domain: ags5 (401 districts) × hvm (4 modes) = ~1600 observations
# Modes: fuss, fahrrad, miv (driver + passenger), oepv
# ==============================================================================

# Setup ------------------------------------------------------------------------
rm(list = ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table)

data_path <- "./data"
output_dir <- file.path(data_path, "data_cleaning")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ==============================================================================
# STEP 1: Load and Clean Person Data
# ==============================================================================

MiD2017_Regional_Personen_raw <- read_csv2(
  file.path(data_path, "raw/MiD2017_Regional_Personen.csv")
)

# Clean person data ------------------------------------------------------------

# Error codes in person data (from codebook):
# - anzwege3: 803 = "Person ohne Wegerfassung" (person without trip recording)
# - anzwege3: 804 = "Person mit unbekannter Mobilität" (person with unknown mobility)
# - perskm1: 80803.00 = person without trip recording
# - perskm1: 80804.00 = person with unknown mobility
# - KKZ (Kreiskennziffer): 7320 = Zweibrücken (excluded in original due to missing CASA covariates)
# - VBGEM (Verbandsgemeinde): 999999995 = missing/invalid code (20,188 persons, NOT filtered)
#   → These persons are classified via RegioStar7 fallback mapping in direct_estimation_wegkm.r

df_persons <- MiD2017_Regional_Personen_raw %>%
  # Rename KKZ to ags5 (Amtlicher Gemeindeschlüssel, 5 digits)
  rename(ags5 = KKZ) %>%
  # Convert key variables to numeric/character
  mutate(
    anzwege3 = as.numeric(anzwege3),
    perskm1 = as.numeric(perskm1),
    P_HOCH = as.numeric(P_HOCH),
    P_GEW = as.numeric(P_GEW),
    alter_gr5 = as.character(alter_gr5),
    ags5 = as.character(ags5)
  ) %>%
  # Handle missing age groups
  mutate(
    alter_gr5 = if_else(alter_gr5 == "99", NA_character_, alter_gr5)
  ) %>%
  # Create broader age groups for calibration (matching census)
  mutate(
    age_group = case_when(
      alter_gr5 %in% c("1", "2", "3") ~ "0-17",      # 0-17 Jahre
      alter_gr5 %in% c("4", "5", "6") ~ "18-34",     # 18-34 Jahre
      alter_gr5 %in% c("7", "8", "9") ~ "35-49",     # 35-49 Jahre
      alter_gr5 %in% c("10", "11", "12") ~ "50-64",  # 50-64 Jahre
      alter_gr5 %in% c("13", "14", "15") ~ "65+",    # 65+ Jahre
      TRUE ~ NA_character_
    )
  ) %>%
  # Recode household size to match census categories (1, 2, 3+)
  mutate(
    hhgr_gr2_original = hhgr_gr2,  # Keep original for reference
    hhgr_gr2 = case_when(
      hhgr_gr2 == 1 ~ "1",
      hhgr_gr2 == 2 ~ "2",
      hhgr_gr2 %in% c(3, 4) ~ "3+",  # Collapse 3 and 4+ to match census
      TRUE ~ NA_character_
    )
  ) %>%
  # Filter out "keine Angabe" (9) for sex variable
  # Only 140 respondents
  filter(HP_SEX != 9) %>%
  # Filter out invalid age groups NA:
  filter(!is.na(alter_gr5)) %>%
  # Standardize ags5 to 5 digits
  mutate(
    ags5 = if_else(nchar(ags5) == 4, paste0("0", ags5), ags5)
  ) %>%
  # Handle Göttingen district consolidation (03152, 03156 → 03159)
  mutate(
    ags5 = case_when(
      ags5 == "03152" ~ "03159",
      ags5 == "03156" ~ "03159",
      TRUE ~ ags5
    )
  ) %>%
  # Filter out Zweibrücken (now has census covariates, but excluded for consistency)
  # filter(ags5 != "07320") %>%
  # Handle error codes for anzwege3 (number of trips on survey day)
  mutate(
    anzwege3_original = anzwege3,
    anzwege3 = case_when(
      anzwege3 == 803 ~ NA_real_,  # Person ohne Wegerfassung
      anzwege3 == 804 ~ NA_real_,  # Person mit unbekannter Mobilität
      TRUE ~ anzwege3
    )
  ) %>%
  # Handle error codes for perskm1 (person km/day)
  mutate(
    perskm1_original = perskm1,
    perskm1 = case_when(
      perskm1 == 80803.00 ~ NA_real_,  # Person ohne Wegerfassung
      perskm1 == 80804.00 ~ NA_real_,  # Person mit unbekannter Mobilität
      TRUE ~ perskm1
    )
  ) %>%
  # Note: VBGEM 999999995 (missing/invalid, 20,188 persons) is NOT filtered
  # These persons will be classified using RegioStar7 fallback in direct_estimation_wegkm.r
  # Create quarter variable from survey month
  mutate(
    ST_QUARTAL = case_when(
      ST_MONAT %in% c(1, 2, 3) ~ 1,
      ST_MONAT %in% c(4, 5, 6) ~ 2,
      ST_MONAT %in% c(7, 8, 9) ~ 3,
      ST_MONAT %in% c(10, 11, 12) ~ 4,
      TRUE ~ NA_real_
    )
  ) %>%
  # Relocate key columns
  relocate(ags5, .after = HP_ID_Reg) %>%
  relocate(perskm1, .after = ags5) %>%
  relocate(anzwege3, .after = perskm1)

# ==============================================================================
# STEP 2: Load and Clean Trip Data
# ==============================================================================

MiD2017_Regional_Wege_raw <- read_csv2(
  file.path(data_path, "raw/MiD2017_Regional_Wege.csv")
)

# Clean trip data --------------------------------------------------------------

# Error codes in trip data (from codebook):
# - KKZ_SO, KKZ_ZO: 99995 = "nicht zuzuordnen" (cannot be assigned)
# - KKZ_SO, KKZ_ZO: 700701 = "bei regelmäßigen beruf. Weg nicht zu bestimmen"
#   (Not filtered: no missings in wegkm_imp for these codes, and origin/destination not needed for person-level analysis)
# - wegkm_imp: 9994, 9999, 70703 = missing/error codes
# - hvm: 9 = keine Angabe, 703 = Weg ohne Detailerfassung (exclude)

df_trips <- MiD2017_Regional_Wege_raw %>%
  # Rename KKZ to ags5 (Amtlicher Gemeindeschlüssel, 5 digits)
  rename(
    ags5 = KKZ,
    ags5_so = KKZ_SO,
    ags5_zo = KKZ_ZO
  ) %>%
  # Convert key variables to numeric
  mutate(
    wegkm_imp = as.numeric(wegkm_imp),
    W_HOCH = as.numeric(W_HOCH),
    W_GEW = as.numeric(W_GEW),
    ags5 = as.character(ags5),
    ags5_so = as.character(ags5_so),
    ags5_zo = as.character(ags5_zo)
  ) %>%
  # Filter out error codes in origin/destination districts (not needed for person-level analysis)
  # filter(
  #   !ags5_so %in% c("99995", "700701"),
  #   !ags5_zo %in% c("99995", "700701")
  # ) %>%
  # Filter out error codes in trip distance
  filter(!wegkm_imp %in% c(9994, 9999, 70703)) %>%
  # Filter out invalid mode codes
  filter(!hvm %in% c(9, 703)) %>%
  # Create clean mode variable (merge MIV driver and passenger)
  mutate(
    hvm_label = case_when(
      hvm == 1 ~ "fuss",
      hvm == 2 ~ "fahrrad",
      hvm %in% c(3, 4) ~ "miv",  # Merge driver and passenger
      hvm == 5 ~ "oepv",
      TRUE ~ NA_character_
    )
  ) %>%
  # Standardize district codes to 5 digits
  mutate(
    ags5 = if_else(nchar(ags5) == 4, paste0("0", ags5), ags5),
    ags5_so = if_else(nchar(ags5_so) == 4, paste0("0", ags5_so), ags5_so),
    ags5_zo = if_else(nchar(ags5_zo) == 4, paste0("0", ags5_zo), ags5_zo)
  ) %>%
  # Handle Göttingen district consolidation
  mutate(
    ags5 = case_when(
      ags5 %in% c("03152", "03156") ~ "03159",
      TRUE ~ ags5
    ),
    ags5_so = case_when(
      ags5_so %in% c("03152", "03156") ~ "03159",
      TRUE ~ ags5_so
    ),
    ags5_zo = case_when(
      ags5_zo %in% c("03152", "03156") ~ "03159",
      TRUE ~ ags5_zo
    )
  ) %>%
  # Filter out Zweibrücken (now has census covariates, but excluded for consistency)
  # filter(ags5_so != "07320") %>%
  # Create quarter variable
  mutate(
    ST_QUARTAL = case_when(
      ST_MONAT %in% c(1, 2, 3) ~ 1,
      ST_MONAT %in% c(4, 5, 6) ~ 2,
      ST_MONAT %in% c(7, 8, 9) ~ 3,
      ST_MONAT %in% c(10, 11, 12) ~ 4,
      TRUE ~ NA_real_
    )
  ) %>%
  # Relocate key columns
  relocate(c(ags5, ags5_so, ags5_zo), .after = HP_ID_Reg)

# ==============================================================================
# STEP 3: Merge MiD-Gemeindetyp Stratification Variable
# ==============================================================================

# Load MiD-Gemeindetyp lookup tables for stratification
# Primary: VBGEM-based lookup (4,603 VWG codes)
# Fallback: RegioStar17-based mapping for VBGEM codes not in BBSR 2021 data
message("\n=== Loading MiD-Gemeindetyp Lookup Tables ===")

lookup_mid_gemeindetyp <- readRDS(
  file.path(data_path, "raw/lookup_mid_gemeindetyp_vbgem.rds")
)
lookup_regiostar_fallback <- readRDS(
  file.path(data_path, "raw/lookup_regiostar_fallback.rds")
)

message("VBGEM lookup: ", nrow(lookup_mid_gemeindetyp), " VWG codes")
message("RegioStaR17 fallback: ", nrow(lookup_regiostar_fallback), " categories")

# Merge MiD-Gemeindetyp with person data using VBGEM
# Note: Convert vwg (character) to numeric to match VBGEM type in df_persons
message("\n=== Merging MiD-Gemeindetyp with Person Data ===")

df_persons <- df_persons %>%
  left_join(
    lookup_mid_gemeindetyp %>%
      select(vwg, mid_gemeindetyp, mid_gemeindetyp_code) %>%
      mutate(VBGEM = as.numeric(vwg)) %>%
      select(-vwg),
    by = "VBGEM"
  ) %>%
  # Apply RegioStar17 fallback for missing VBGEM codes
  # (Due to municipal reforms between MiD 2017 survey and BBSR 2021 reference)
  left_join(
    lookup_regiostar_fallback %>%
      select(RegioStaR17, mid_gemeindetyp_code_fallback),
    by = "RegioStaR17"
  ) %>%
  mutate(
    mid_gemeindetyp_code = coalesce(mid_gemeindetyp_code, mid_gemeindetyp_code_fallback)
  ) %>%
  select(-mid_gemeindetyp_code_fallback)

message("Primary merge (VBGEM): ", sum(!is.na(df_persons$mid_gemeindetyp)), " persons")
message("Fallback applied (RegioStaR17): ", sum(is.na(df_persons$mid_gemeindetyp) & !is.na(df_persons$mid_gemeindetyp_code)), " persons")
message("Total coverage: ", sum(!is.na(df_persons$mid_gemeindetyp_code)), " / ", nrow(df_persons), " persons")
message("\nDistribution of MiD-Gemeindetyp:")
print(table(df_persons$mid_gemeindetyp_code, useNA = "ifany"))

# ==============================================================================
# STEP 4: Adjust Calibrated Weights After Filtering
# ==============================================================================

# Calculate adjustment factor for trips
# (Weight before filtering / Weight after filtering)
trip_weight_before <- sum(as.numeric(MiD2017_Regional_Wege_raw$W_HOCH), na.rm = TRUE)
trip_weight_after <- sum(df_trips$W_HOCH, na.rm = TRUE)
trip_weight_factor <- trip_weight_before / trip_weight_after

df_trips <- df_trips %>%
  mutate(
    W_HOCH_original = W_HOCH,
    W_HOCH = W_HOCH * trip_weight_factor
  )

# Calculate adjustment factor for persons
person_weight_before <- sum(as.numeric(MiD2017_Regional_Personen_raw$P_HOCH), na.rm = TRUE)
person_weight_after <- sum(df_persons$P_HOCH, na.rm = TRUE)
person_weight_factor <- person_weight_before / person_weight_after

df_persons <- df_persons %>%
  mutate(
    P_HOCH_original = P_HOCH,
    P_HOCH = P_HOCH * person_weight_factor
  )

# ==============================================================================
# STEP 5: Merge Person Data to Trips
# ==============================================================================

# Filter trips to only include trips from persons in df_persons
# (Removes trips from persons filtered out due to HP_SEX == 9 or missing alter_gr5)
df_trips <- df_trips %>%
  semi_join(df_persons, by = "HP_ID_Reg")

# Merge all person data to trips (avoiding duplicate columns)
df_trips_with_persons <- df_trips %>%
  left_join(
    df_persons,
    by = "HP_ID_Reg",
    suffix = c("_trip", "_person")
  )

# ==============================================================================
# STEP 6: Save Outputs
# ==============================================================================

# Save trip-level dataset with person data
saveRDS(df_trips_with_persons, file.path(output_dir, "df_trips_with_persons.rds"))

# Save person-level dataset separately (for calibration)
saveRDS(df_persons, file.path(output_dir, "df_persons.rds"))

# # Save summary statistics
# summary_stats <- list(
#   # Sample sizes
#   n_persons = nrow(df_persons),
#   n_trips = nrow(df_trips_with_persons),

#   # Mode distribution
#   trips_by_mode = df_trips_with_persons %>% count(hvm_label) %>% deframe(),

#   # District counts
#   n_districts = length(unique(df_trips_with_persons$ags5_trip)),
#   districts = sort(unique(df_trips_with_persons$ags5_trip)),

#   # Weights
#   total_person_weight = sum(df_persons$P_HOCH, na.rm = TRUE),
#   total_trip_weight = sum(df_trips_with_persons$W_HOCH, na.rm = TRUE)
# )

# saveRDS(summary_stats, file.path(output_dir, "cleaning_summary_stats.rds"))

# ==============================================================================
# END OF SCRIPT
# ==============================================================================
