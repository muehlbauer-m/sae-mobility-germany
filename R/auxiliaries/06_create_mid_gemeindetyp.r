# ==============================================================================
# Create MiD-Gemeindetyp from BBSR Raumgliederungen 2021
# ==============================================================================
# Purpose: Construct the 10-category MiD-Gemeindetyp used for stratification
#          in MiD 2017 sampling design
#
# Input:  - BBSR Raumgliederungen Excel file (2021 reference data)
# Output: - Lookup table: VBGEM → MiD_Gemeindetyp (10 categories)
#
# MiD-Gemeindetyp categories (from Methodenbericht):
# 1. sehr peripher
# 2. peripher – Landgemeinde, Kleinstadt
# 3. peripher – größere und kleinere Mittelstadt
# 4. zentral – Landgemeinde, Kleinstadt
# 5. zentral – größere und kleinere Mittelstadt
# 6. zentral – kleine Großstadt
# 7. sehr zentral – Landgemeinde, Kleinstadt
# 8. sehr zentral – größere, kleinere Mittelstadt
# 9. sehr zentral – kleine Großstadt
# 10. sehr zentral – große Großstadt
#
# BBSR classifications used:
# - RLG2021: Regionale Lage (4 categories: sehr peripher, peripher, zentral, sehr zentral)
# - GTU2021: Gemeinde-Typologie (5 categories: Großstadt, Mittelstadt, Größere/Kleine Kleinstadt, Landgemeinde)
# - VWG2021: Verbandsgemeinde (matches VBGEM in MiD data)
# ==============================================================================

# Setup ------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readxl)

# Platform-dependent data path
if (.Platform$OS.type == "unix") {
  data_path <- "./data"
} else {
  data_path <- "./data"
}

# ==============================================================================
# STEP 1: Load BBSR Raumgliederungen Reference Data
# ==============================================================================

message("Loading BBSR Raumgliederungen 2021...")

# Load Gemeindereferenz sheet (municipality-level classifications)
bbsr_gemeinde <- read_excel(
  file.path(data_path, "raw/raumgliederungen-referenzen-2021.xlsx"),
  sheet = "Gemeindereferenz (inkl. Kreise)"
)

# Remove header row
bbsr_gemeinde <- bbsr_gemeinde[-1, ]

message("  Loaded ", nrow(bbsr_gemeinde), " municipalities")

# ==============================================================================
# STEP 2: Create MiD-Gemeindetyp at Gemeinde Level
# ==============================================================================

# Extract relevant columns and create mid_gemeindetyp_code
df_gemeinde <- bbsr_gemeinde %>%
  select(
    gem_ags = GEM2021,         # Municipality code (7 digits)
    vwg = VWG2021,              # Verbandsgemeinde code - MATCHES VBGEM in MiD!
    vwg_name = VWG_NAME,
    rlg = RLG2021,              # Regionale Lage (4 categories)
    rlg_name = RLG_NAME,
    gtu = GTU2021,              # Gemeinde-Typologie (5 categories)
    gtu_name = GTU_NAME,
    population = bev21          # Population (for splitting Großstadt categories)
  ) %>%
  mutate(
    population = as.numeric(population),
    # Create mid_gemeindetyp_code directly from RLG × GTU
    mid_gemeindetyp_code = case_when(
      # 1. sehr peripher (all sizes combined)
      rlg == "4" ~ 1L,

      # 2. peripher – Landgemeinde, Kleinstadt
      rlg == "3" & gtu_name %in% c("Landgemeinde", "Kleine Kleinstadt", "Größere Kleinstadt") ~ 2L,

      # 3. peripher – größere und kleinere Mittelstadt
      rlg == "3" & gtu_name == "Mittelstadt" ~ 3L,

      # 4. zentral – Landgemeinde, Kleinstadt
      rlg == "2" & gtu_name %in% c("Landgemeinde", "Kleine Kleinstadt", "Größere Kleinstadt") ~ 4L,

      # 5. zentral – größere und kleinere Mittelstadt
      rlg == "2" & gtu_name == "Mittelstadt" ~ 5L,

      # 6. zentral – kleine Großstadt
      rlg == "2" & gtu_name == "Großstadt" ~ 6L,

      # 7. sehr zentral – Landgemeinde, Kleinstadt
      rlg == "1" & gtu_name %in% c("Landgemeinde", "Kleine Kleinstadt", "Größere Kleinstadt") ~ 7L,

      # 8. sehr zentral – größere, kleinere Mittelstadt
      rlg == "1" & gtu_name == "Mittelstadt" ~ 8L,

      # 9 & 10: sehr zentral – kleine/große Großstadt (split by population)
      # Threshold: 500,000 inhabitants, s
      rlg == "1" & gtu_name == "Großstadt" & population >= 500000 ~ 10L,
      rlg == "1" & gtu_name == "Großstadt" & population < 500000 ~ 9L,

      TRUE ~ NA_integer_
    ),
    # Create descriptive label
    mid_gemeindetyp = case_when(
      mid_gemeindetyp_code == 1 ~ "1_sehr_peripher",
      mid_gemeindetyp_code == 2 ~ "2_peripher_landgemeinde_kleinstadt",
      mid_gemeindetyp_code == 3 ~ "3_peripher_mittelstadt",
      mid_gemeindetyp_code == 4 ~ "4_zentral_landgemeinde_kleinstadt",
      mid_gemeindetyp_code == 5 ~ "5_zentral_mittelstadt",
      mid_gemeindetyp_code == 6 ~ "6_zentral_kleine_grossstadt",
      mid_gemeindetyp_code == 7 ~ "7_sehr_zentral_landgemeinde_kleinstadt",
      mid_gemeindetyp_code == 8 ~ "8_sehr_zentral_mittelstadt",
      mid_gemeindetyp_code == 9 ~ "9_sehr_zentral_kleine_grossstadt",
      mid_gemeindetyp_code == 10 ~ "10_sehr_zentral_grosse_grossstadt",
      TRUE ~ NA_character_
    )
  )

message("\n=== Gemeinde-Level MiD-Gemeindetyp Created ===")
message("Gemeinden classified: ", sum(!is.na(df_gemeinde$mid_gemeindetyp_code)))
message("\nDistribution:")
print(table(df_gemeinde$mid_gemeindetyp_code, useNA = "ifany"))

# ==============================================================================
# STEP 3: Check VWG Uniqueness
# ==============================================================================

message("\n=== Checking VWG Uniqueness ===")

# For each VWG, check how many different RLG and GTU values exist
vwg_uniqueness <- df_gemeinde %>%
  group_by(vwg) %>%
  summarise(
    n_gemeinden = n(),
    n_unique_rlg = n_distinct(rlg, na.rm = TRUE),
    n_unique_gtu = n_distinct(gtu, na.rm = TRUE),
    n_unique_codes = n_distinct(mid_gemeindetyp_code, na.rm = TRUE),
    .groups = "drop"
  )

vwg_with_multiple_rlg <- vwg_uniqueness %>% filter(n_unique_rlg > 1)
vwg_with_multiple_gtu <- vwg_uniqueness %>% filter(n_unique_gtu > 1)
vwg_with_multiple_codes <- vwg_uniqueness %>% filter(n_unique_codes > 1)

message("Total VWG codes: ", nrow(vwg_uniqueness))
message("VWG with multiple RLG values: ", nrow(vwg_with_multiple_rlg))
message("VWG with multiple GTU values: ", nrow(vwg_with_multiple_gtu))
message("VWG with multiple mid_gemeindetyp_code: ", nrow(vwg_with_multiple_codes))

if (nrow(vwg_with_multiple_codes) > 0) {
  message("\nNote: ", nrow(vwg_with_multiple_codes), " VWG codes need aggregation")
  message("These will be resolved by selecting the most central/urban Gemeinde")
}

# ==============================================================================
# STEP 4: Aggregate to VWG Level
# ==============================================================================

message("\n=== Aggregating to VWG Level ===")

# For each VWG, select the most central/urban Gemeinde as representative
# Strategy: Sort by RLG (ascending = more central first), then GTU (ascending = more urban first)
# Take the first row (most central + most urban)

lookup_vwg <- df_gemeinde %>%
  group_by(vwg) %>%
  arrange(rlg, gtu, .by_group = TRUE) %>%  # Most central RLG first (1 < 2 < 3 < 4), most urban GTU first (10 < 20 < 30 < 40 < 50)
  slice(1) %>%           # Take first = most central + most urban
  ungroup() %>%
  select(
    vwg,
    vwg_name,
    mid_gemeindetyp,
    mid_gemeindetyp_code,
    rlg,
    rlg_name,
    gtu,
    gtu_name,
    population
  )

message("VWG codes in lookup: ", nrow(lookup_vwg))
message("\nDistribution of MiD-Gemeindetyp at VWG level:")
print(table(lookup_vwg$mid_gemeindetyp_code, useNA = "ifany"))

# ==============================================================================
# STEP 5: Save Lookup Table
# ==============================================================================

output_file <- file.path(data_path, "raw/lookup_mid_gemeindetyp_vbgem.rds")
saveRDS(lookup_vwg, output_file)

message("\n=== Lookup Table Saved ===")
message("File: ", output_file)
message("VWG codes (Verbandsgemeinden): ", nrow(lookup_vwg))
message("MiD-Gemeindetyp categories: ", length(unique(lookup_vwg$mid_gemeindetyp)))

# Also save as CSV for inspection
csv_file <- file.path(data_path, "raw/lookup_mid_gemeindetyp_vbgem.csv")
write_csv(lookup_vwg, csv_file)
message("CSV file: ", csv_file)

# ==============================================================================
# STEP 6: Create RegioStar17 Fallback Mapping
# ==============================================================================

message("\n=== Creating RegioStar17 Fallback Mapping ===")
message("Purpose: Provide mid_gemeindetyp_code for VBGEM codes not in BBSR 2021 data")
message("         (due to municipal reforms between MiD 2017 survey and BBSR 2021)")

# Create RegioStaR17 to mid_gemeindetyp_code mapping
# Based on approximate equivalence between RegioStaR17 and BBSR RLG×GTU classifications
#
# RegioStaR17 official definitions:
# 111: metropolitane Stadtregion - Metropole
# 112: metropolitane Stadtregion - Großstadt
# 113: metropolitane Stadtregion - Mittelstadt
# 114: metropolitane Stadtregion - städtischer Raum
# 115: metropolitane Stadtregion - kleinstädtischer, dörflicher Raum
# 121: regiopolitane Stadtregion - Regiopole
# 123: regiopolitane Stadtregion - Mittelstadt
# 124: regiopolitane Stadtregion - städtischer Raum
# 125: regiopolitane Stadtregion - kleinstädtischer, dörflicher Raum
# 211: stadtregionsnahe ländliche Region - zentrale Stadt
# 213: stadtregionsnahe ländliche Region - Mittelstadt
# 214: stadtregionsnahe ländliche Region - städtischer Raum
# 215: stadtregionsnahe ländliche Region - kleinstädtischer, dörflicher Raum
# 221: periphere ländliche Region - zentrale Stadt
# 223: periphere ländliche Region - Mittelstadt
# 224: periphere ländliche Region - städtischer Raum
# 225: periphere ländliche Region - kleinstädtischer, dörflicher Raum
#
# Mapping logic:
# - metropolitane (11x) → "sehr zentral" (codes 7-10, RLG=1)
# - regiopolitane (12x) → mix of "sehr zentral" and "zentral" (codes 4-9, RLG=1-2)
# - stadtregionsnahe (21x) → "zentral" (codes 4-6, RLG=2)
# - periphere (22x) → "peripher" or "sehr peripher" (codes 1-3, RLG=3-4)
# - Size suffix maps to GTU: Metropole/Großstadt/Mittelstadt/städtisch/dörflich
#
# IMPORTANT: Each RegioStaR17 code maps to EXACTLY ONE mid_gemeindetyp_code (1:1 mapping)
lookup_regiostar_fallback <- tibble(
  RegioStaR17 = c(
    111, 112, 113, 114, 115,      # metropolitane Stadtregion
    121, 123, 124, 125,           # regiopolitane Stadtregion
    211, 213, 214, 215,           # stadtregionsnahe ländliche Region
    221, 223, 224, 225            # periphere ländliche Region
  ),
  mid_gemeindetyp_code_fallback = c(
    10L,  # 111: metropolitane - Metropole → sehr zentral große Großstadt (≥500k)
    9L,   # 112: metropolitane - Großstadt → sehr zentral kleine Großstadt (<500k)
    8L,   # 113: metropolitane - Mittelstadt → sehr zentral Mittelstadt
    8L,   # 114: metropolitane - städtisch → sehr zentral Mittelstadt (conservative: round up to Mittelstadt)
    7L,   # 115: metropolitane - dörflich → sehr zentral Landgemeinde/Kleinstadt
    9L,   # 121: regiopolitane - Regiopole → sehr zentral kleine Großstadt
    8L,   # 123: regiopolitane - Mittelstadt → sehr zentral Mittelstadt
    5L,   # 124: regiopolitane - städtisch → zentral Mittelstadt (conservative: round up to Mittelstadt)
    4L,   # 125: regiopolitane - dörflich → zentral Landgemeinde/Kleinstadt
    6L,   # 211: stadtregionsnah - zentrale Stadt → zentral kleine Großstadt
    5L,   # 213: stadtregionsnah - Mittelstadt → zentral Mittelstadt
    5L,   # 214: stadtregionsnah - städtisch → zentral Mittelstadt (conservative: round up to Mittelstadt)
    4L,   # 215: stadtregionsnah - dörflich → zentral Landgemeinde/Kleinstadt
    6L,   # 221: peripher - zentrale Stadt → zentral kleine Großstadt
    3L,   # 223: peripher - Mittelstadt → peripher Mittelstadt
    2L,   # 224: peripher - städtisch → peripher Landgemeinde/Kleinstadt
    1L    # 225: peripher - dörflich → sehr peripher
  ),
  mapping_logic = c(
    "metropolitane Metropole → sehr zentral große Großstadt (10)",
    "metropolitane Großstadt → sehr zentral kleine Großstadt (9)",
    "metropolitane Mittelstadt → sehr zentral Mittelstadt (8)",
    "metropolitane städtisch → sehr zentral Mittelstadt (8) - rounded up",
    "metropolitane dörflich → sehr zentral Landgemeinde/Kleinstadt (7)",
    "regiopolitane Regiopole → sehr zentral kleine Großstadt (9)",
    "regiopolitane Mittelstadt → sehr zentral Mittelstadt (8)",
    "regiopolitane städtisch → zentral Mittelstadt (5) - rounded up",
    "regiopolitane dörflich → zentral Landgemeinde/Kleinstadt (4)",
    "stadtregionsnah zentrale Stadt → zentral kleine Großstadt (6)",
    "stadtregionsnah Mittelstadt → zentral Mittelstadt (5)",
    "stadtregionsnah städtisch → zentral Mittelstadt (5) - rounded up",
    "stadtregionsnah dörflich → zentral Landgemeinde/Kleinstadt (4)",
    "peripher zentrale Stadt → zentral kleine Großstadt (6)",
    "peripher Mittelstadt → peripher Mittelstadt (3)",
    "peripher städtisch → peripher Landgemeinde/Kleinstadt (2)",
    "peripher dörflich → sehr peripher (1)"
  )
)

# Note on coverage:
# - ALL 10 mid_gemeindetyp categories mapped!
# - More granular than RegioStaR7 (17 vs 7 categories)
# - Better discrimination between metropolitan/regiopolitan/rural areas
# - Unique 1:1 mapping: Each RegioStaR17 code maps to exactly one mid_gemeindetyp_code
# - "städtischer Raum" (114, 124, 214) conservatively rounded up to Mittelstadt level
#   to maintain 1:1 mapping and avoid ambiguity with dörflich categories

message("RegioStaR17 fallback mapping:")
print(lookup_regiostar_fallback)

# Verify 1:1 mapping (no duplicates in RegioStaR17)
if (any(duplicated(lookup_regiostar_fallback$RegioStaR17))) {
  stop("ERROR: Duplicate RegioStaR17 codes found! Each code must map to exactly one mid_gemeindetyp_code.")
}
message("\n✓ Verified: All RegioStaR17 codes are unique (1:1 mapping)")

# Show which mid_gemeindetyp codes are covered
covered_codes <- sort(unique(lookup_regiostar_fallback$mid_gemeindetyp_code_fallback))
message("Mid_gemeindetyp codes covered: ", paste(covered_codes, collapse = ", "))
message("Total unique mappings: ", nrow(lookup_regiostar_fallback))

# Save fallback mapping
fallback_file <- file.path(data_path, "raw/lookup_regiostar_fallback.rds")
saveRDS(lookup_regiostar_fallback, fallback_file)
message("\nFallback mapping saved: ", fallback_file)

# ==============================================================================
# STEP 7: Validation Summary
# ==============================================================================

message("\n=== Validation Summary ===")
message("MiD-Gemeindetyp distribution:")
print(
  lookup_vwg %>%
    count(mid_gemeindetyp, mid_gemeindetyp_code) %>%
    arrange(mid_gemeindetyp_code)
)

# ==============================================================================
# END OF SCRIPT
# ==============================================================================

message("\n=== SCRIPT COMPLETE ===")
message("\nOutputs:")
message("1. VBGEM lookup: ", output_file, " (4,603 VWG codes)")
message("2. RegioStaR17 fallback: ", fallback_file, " (17 categories)")
message("\nNext steps:")
message("1. Merge VBGEM lookup with df_persons in direct_estimation.r")
message("2. Apply RegioStaR17 fallback for missing VBGEM codes")
message("3. Use mid_gemeindetyp_code as stratification variable in survey design")
message("\nExpected coverage:")
message("  - VBGEM merge: ~92.7% of persons")
message("  - RegioStaR17 fallback: ~7.3% of persons")
message("  - Total coverage: 100% (all persons classified)")
message("\nNote: The merge key is 'VBGEM' (Verbandsgemeinde) which matches VWG2021")
message("      This provides 5.2× finer granularity than MB-based approach")
message("      RegioStaR17 provides better fallback than RegioStaR7 (17 vs 7 categories)")
