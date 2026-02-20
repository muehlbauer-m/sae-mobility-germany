################################################################################
# Generate Census Variable Translations
#
# Purpose: Create a lookup table mapping census variable names to English
#          descriptions. This table is used by generate_final_varset_table.r
#          to provide English descriptions for Destatis variables.
#
# Input:
#   - data/raw/census/census_merged_2017.rds (to get all variable names)
#
# Output:
#   - data/Tex/Tables/tbl_census_translations.csv
#
# Note: This script generates a template with all variables. You should
#       manually review and improve the English translations as needed.
#       The script uses simple rules to auto-generate initial translations
#       which can then be refined.
#
# Created: 2026-01-23
################################################################################

# Load packages ---------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  stringr
)

# Set paths -------------------------------------------------------------------
data_path <- "./data"

cat("\n==================================================\n")
cat("Generate Census Variable Translations\n")
cat("==================================================\n\n")

# ==============================================================================
# STEP 1: Load census data and extract variable names
# ==============================================================================
cat("STEP 1: Loading census data...\n")

census_file <- file.path(data_path, "raw/census/census_merged_2017.rds")
if (!file.exists(census_file)) {
  stop("Census data not found: ", census_file)
}

census <- readRDS(census_file)
var_names <- names(census)

# Filter to only census_ prefixed variables (exclude ags5)
census_vars <- var_names[str_detect(var_names, "^census_")]
cat("  Found ", length(census_vars), " census variables\n\n", sep = "")

# ==============================================================================
# STEP 2: Create translation function
# ==============================================================================
cat("STEP 2: Generating English translations...\n")

# German to English translation lookup for common terms
german_english <- c(
  # Land use / area types
  "bodenflaeche" = "Land area",
  "fliessgewaesser" = "flowing water",
  "stehendes_gewaesser" = "standing water",
  "gruenanlage" = "green spaces/parks",
  "industrie" = "industrial",
  "gewerbeflaeche" = "commercial area",
  "wald" = "forest",
  "landwirtschaft" = "agricultural",
  "wohnbauflaeche" = "residential",
  "verkehrsflaeche" = "transport area",
  "erholungsflaeche" = "recreational area",
  "friedhof" = "cemetery",
  "bergbaubetrieb" = "mining",
  "tagebau" = "open-pit mining",
  "halde" = "spoil heap",
  "unland" = "wasteland",
  "sumpf" = "swamp",
  "heide" = "heathland",
  "moor" = "moor/bog",
  "gehoelz" = "woodland/copse",

  # Buildings
  "gebaeude" = "Buildings",
  "wohnung" = "dwelling",
  "wohnungen" = "dwellings",
  "wohngebaeude" = "Residential buildings",
  "nichtwohngebaeude" = "Non-residential buildings",
  "neubau" = "New construction",
  "fertigstellung" = "Completion",
  "baugenehmigung" = "Building permit",
  "baufertigstellung" = "Building completion",
  "bestand" = "Stock",

  # Population
  "bevoelkerungsstand" = "Population",
  "bevoelkerung" = "Population",
  "einwohner" = "Inhabitants",
  "maennlich" = "male",
  "weiblich" = "female",
  "total" = "total",
  "gesamt" = "total",
  "deutsch" = "German",
  "auslaender" = "foreign",
  "alter" = "age",
  "jahre" = "years",
  "unter" = "under",
  "bis" = "to",

  # Demographics
  "haushalte" = "Households",
  "haushalt" = "Household",
  "haushaltsgroesse" = "Household size",
  "personen" = "persons",
  "person" = "person",
  "lebendgeborene" = "Live births",
  "gestorbene" = "Deaths",
  "geburten" = "Births",
  "sterbefaelle" = "Deaths",
  "wanderung" = "Migration",
  "zuzuege" = "In-migration",
  "fortzuege" = "Out-migration",
  "durchschnittsalter" = "Mean age",
  "medianalter" = "Median age",
  "jugendquotient" = "Youth dependency ratio",
  "altenquotient" = "Old-age dependency ratio",

  # Employment
  "erwerbstaetige" = "Employed persons",
  "beschaeftigte" = "Employees",
  "arbeitsort" = "workplace",
  "wohnort" = "residence",
  "sozialversicherungspflichtig" = "Subject to social insurance",
  "vollzeit" = "full-time",
  "teilzeit" = "part-time",
  "arbeitslose" = "Unemployed",
  "arbeitslosenquote" = "Unemployment rate",
  "wirtschaftsbereich" = "Economic sector",
  "branche" = "Industry",
  "nationalitaet" = "Nationality",
  "geschlecht" = "Sex",
  "ausbildung" = "Education",
  "beruf" = "Occupation",

  # Income / Economy
  "einkommen" = "Income",
  "verfuegbares_einkommen" = "Disposable income",
  "steuerpflichtige" = "Taxpayers",
  "einkommensteuer" = "Income tax",
  "lohnsteuer" = "Wage tax",
  "gesamtbetrag" = "Total amount",
  "einkuenfte" = "Earnings",
  "hebesatz" = "Tax rate",
  "realsteuer" = "Property tax",
  "gewerbesteuer" = "Trade tax",
  "grundsteuer" = "Property tax",

  # Transport / Mobility
  "fahrzeug" = "Vehicle",
  "fahrzeuge" = "Vehicles",
  "fahrzeugbestand" = "Vehicle stock",
  "pkw" = "Cars",
  "kraftrad" = "Motorcycle",
  "kraftfahrzeug" = "Motor vehicle",
  "zulassung" = "Registration",

  # Tourism
  "gaesteuebernachtungen" = "Guest overnight stays",
  "gaestankuenfte" = "Guest arrivals",
  "beherbergung" = "Accommodation",
  "beherbergungsbetriebe" = "Accommodation establishments",
  "schlafgelegenheiten" = "Beds",
  "betriebsart" = "Type of establishment",
  "herkunft" = "Origin",

  # Infrastructure
  "flaeche" = "Area",
  "siedlung" = "Settlement",
  "verkehr" = "Transport",
  "nutzung" = "Use",
  "nutzungsart" = "Type of use",

  # Education / Social
  "schule" = "School",
  "schulen" = "Schools",
  "schueler" = "Students",
  "allgemeinbildend" = "General education",
  "tageseinrichtung" = "Daycare facility",
  "kinder" = "Children",
  "plaetze" = "Places",
  "einrichtung" = "Facility"
)

# Function to translate a census variable name
translate_census_var <- function(var_name) {
  # Remove census_ prefix
  clean <- str_remove(var_name, "^census_")

  # Split by underscore
  parts <- str_split(clean, "_")[[1]]

  # Translate each part
  translated_parts <- sapply(parts, function(p) {
    p_lower <- str_to_lower(p)
    if (p_lower %in% names(german_english)) {
      return(german_english[[p_lower]])
    }
    # Keep numbers and short codes as-is
    if (str_detect(p, "^[0-9]+$")) {
      return(p)
    }
    # Return original with first letter capitalized if no translation
    return(str_to_title(p))
  })

  # Join with appropriate separators
  result <- paste(translated_parts, collapse = " ")

  # Clean up common patterns
  result <- str_replace_all(result, "\\s+", " ")
  result <- str_trim(result)

  return(result)
}

# ==============================================================================
# STEP 3: Generate translations for all variables
# ==============================================================================
cat("STEP 3: Creating translation table...\n")

df_translations <- tibble(
  variable_name = census_vars
) %>%
  rowwise() %>%
  mutate(
    english_name = translate_census_var(variable_name)
  ) %>%
  ungroup() %>%
  arrange(variable_name)

cat("  Generated translations for ", nrow(df_translations), " variables\n\n", sep = "")

# ==============================================================================
# STEP 4: Save output
# ==============================================================================
cat("STEP 4: Saving translation table...\n")

output_dir <- file.path(data_path, "Tex", "Tables")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

output_file <- file.path(output_dir, "tbl_census_translations.csv")
write_csv(df_translations, output_file)
cat("  Saved: ", output_file, "\n\n", sep = "")

# ==============================================================================
# Summary
# ==============================================================================
cat("==================================================\n")
cat("Census Translations Generated\n")
cat("==================================================\n\n")

cat("Total variables: ", nrow(df_translations), "\n\n", sep = "")

cat("Sample translations:\n")
print(head(df_translations, 20))

cat("\nNote: Review and manually improve translations as needed.\n")
cat("The file can be edited directly: ", output_file, "\n\n")

################################################################################
# END OF SCRIPT
################################################################################
