# ==============================================================================
# Census Data Acquisition using restatis
# ==============================================================================
# Purpose: Download 2017 population data from Destatis GENESIS-Online
# Author: Michael Mühlbauer
# Date: 2025-11-20 (Updated: 2025-12-22 - merged additional tables)
#
# Strategy: Download ~50 targeted tables organized by priority
# Runtime: ~60 minutes
#
# Input: None (downloads from Destatis API)
# Output:
#   - data/raw/census/table_[code]_2017.rds (individual tables)
#   - data/raw/census/census_merged_2017.rds (merged dataset)
#   - data/raw/census/selected_tables.csv (table metadata)
# ==============================================================================

# Setup ------------------------------------------------------------------------
rm(list = ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(restatis, tidyverse, usethis)
# Source census data processing functions from auxiliaries
source("./R/auxiliaries/05_census_data_functions.r")

data_path <- "./data"
census_dir <- file.path(data_path, "raw/census")
dir.create(census_dir, recursive = TRUE, showWarnings = FALSE)

# Authentication ---------------------------------------------------------------
#restatis::gen_auth_save("genesis")
#restatis::gen_auth_save(database = "regio")
#save API keys in .Renviron
#usethis::edit_r_environ()
# # Keys stored in .Renviron: GENESIS_KEY, REGIO_KEY

gen_logincheck(database = "genesis")
gen_logincheck(database = "regio")

# Find all Kreise tables
message("Searching for available Kreise tables...")
census_available_tables <- restatis::gen_find(
  term = "Kreise",
  database = c("genesis", "regio"),
  detailed = TRUE,
  category = "tables",
  pagelength = 2500
)

#save census_available_tables in data/output/variables/:

saveRDS(census_available_tables,
   file.path(data_path, "/output/variables/census_available_tables.rds"))

# Extract available table codes from each database
genesis_codes <- census_available_tables[[1]]$Tables$Code
regio_codes <- census_available_tables[[2]]$Tables$Code

message("Found ", length(genesis_codes), " tables in GENESIS")
message("Found ", length(regio_codes), " tables in REGIO")

# print(census_available_tables[[1]]$Tables, n = 10000)
# print(census_available_tables[[2]]$Tables, n = 10000)

# ==============================================================================
# STEP 1: DEFINE TABLE SELECTION (Organized by Priority)
# ==============================================================================

# Priority 1: CALIBRATION VARIABLES (REQUIRED for bootstrap) ------------------
# These are needed for census population totals by BLAND, age, sex, HH size
calibration_tables <- tribble(
  ~table_code,      ~description,                                                   ~use,                      ~database,
  # Row 16 (REGIO): Population by sex
  "12411-01-01-4",  "Bevölkerung: Deutschland, Stichtag, Geschlecht, Kreise",       "Sex (2 categories)",      "regio",
  # Row 26 (REGIO): Population by age groups
  "12411-09-01-4",  "Bevölkerung: Deutschland, Stichtag, Geschlecht, Altersgruppen","Age (5 categories)",      "regio",
  # Row 13 (REGIO): Households - might be available as "Haushalte nach Haushaltsgröße"
  # Note: This exact code not confirmed in truncated results; validation will check
  "12211-Z-10",     "Haushalte nach Haushaltsgröße",                                "HH size (4 categories)",  "regio"
)

# Priority 2: INCOME & ECONOMIC ACTIVITY (HIGHEST VALUE - new!) ---------------
# Income is strongest predictor of car ownership, currently missing from Priority 2
income_tables <- tribble(
  ~table_code,      ~description,                                                                                   ~use,                      ~database,
  # INCOME (strongest predictor of car ownership)
  "73111-01-01-4",  "Lohn- und Einkommensteuerpflichtige, Gesamtbetrag der Einkünfte, Lohn- und Einkommensteuer", "Taxable income",          "regio",
  # DISPOSABLE INCOME (alternative measure)
  "82000-07-01-4",  "Verfügbares Einkommen der privaten Haushalte inkl. privater Organisationen ohne Erwerbszweck", "Disposable income",   "regio",
  # FISCAL CAPACITY (indirect income proxy)
  "71231-01-03-4",  "Realsteuervergleich: Hebesätze der Realsteuern",                                              "Municipal tax rates",     "regio"
)

# Priority 3: MOBILITY & TRANSPORT (HIGH VALUE for mobility analysis) ---------
# Car ownership + commuting + migration = direct mobility indicators
mobility_transport_tables <- tribble(
  ~table_code,      ~description,                                                   ~use,                           ~database,
  # Row 25 (GENESIS): CAR OWNERSHIP - CRITICAL for mobility analysis!
  "46251-0020",     "Fahrzeugbestand: Kreise, Quartale, Fahrzeugklassen",          "Car ownership by type",        "genesis",
  # Note: 46251-0021 is too large for simple download - removed
  # Rows 69-70 (REGIO): Employment at work vs residence = commuting proxy
  "13111-01-03-4",  "Erwerbstätige (Arbeitsort): Geschlecht, Nationalität, Kreise","Employment at workplace",      "regio",
  "13111-02-02-4",  "Erwerbstätige (Wohnort): Geschlecht, Nationalität, Kreise",   "Employment at residence",      "regio",
  # COMMUTING FLOWS (detailed employment data for commuting analysis)
  "13111-07-05-4",  "Sozialversicherungspflichtig Beschäftigte am Arbeitsort nach Geschlecht, Nationalität, WZ",  "Employment at workplace by sector", "regio",
  "13111-11-04-4",  "Sozialversicherungspflichtig Beschäftigte am Arbeitsort nach Geschlecht, Nationalität, Ausbildung", "Employment at workplace by education", "regio",
  # EMPLOYMENT DETAIL (workplace + residence by FT/PT, age, education)
  "13111-04-02-4",  "Sozialversicherungspflichtig Beschäftigte am Wohnort nach Geschlecht, Nationalität, Beschäftigungsumfang", "Employment at residence by FT/PT", "regio",
  "13111-06-02-4",  "Sozialversicherungspflichtig Beschäftigte am Wohnort nach Geschlecht, Nationalität, Altersgruppen", "Employment at residence by age", "regio",
  "13111-12-03-4",  "Sozialversicherungspflichtig Beschäftigte am Wohnort nach Geschlecht, Nationalität, Ausbildung", "Employment at residence by education", "regio",
  # Note: 13111-09-01-4 (Inter-district commuting) not found in database - removed
  # Row 66 (REGIO): Migration across district borders - mobility proxy
  "12711-04-02-4",  "Zu- und Fortzüge über Kreisgrenzen",                          "Inter-district migration",     "regio"
)

# Priority 4: TOURISM & REGIONAL ATTRACTIVENESS (HIGH VALUE - new!) -----------
# Proxy for transport infrastructure quality and regional connectivity
tourism_tables <- tribble(
  ~table_code,      ~description,                                                                                   ~use,                           ~database,
  # TOURISM (proxy for infrastructure quality)
  "45412-03-01-4",  "Gästeübernachtungen, Gästeankünfte nach Herkunft - Jahressumme (bis 2017)",                  "Guest overnight stays 2017",   "regio",
  # ADDITIONAL TOURISM (post-2017 data - comprehensive)
  "45412-01-03-4",  "Beherbergungsbetriebe, Schlafgelegenheiten, Gästeankünfte, Gästeübernachtungen (ab 2018)",   "Tourism establishments (from 2018)", "regio",
  "45412-02-02-4",  "Beherbergungsbetriebe, Schlafgelegenheiten, Gästeankünfte, Gästeübernachtungen nach Betriebsarten (ab 2018)", "Tourism by establishment type (from 2018)", "regio",
  "45412-03-02-4",  "Gästeankünfte und Gästeübernachtungen nach Herkunftsgebieten (ab 2018)",                      "Tourism by origin (from 2018)", "regio"
)

# Priority 5: DEMOGRAPHICS (HIGH VALUE for age/structure predictors) ----------
demographic_tables <- tribble(
  ~table_code,      ~description,                                                   ~use,                           ~database,
  # Rows 19, 21, 22 (REGIO): Age structure metrics
  "12411-07-01-4",  "Durchschnittsalter der Bevölkerung: Kreise, Stichtag",        "Mean age",                     "regio",
  "12411-08-01-4",  "Jugend- und Altenquotient: Kreise, Stichtag",                 "Dependency ratios",            "regio",
  "12411-10-01-4",  "Medianalter der Bevölkerung nach Geschlecht: Kreise",         "Median age",                   "regio",
  # Row 27 (REGIO): Nationality - migration background indicator
  "12411-11-01-4",  "Bevölkerung: Nationalität (Deutsche/Ausländer), Geschlecht",  "Migration background",         "regio",
  # Row 64 (REGIO): Migration within municipality borders (less relevant but available)
  "12711-01-03-4",  "Zu- und Fortzüge über Gemeindegrenzen",                       "Local mobility",               "regio",
  # Rows 58-59 (REGIO): Births and deaths
  "12612-01-01-4",  "Lebendgeborene nach Geschlecht: Kreise",                      "Birth rate",                   "regio",
  "12613-01-01-4",  "Gestorbene nach Geschlecht: Kreise",                          "Death rate",                   "regio"
)

# Priority 6: EMPLOYMENT & ECONOMY (HIGH VALUE for socioeconomic status) ------
employment_tables <- tribble(
  ~table_code,      ~description,                                                   ~use,                           ~database,
  # Row 72 (REGIO): Employment by type (full-time vs part-time)
  "13111-03-02-4",  "Erwerbstätige: Geschlecht, Stellung im Beruf, Kreise",        "Employment type",              "regio",
  # Row 73 (REGIO): Employment by age groups
  "13111-05-03-4",  "Erwerbstätige: Geschlecht, Altersgruppen, Kreise",            "Employment by age",            "regio",
  # Row 76 (REGIO): Unemployment rate
  "13211-02-05-4",  "Arbeitslose und Arbeitslosenquote: Kreise",                   "Unemployment rate",            "regio",
  # Row 77 (REGIO): Employment by economic sector
  "13312-01-05-4",  "Erwerbstätige: Wirtschaftsbereiche, Kreise",                  "Economic structure",           "regio"
)

# Priority 7: INFRASTRUCTURE & BUILT ENVIRONMENT (MEDIUM VALUE) ---------------
infrastructure_tables <- tribble(
  ~table_code,      ~description,                                                   ~use,                           ~database,
  # Row 7 (REGIO): Area in km²
  "11111-01-01-4",  "Fläche insgesamt: Kreise, Stichtag",                          "Area (for density calc)",      "regio",
  # Rows 128-129 (REGIO): Building activity
  "31111-01-02-4",  "Baugenehmigungen: Wohngebäude, Kreise",                       "Building permits",             "regio",
  "31121-01-02-4",  "Baufertigstellungen: Wohngebäude, Kreise",                    "Building completions",         "regio",
  # Row 131 (REGIO): Housing stock
  "31231-02-01-4",  "Wohngebäude- und Wohnungsbestand: Kreise",                    "Housing stock",                "regio",
  # Rows 141-142 (REGIO): Land use
  "33111-01-02-4",  "Bodenfläche nach Art der tatsächlichen Nutzung: Kreise",      "Land use",                     "regio",
  "33111-02-01-4",  "Siedlungs- und Verkehrsfläche nach Nutzungsarten: Kreise",    "Settlement area",              "regio"
)

# Priority 8: EDUCATION & SOCIAL SERVICES (LOWER VALUE) -----------------------
education_social_tables <- tribble(
  ~table_code,      ~description,                                                   ~use,                           ~database,
  # Row 90 (REGIO): Schools
  "21111-01-03-4",  "Allgemeinbildende Schulen: Schulen, Schüler/-innen, Kreise",  "School infrastructure",        "regio",
  # Rows 98-99 (REGIO): Childcare
  "22541-01-04-4",  "Tageseinrichtungen für Kinder: Einrichtungen, Plätze, Kreise","Childcare facilities",         "regio"
  #"22543-01-03-4",  "Kinder in Tagesbetreuung: Alter, Betreuungsart, Kreise",      "Childcare usage",             "regio" #too many NA
  # Note: 23111-01-05-4 (Hospital infrastructure) is too large for simple download - removed
)

# Combine all tables -----------------------------------------------------------
all_selected_tables <- bind_rows(
  calibration_tables %>% mutate(priority = 1, priority_label = "Calibration"),
  income_tables %>% mutate(priority = 2, priority_label = "Income & Economic"),
  mobility_transport_tables %>% mutate(priority = 3, priority_label = "Mobility & Transport"),
  tourism_tables %>% mutate(priority = 4, priority_label = "Tourism"),
  demographic_tables %>% mutate(priority = 5, priority_label = "Demographics"),
  employment_tables %>% mutate(priority = 6, priority_label = "Employment"),
  infrastructure_tables %>% mutate(priority = 7, priority_label = "Infrastructure"),
  education_social_tables %>% mutate(priority = 8, priority_label = "Education & Social")
)

message("Total selected tables: ", nrow(all_selected_tables))
message("By priority:")
all_selected_tables %>%
  count(priority, priority_label) %>%
  arrange(priority) %>%
  {
    for (i in 1:nrow(.)) {
      message("  Priority ", .$priority[i], " (", .$priority_label[i], "): ", .$n[i], " tables")
    }
  }

# Validate table codes against available databases
message("\n=== Validating table codes ===")
all_selected_tables <- all_selected_tables %>%
  mutate(
    in_genesis = table_code %in% genesis_codes,
    in_regio = table_code %in% regio_codes,
    database_validated = case_when(
      in_regio ~ "regio",
      in_genesis ~ "genesis",
      TRUE ~ "NOT_FOUND"
    )
  )

# Report validation results
not_found <- all_selected_tables %>% filter(database_validated == "NOT_FOUND")
if (nrow(not_found) > 0) {
  warning("Tables not found in any database: ", paste(not_found$table_code, collapse = ", "))
}

in_genesis <- all_selected_tables %>% filter(database_validated == "genesis")
if (nrow(in_genesis) > 0) {
  message("Tables found in GENESIS: ", nrow(in_genesis))
}

in_regio <- all_selected_tables %>% filter(database_validated == "regio")
if (nrow(in_regio) > 0) {
  message("Tables found in REGIO: ", nrow(in_regio))
}

# Update database column with validated values
all_selected_tables <- all_selected_tables %>%
  mutate(database = database_validated) %>%
  select(-in_genesis, -in_regio, -database_validated) %>%
  filter(database != "NOT_FOUND")

# Save selection metadata
write.csv(all_selected_tables,
          file.path(census_dir, "selected_tables.csv"),
          row.names = FALSE)

message("\nSaved table selection to: ", file.path(census_dir, "selected_tables.csv"))

# ==============================================================================
# STEP 2: DOWNLOAD TABLES
# ==============================================================================

download_census_table_safe <- function(table_code, year = 2017, database = "regio") {

  tryCatch({

    # First attempt: try with year parameters
    data <- tryCatch({
      restatis::gen_table(
        name = table_code,
        database = database,
        startyear = year,
        endyear = year
      )
    }, error = function(e) {
      # If year parameters fail, try without them
      message("  → Year filter not supported, trying without year parameters...")
      restatis::gen_table(
        name = table_code,
        database = database
      )
    })

    # Add metadata
    data <- data %>%
      mutate(
        table_code = table_code,
        year_download = year,
        download_date = Sys.Date()
      )

    # Save individual table
    saveRDS(data,
            file.path(census_dir, paste0("table_", table_code, "_", year, ".rds")))

    message("✓ Downloaded: ", table_code)
    return(data)

  }, error = function(e) {
    warning("✗ Failed: ", table_code, " - ", e$message)
    return(NULL)
  })
}

# Download all tables by priority ----------------------------------------------
message("\n=== Starting downloads ===")
downloaded_tables <- list()

for (i in 1:nrow(all_selected_tables)) {
  table_code <- all_selected_tables$table_code[i]
  db <- all_selected_tables$database[i]
  priority <- all_selected_tables$priority[i]

  message("\n[", i, "/", nrow(all_selected_tables), "] Priority ", priority, ": ", table_code)

  downloaded_tables[[table_code]] <- download_census_table_safe(
    table_code = table_code,
    year = 2017,
    database = db
  )

  # Be nice to API
  Sys.sleep(0.5)
}

# Remove NULL entries (failed downloads)
downloaded_tables <- downloaded_tables[!sapply(downloaded_tables, is.null)]

message("\n=== Download Summary ===")
message("Successfully downloaded: ", length(downloaded_tables), " / ", nrow(all_selected_tables), " tables")

if (length(downloaded_tables) < nrow(all_selected_tables)) {
  failed_codes <- setdiff(all_selected_tables$table_code, names(downloaded_tables))
  warning("Failed downloads: ", paste(failed_codes, collapse = ", "))
}

# Special case: Table 12211-Z-10 (HH size) requires manual download due to API issues

`12211-Z-10` <- read.csv(file.path(census_dir, "12211-Z-10.csv"), sep = ";") %>%
  tibble() %>%
  # Rename X1_variable_... to 1_variable_... for consistency with other tables
  rename_with(
    ~ gsub("^X([0-9])", "\\1", .x),
    matches("^X[0-9]_variable")
  )

downloaded_tables[["12211-Z-10"]] <- `12211-Z-10`

# ==============================================================================
# STEP 3: Merge all downloaded tables
# ==============================================================================

message("\n=== Merging tables ===")
#undebug(merge_all_census_tables)
census_merged <- merge_all_census_tables(downloaded_tables)

message("Merged ", length(downloaded_tables), " tables")
message("  Districts (ags5): ", length(unique(census_merged$ags5)))
message("  Variables: ", ncol(census_merged))

# Add census_ prefix to all variables (except ags5)
message("\n=== Adding census_ prefix to variable names ===")
census_var_cols <- setdiff(names(census_merged), "ags5")
census_merged <- census_merged %>%
  rename_with(~paste0("census_", .), all_of(census_var_cols))
message("Added census_ prefix to ", length(census_var_cols), " variables")

# ==============================================================================
# STEP 4: SAVE OUTPUTS
# ==============================================================================

# Save merged census data
saveRDS(census_merged, file.path(census_dir, "census_merged_2017.rds"))
write.csv(census_merged, file.path(census_dir, "census_merged_2017.csv"),
          row.names = FALSE)

message("\nSaved: census_merged_2017.rds (",
        round(file.size(file.path(census_dir, "census_merged_2017.rds")) / 1024, 1),
        " KB)")

message("\n=== DONE ===")
message("Next steps:")
message("1. Check: data/raw/census/census_merged_2017.rds")
message("2. Use in variable_selection_perskm.r")
message("3. Re-fit FH model and check diagnostics")

