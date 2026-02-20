################################################################################
# Generate INKAR Appendix Table
#
# Purpose: Create a LaTeX longtable listing all INKAR variables with:
#          - Original German name (KurznamePlus)
#          - English translation (from lookup table)
#          - Thematic category
#
# Input:  data/temp/inkar_variable_metadata.rds
#         data/temp/inkar_variable_quality.rds
# Output: data/Tex/Tables/tbl_inkar_variables.tex
#
# Created: 2026-01-16
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
cat("Generate INKAR Appendix Table\n")
cat("==================================================\n\n")

# STEP 1: Load metadata -------------------------------------------------------
cat("STEP 1: Loading INKAR metadata...\n")

df_meta <- readRDS(file.path(data_path, "temp", "inkar_variable_metadata.rds"))
df_quality <- readRDS(file.path(data_path, "temp", "inkar_variable_quality.rds"))
df_wide <- readRDS(file.path(data_path, "temp", "inkar_2017_districts.rds"))

# Get actual variable names from final dataset (excluding ags5)
final_vars <- setdiff(names(df_wide), "ags5")

cat("  Metadata:", nrow(df_meta), "variables\n")
cat("  Quality file:", nrow(df_quality), "variables\n")
cat("  Final dataset:", length(final_vars), "variables\n\n")

# STEP 2: Create cleaned variable names matching the final dataset -----------
cat("STEP 2: Matching variable names...\n")

df_meta <- df_meta %>%
  mutate(
    # Replicate the cleaning logic from inkar_data_acquisition.r
    var_clean = str_to_lower(KurznamePlus),
    var_clean = str_replace_all(var_clean, "ä", "ae"),
    var_clean = str_replace_all(var_clean, "ö", "oe"),
    var_clean = str_replace_all(var_clean, "ü", "ue"),
    var_clean = str_replace_all(var_clean, "ß", "ss"),
    var_clean = str_replace_all(var_clean, "[^a-z0-9_]", "_"),
    var_clean = str_replace_all(var_clean, "_+", "_"),
    var_clean = str_remove(var_clean, "^_|_$"),
    var_clean = paste0("inkar_", var_clean)
  ) %>%
  # Deduplicate metadata by var_clean (keep first occurrence)
  group_by(var_clean) %>%
  slice(1) %>%
  ungroup()

# Filter to only variables in the final dataset
df_matched <- df_quality %>%
  filter(variable %in% final_vars) %>%
  select(variable, year_used, pct_missing) %>%
  left_join(
    df_meta %>% select(var_clean, KurznamePlus, theme_name, theme_sub, Gruppe),
    by = c("variable" = "var_clean")
  )

cat("  Final dataset variables:", length(final_vars), "\n")
cat("  Matched with metadata:", sum(!is.na(df_matched$KurznamePlus)), "of", nrow(df_matched), "\n")

# Check for unmatched
unmatched <- df_matched %>% filter(is.na(KurznamePlus))
if (nrow(unmatched) > 0) {
  cat("  Unmatched variables (first 10):\n")
  print(head(unmatched$variable, 10))
}
cat("\n")

# STEP 3: English translations lookup table -----------------------------------
cat("STEP 3: Loading English translations lookup table...\n")

# Complete lookup table with English translations for all 540 INKAR variables
translations_en <- tribble(
  ~german_name, ~english_name,
  # Absolutzahlen -- Absolutzahlen (9 variables)
  "Arbeitslose", "Unemployed persons",
  "Bevölkerung (mit BBSR-Zensuskorrekturen)", "Population (with BBSR census corrections)",
  "Bevölkerung gesamt", "Total population",
  "Bevölkerung männlich", "Male population",
  "Bevölkerung weiblich", "Female population",
  "Bodenfläche gesamt qkm", "Total land area (sq km)",
  "Bruttoinlandsprodukt in 1000 Euro", "Gross domestic product (EUR 1,000)",
  "Erwerbsfähige Bevölkerung (15 bis unter 65 Jahre)", "Working-age population (15 to under 65 years)",
  "Erwerbstätige", "Employed persons",

  # Arbeitslosigkeit -- Allgemein (5 variables)
  "Arbeitslose Frauen", "Unemployed women",
  "Arbeitslose Männer", "Unemployed men",
  "Arbeitslosenquote", "Unemployment rate",
  "Arbeitslosenquote Frauen", "Female unemployment rate",
  "Arbeitslosenquote Männer", "Male unemployment rate",

  # Arbeitslosigkeit -- Altersgruppen (14 variables)
  "Anteil jüngere Arbeitslose", "Share of younger unemployed",
  "Anteil männliche jüngere Arbeitslose", "Share of younger unemployed men",
  "Anteil männliche ältere Arbeitslose", "Share of older unemployed men",
  "Anteil weibliche jüngere Arbeitslose", "Share of younger unemployed women",
  "Anteil weibliche ältere Arbeitslose", "Share of older unemployed women",
  "Anteil ältere Arbeitslose", "Share of older unemployed",
  "Arbeitslosenquote Jüngere", "Youth unemployment rate",
  "Arbeitslosenquote Ältere", "Older workers unemployment rate",
  "Jüngere Arbeitslose", "Younger unemployed",
  "Männliche jüngere Arbeitslose", "Younger unemployed men",
  "Männliche ältere Arbeitslose", "Older unemployed men",
  "Weibliche jüngere Arbeitslose", "Younger unemployed women",
  "Weibliche ältere Arbeitslose", "Older unemployed women",
  "Ältere Arbeitslose", "Older unemployed",

  # Arbeitslosigkeit -- Struktur (15 variables)
  "Arbeitslose mit Anforderungsniveau Experte", "Unemployed with expert skill level",
  "Arbeitslose mit Anforderungsniveau Fachkraft", "Unemployed with skilled worker level",
  "Arbeitslose mit Anforderungsniveau Helfer", "Unemployed with helper skill level",
  "Arbeitslose mit Anforderungsniveau Spezialist", "Unemployed with specialist skill level",
  "Arbeitslose ohne Ausbildung", "Unemployed without vocational training",
  "Ausländische Arbeitslose", "Foreign unemployed",
  "Ausländische männliche Arbeitslose", "Foreign unemployed men",
  "Ausländische weibliche Arbeitslose", "Foreign unemployed women",
  "Langzeitarbeitslose", "Long-term unemployed",
  "Männliche Langzeitarbeitslose", "Male long-term unemployed",
  "Offene Stellen mit Anforderungsniveau Experte", "Job vacancies requiring expert skill level",
  "Offene Stellen mit Anforderungsniveau Fachkraft", "Job vacancies requiring skilled worker level",
  "Offene Stellen mit Anforderungsniveau Helfer", "Job vacancies requiring helper skill level",
  "Offene Stellen mit Anforderungsniveau Spezialist", "Job vacancies requiring specialist skill level",
  "Weibliche Langzeitarbeitslose", "Female long-term unemployed",

  # Bauen und Wohnen -- Baulandmarkt und Bautätigkeit (14 variables)
  "Angebotsmietpreise", "Asking rental prices",
  "Baugenehmigungen für Wohnungen in Ein- und Zweifamilienhäusern", "Building permits for dwellings in single/two-family houses",
  "Baugenehmigungen für Wohnungen in Mehrfamilienhäusern", "Building permits for dwellings in multi-family houses",
  "Baugenehmigungen für Wohnungenje Einwohner", "Building permits for dwellings per inhabitant",
  "Baulandpreise", "Building land prices",
  "Bauüberhänge", "Construction backlogs",
  "Fertiggestellte Wohngebäude mit erneuerbarer Heizenergie", "Completed residential buildings with renewable heating",
  "Fertiggestellte Wohnungen je Wohnung im Bestand", "Completed dwellings per existing dwelling",
  "Fertiggestellte Wohnungen mit erneuerbarer Heizenergie", "Completed dwellings with renewable heating",
  "Neubauwohnungen in Ein- und Zweifamilienhäusern", "New dwellings in single/two-family houses",
  "Neubauwohnungen in Ein- und Zweifamilienhäusern je Einwohner", "New dwellings in single/two-family houses per inhabitant",
  "Neubauwohnungen in Mehrfamilienhäusern je Einwohner", "New dwellings in multi-family houses per inhabitant",
  "Neubauwohnungen je Einwohner", "New dwellings per inhabitant",
  "Neue Ein- und Zweifamilienhäuser", "New single/two-family houses",

  # Bauen und Wohnen -- Gebäude- und Wohnungsbestand (7 variables)
  "5- und mehr Raum-Wohnungen", "Dwellings with 5+ rooms",
  "Ein- und Zweifamilienhäuser", "Single/two-family houses",
  "Ein- und Zweiraumwohnungen", "One- and two-room dwellings",
  "Mehrfamilienhäuser", "Multi-family houses",
  "Wohnfläche", "Living space",
  "Wohnungen in Ein- und Zweifamilienhäusern", "Dwellings in single/two-family houses",
  "Wohnungen in Mehrfamilienhäusern", "Dwellings in multi-family houses",

  # Beschäftigung und Erwerbstätigkeit -- Atypische Beschäftigung (13 variables)
  "Anteil Minijobs (Frauen)", "Share of mini-jobs (women)",
  "Anteil Minijobs (Männer)", "Share of mini-jobs (men)",
  "Anteil Minijobs (Nebenverdienst)", "Share of mini-jobs (secondary income)",
  "Anteil Minijobs (Nebenverdienst) an den Beschäftigungsverhältnissen", "Share of secondary mini-jobs in employment relationships",
  "Anteil Minijobs (ausschließlich) an den Beschäftigungsverhältnissen", "Share of exclusive mini-jobs in employment relationships",
  "Anteil Minijobs (ausschließlich) an geringfügig Beschäftigten", "Share of exclusive mini-jobs among marginally employed",
  "Anteil Minijobs an den Beschäftigungsverhältnissen", "Share of mini-jobs in employment relationships",
  "Aufstocker", "Income top-up recipients",
  "Geringfügig Beschäftigte 65 Jahre und älter", "Marginally employed aged 65+",
  "Geringfügig Beschäftigte 65 Jahre und älter männlich", "Male marginally employed aged 65+",
  "Geringfügig Beschäftigte 65 Jahre und älter weiblich", "Female marginally employed aged 65+",
  "Kurzarbeiter", "Short-time workers",
  "Unterbeschäftigungsquote", "Underemployment rate",

  # Beschäftigung und Erwerbstätigkeit -- Qualifikation (10 variables)
  "Beschäftigte am AO mit Berufsabschluss", "Employees at workplace with vocational qualification",
  "Beschäftigte am AO mit akademischem Berufsabschluss", "Employees at workplace with academic degree",
  "Beschäftigte am AO ohne Berufsabschluss", "Employees at workplace without vocational qualification",
  "Beschäftigte am WO mit Berufsabschluss", "Employees at residence with vocational qualification",
  "Beschäftigte am WO mit akademischen Abschluss", "Employees at residence with academic degree",
  "Beschäftigte am WO ohne Berufsabschluss", "Employees at residence without vocational qualification",
  "Beschäftigte mit Anforderungsniveau Experte", "Employees with expert skill level",
  "Beschäftigte mit Anforderungsniveau Fachkraft", "Employees with skilled worker level",
  "Beschäftigte mit Anforderungsniveau Helfer", "Employees with helper skill level",
  "Beschäftigte mit Anforderungsniveau Spezialist", "Employees with specialist skill level",

  # Beschäftigung und Erwerbstätigkeit -- Struktur (13 variables)
  "Beschäftigtenquote", "Employment rate",
  "Beschäftigtenquote Ausländer", "Foreign employment rate",
  "Beschäftigtenquote Frauen", "Female employment rate",
  "Beschäftigtenquote Männer", "Male employment rate",
  "Erwerbsquote", "Labor force participation rate",
  "Erwerbsquote Frauen", "Female labor force participation rate",
  "Erwerbsquote Männer", "Male labor force participation rate",
  "Quote jüngere Beschäftigte", "Younger workers employment rate",
  "Quote ältere Beschäftigte", "Older workers employment rate",
  "Selbstständigenquote", "Self-employment rate",
  "Teilzeitbeschäftige Frauen", "Part-time employed women",
  "Teilzeitbeschäftigte", "Part-time employees",
  "Verhältnis junge zu alte Erwerbsfähige", "Ratio of young to old working-age population",

  # Beschäftigung und Erwerbstätigkeit -- Wirtschafts- und Berufszweige (17 variables)
  "Anteil Erwerbstätige Finanz- und Unternehmensdienstleistungen", "Share employed in finance and business services",
  "Anteil Erwerbstätige Verarbeitendes Gewerbe an Industrie", "Share employed in manufacturing vs. industry",
  "Beschäftigte Pimärer Sektor", "Employees in primary sector",
  "Beschäftigte Sekundärer Sektor", "Employees in secondary sector",
  "Beschäftigte Tertiärer Sektor", "Employees in tertiary sector",
  "Beschäftigte im Baugewerbe", "Employees in construction",
  "Beschäftigte im Handwerk", "Employees in skilled trades",
  "Beschäftigte in IT- und naturwissenschaftlichen Dienstleistungsberufen", "Employees in IT and natural science services",
  "Beschäftigte in Kreativbranchen", "Employees in creative industries",
  "Beschäftigte in unternehmensbezogenen Dienstleistungen", "Employees in business-related services",
  "Beschäftigte in wissensintensiven Industrien", "Employees in knowledge-intensive industries",
  "Dienstleistungsquote", "Service sector share",
  "Erwerbstätige Primärer Sektor", "Employed in primary sector",
  "Erwerbstätige Sekundärer Sektor", "Employed in secondary sector",
  "Erwerbstätige Tertiärer Sektor", "Employed in tertiary sector",
  "Industriequote", "Industry share",

  # Bevölkerung -- Altersstruktur (21 variables)
  "Durchschnittsalter der Bevölkerung", "Average age of population",
  "Einwohner 65 Jahre und älter", "Inhabitants aged 65+",
  "Einwohner 65 Jahre und älter, Frauen", "Female inhabitants aged 65+",
  "Einwohner 75 Jahre und älter", "Inhabitants aged 75+",
  "Einwohner 75 Jahre und älter, Frauen", "Female inhabitants aged 75+",
  "Einwohner 85 Jahre und älter", "Inhabitants aged 85+",
  "Einwohner 85 Jahre und älter, Frauen", "Female inhabitants aged 85+",
  "Einwohner unter 3 Jahren", "Inhabitants under 3 years",
  "Einwohner unter 6 Jahre", "Inhabitants under 6 years",
  "Einwohner von 18 bis unter 25 Jahren", "Inhabitants aged 18 to under 25",
  "Einwohner von 18 bis unter 25 Jahren, Frauen", "Female inhabitants aged 18 to under 25",
  "Einwohner von 25 bis unter 30 Jahren", "Inhabitants aged 25 to under 30",
  "Einwohner von 25 bis unter 30 Jahren, Frauen", "Female inhabitants aged 25 to under 30",
  "Einwohner von 3 bis unter 6 Jahren", "Inhabitants aged 3 to under 6",
  "Einwohner von 30 bis unter 50 Jahren", "Inhabitants aged 30 to under 50",
  "Einwohner von 50 bis unter 65 Jahren", "Inhabitants aged 50 to under 65",
  "Einwohner von 6 bis unter 18 Jahren", "Inhabitants aged 6 to under 18",
  "Einwohner von 65 bis unter 75 Jahren", "Inhabitants aged 65 to under 75",
  "Einwohner von 65 bis unter 75 Jahren, Frauen", "Female inhabitants aged 65 to under 75",
  "Einwohner von 75 bis unter 85 Jahren", "Inhabitants aged 75 to under 85",
  "Einwohner von 75 bis unter 85 Jahren, Frauen", "Female inhabitants aged 75 to under 85",

  # Bevölkerung -- Bevölkerungsstruktur (22 variables)
  "Abhängigenquote Alte", "Old-age dependency ratio",
  "Abhängigenquote Junge", "Youth dependency ratio",
  "Ausländeranteil", "Share of foreigners",
  "Ausländerinnen", "Foreign women",
  "Bevölkerungsentwicklung (10 Jahre)", "Population change (10 years)",
  "Bevölkerungsentwicklung (5 Jahre)", "Population change (5 years)",
  "Ehescheidungen", "Divorces",
  "Eheschließungen", "Marriages",
  "Einbürgerungen je Ausländer", "Naturalizations per foreigner",
  "Einbürgerungen je Einwohner", "Naturalizations per inhabitant",
  "Einpersonenhaushalte", "Single-person households",
  "Frauenanteil", "Share of women",
  "Frauenanteil 20 bis unter 40 Jahre", "Share of women aged 20 to under 40",
  "Gender-Index", "Gender index",
  "Haushalte mit Kindern", "Households with children",
  "Haushaltsgröße", "Household size",
  "Prognostizierte Bevölkerungsentwicklung (2022-2030)", "Projected population change (2022-2030)",
  "Prognostizierte Bevölkerungsentwicklung (2022-2035)", "Projected population change (2022-2035)",
  "Prognostizierte Bevölkerungsentwicklung (2022-2040)", "Projected population change (2022-2040)",
  "Prognostizierte Bevölkerungsentwicklung (2022-2045)", "Projected population change (2022-2045)",
  "Schutzsuchende an Bevölkerung", "Refugees as share of population",
  "Schutzsuchende an ausländischer Bevölkerung", "Refugees as share of foreign population",

  # Bevölkerung -- Bundestagswahlen (8 variables)
  "Stimmenanteile AfD", "Vote share AfD",
  "Stimmenanteile CDU/CSU", "Vote share CDU/CSU",
  "Stimmenanteile Die Linke", "Vote share Die Linke",
  "Stimmenanteile FDP", "Vote share FDP",
  "Stimmenanteile Grüne", "Vote share Greens",
  "Stimmenanteile SPD", "Vote share SPD",
  "Stimmenanteile Sonstige Parteien", "Vote share other parties",
  "Wahlbeteiligung", "Voter turnout",

  # Bevölkerung -- Natürliche Bevölkerungsbewegungen (7 variables)
  "Fertilitätsrate", "Fertility rate",
  "Geborene", "Births",
  "Geburten junger Mütter (<20 Jahre)", "Births to young mothers (<20 years)",
  "Geburten älterer Mütter (>=40 Jahre)", "Births to older mothers (>=40 years)",
  "Gestorbene", "Deaths",
  "Natürlicher Saldo", "Natural population balance",
  "Säuglingssterblichkeit (<1 Jahr)", "Infant mortality (<1 year)",

  # Bevölkerung -- Sicherheit (2 variables)
  "Straftaten", "Criminal offenses",
  "Wohnungseinbruchdiebstahl", "Residential burglary",

  # Bevölkerung -- Wanderungen (22 variables)
  "Außenwanderungssaldo", "Net external migration",
  "Außenwanderungssaldo Frauen", "Net external migration (women)",
  "Außenwanderungssaldo Männer", "Net external migration (men)",
  "Berufseinstiegswanderer", "Career entry migrants",
  "Bildungswanderer", "Education migrants",
  "Binnenwanderungssaldo", "Net internal migration",
  "Binnenwanderungssaldo Frauen", "Net internal migration (women)",
  "Binnenwanderungssaldo Männer", "Net internal migration (men)",
  "Binnenwanderungsvolumen", "Internal migration volume",
  "Binnenwanderungsvolumen der Frauen", "Internal migration volume (women)",
  "Binnenwanderungsvolumen der Männer", "Internal migration volume (men)",
  "Erwerbswanderer", "Employment migrants",
  "Familienwanderer", "Family migrants",
  "Fortzugsrate", "Out-migration rate",
  "Fortzugsrate Frauen", "Female out-migration rate",
  "Fortzugsrate Männer", "Male out-migration rate",
  "Gesamtwanderungssaldo", "Total net migration",
  "Männliche Berufseinstiegswanderer", "Male career entry migrants",
  "Männliche Bildungswanderer", "Male education migrants",
  "Ruhestandswanderer", "Retirement migrants",
  "Weibliche Berufseinstiegswanderer", "Female career entry migrants",
  "Weibliche Bildungswanderer", "Female education migrants",
  "Zuzugsrate", "In-migration rate",
  "Zuzugsrate Frauen", "Female in-migration rate",
  "Zuzugsrate Männer", "Male in-migration rate",

  # Bildung -- Ausbildungsangebot (16 variables)
  "Ausbildungsplätze", "Vocational training positions",
  "Ausländische Berufsschüler", "Foreign vocational school students",
  "Ausländische Studierende", "Foreign university students",
  "Auszubildende", "Trainees",
  "Auszubildende je 100 Einwohner 15 bis 25 Jahre", "Trainees per 100 inhabitants aged 15-25",
  "Berufsschüler", "Vocational school students",
  "Berufsschülerinnen", "Female vocational school students",
  "Männliche Auszubildende", "Male trainees",
  "Männliche Studierende", "Male university students",
  "Studierende", "University students",
  "Studierende an FH", "Students at universities of applied sciences",
  "Studierende im 1. Semester", "First-semester students",
  "Studierende je 100 Einwohner 18 bis 25 Jahre", "Students per 100 inhabitants aged 18-25",
  "Weibliche Auszubildende", "Female trainees",
  "Weibliche Studierende", "Female university students",

  # Bildung -- Schulische Bildung (15 variables)
  "Ausländische Schüler", "Foreign school students",
  "Ausländische Schüler je 100 Ausländer 6 bis 18 Jahre", "Foreign students per 100 foreigners aged 6-18",
  "Männliche Schulabgänger mit Hauptschulabschluss", "Male school leavers with lower secondary certificate",
  "Männliche Schulabgänger mit allgemeiner Hochschulreife", "Male school leavers with university entrance qualification",
  "Männliche Schulabgänger mit mittlerem Abschluss", "Male school leavers with intermediate certificate",
  "Männliche Schulabgänger ohne Abschluss", "Male school leavers without qualification",
  "Schulabgänger mit Hauptschulabschluss", "School leavers with lower secondary certificate",
  "Schulabgänger mit allgemeiner Hochschulreife", "School leavers with university entrance qualification",
  "Schulabgänger mit mittlerem Abschluss", "School leavers with intermediate certificate",
  "Schulabgänger ohne Abschluss", "School leavers without qualification",
  "Schüler", "School students",
  "Weibliche Schulabgänger mit Hauptschulabschluss", "Female school leavers with lower secondary certificate",
  "Weibliche Schulabgänger mit allgemeiner Hochschulreife", "Female school leavers with university entrance qualification",
  "Weibliche Schulabgänger mit mittlerem Abschluss", "Female school leavers with intermediate certificate",
  "Weibliche Schulabgänger ohne Abschluss", "Female school leavers without qualification",

  # Flächennutzung und Umwelt -- Flächennutzung (12 variables)
  "Erholungsfläche", "Recreation area",
  "Erholungsfläche je Einwohner", "Recreation area per inhabitant",
  "Flächenneuinanspruchnahme", "New land take",
  "Freifläche", "Open space",
  "Freifläche je Einwohner", "Open space per inhabitant",
  "Landwirtschaftsfläche", "Agricultural area",
  "Naturnähere Fläche", "Near-natural area",
  "Naturnähere Fläche je Einwohner", "Near-natural area per inhabitant",
  "Siedlungs- und Verkehrsfläche", "Settlement and transport area",
  "Siedlungsdichte in km²", "Settlement density per sq km",
  "Waldfläche", "Forest area",
  "Wasserfläche", "Water area",

  # Flächennutzung und Umwelt -- Umwelt (7 variables)
  "Abfallmenge", "Waste volume",
  "Haus- und Sperrmüll", "Household and bulky waste",
  "Haushaltsabfälle", "Household waste",
  "Organischer Abfall", "Organic waste",
  "Stickstoffüberschuss", "Nitrogen surplus",
  "Trinkwasserverbrauch", "Drinking water consumption",
  "Wertstoffe", "Recyclable materials",

  # Medizinische und soziale Versorgung -- Medizinische Versorgung (5 variables)
  "Allgemeinärzte", "General practitioners",
  "Hausärzte", "Family doctors",
  "Internisten", "Internists",
  "Kinderärzte", "Pediatricians",
  "Ärzte", "Physicians",

  # Medizinische und soziale Versorgung -- Soziale Versorgung (14 variables)
  "Ambulante Pflege", "Outpatient care",
  "Betreuungsquote Kleinkinder", "Childcare rate for toddlers",
  "Betreuungsquote Vorschulkinder", "Childcare rate for preschool children",
  "Empfänger von Pflegegeld", "Recipients of care allowance",
  "Ganztags-Betreuungsquote Kleinkinder", "Full-day childcare rate for toddlers",
  "Ganztags-Betreuungsquote Vorschulkinder", "Full-day childcare rate for preschool children",
  "Integrative Kindertageseinrichtungen", "Integrative childcare facilities",
  "Kinder ausländischer Herkunft in Tageseinrichtungen", "Children of foreign origin in daycare",
  "Personal in Pflegediensten", "Staff in care services",
  "Personal in Pflegeheimen", "Staff in nursing homes",
  "Pflegebedürftige", "Care-dependent persons",
  "Pflegeheimplätze", "Nursing home places",
  "Pädagogisches Personal in Tageseinrichtungen", "Educational staff in daycare facilities",
  "Stationäre Pflege", "Inpatient care",

  # Privateinkommen, Private Schulden (21 variables)
  "Anteil Selbständige an Insolvenzen", "Share of self-employed in insolvencies",
  "Arbeitsvolumen", "Volume of work",
  "Bruttoverdienst", "Gross earnings",
  "Bruttoverdienst im Produzierenden Gewerbe", "Gross earnings in manufacturing",
  "Einzelhandelsrelevante Kaufkraft", "Retail-relevant purchasing power",
  "Haushalte mit hohem Einkommen", "High-income households",
  "Haushalte mit mittlerem Einkommen", "Medium-income households",
  "Haushalte mit niedrigem Einkommen", "Low-income households",
  "Kaufkraft", "Purchasing power",
  "Medianeinkommen", "Median income",
  "Medianeinkommen 25 bis unter 55-Jährige", "Median income aged 25 to under 55",
  "Medianeinkommen 55 bis unter 65-Jährige", "Median income aged 55 to under 65",
  "Medianeinkommen Frauen", "Female median income",
  "Medianeinkommen Männer", "Male median income",
  "Medianeinkommen akademischer Berufsabschluss", "Median income with academic degree",
  "Medianeinkommen anerkannter Berufsabschluss", "Median income with recognized vocational qualification",
  "Monatliches Haushaltseinkommen", "Monthly household income",
  "Schuldnerquote", "Debtor rate",
  "Verbraucherinsolvenzverfahren", "Consumer insolvency proceedings",
  "Verbraucherinsolvenzverfahren Gläubigerforderungen", "Consumer insolvency creditor claims",
  "Verdienstabstand zwischen Frauen und Männern", "Gender pay gap",

  # Raumwirksame Mittel (12 variables)
  "Arbeitsmarktpolitische Hilfen der BA für Arbeit (kurzfristig)", "Short-term labor market policy support (Federal Employment Agency)",
  "Arbeitsmarktpolitische Hilfen der BA für Arbeit (langfristig)", "Long-term labor market policy support (Federal Employment Agency)",
  "Direkte Projektförderung (kurfristig)", "Direct project funding (short-term)",
  "Direkte Projektförderung (langfristig)", "Direct project funding (long-term)",
  "GRW, Bereich Förderung der gewerblichen Wirtschaft (kurzfristig)", "GRW commercial economy support (short-term)",
  "GRW, Bereich Förderung der gewerblichen Wirtschaft (langfristig)", "GRW commercial economy support (long-term)",
  "GRW, Bereich Förderung wirtschaftsnaher Infrastruktur (kurzfristig)", "GRW economic infrastructure support (short-term)",
  "GRW, Bereich Förderung wirtschaftsnaher Infrastruktur (langfristig)", "GRW economic infrastructure support (long-term)",
  "Hochschulförderung (kurzfristig)", "University funding (short-term)",
  "Hochschulförderung (langfristig)", "University funding (long-term)",
  "Städtebauförderung (kurzfristig)", "Urban development funding (short-term)",
  "Städtebauförderung (langfristig)", "Urban development funding (long-term)",

  # SDG-Indikatoren für Kommunen (44 variables)
  "(SDG 1) Armut - Altersarmut", "(SDG 1) Poverty -- old-age poverty",
  "(SDG 1) Armut - Kinderarmut", "(SDG 1) Poverty -- child poverty",
  "(SDG 1) SGB II-/SGB XII-Quote", "(SDG 1) Social code II/XII rate",
  "(SDG 10) Beschäftigungsquote - Ausländer", "(SDG 10) Employment rate -- foreigners",
  "(SDG 10) Einbürgerungen", "(SDG 10) Naturalizations",
  "(SDG 11) Angebotsmietpreise", "(SDG 11) Asking rental prices",
  "(SDG 11) Fertiggestellte Wohngebäude mit erneuerbarer Heizenergie", "(SDG 11) Completed residential buildings with renewable heating",
  "(SDG 11) Flächeninanspruchnahme", "(SDG 11) Land take",
  "(SDG 11) Flächenneuinanspruchnahme", "(SDG 11) New land take",
  "(SDG 11) Flächennutzungsintensität", "(SDG 11) Land use intensity",
  "(SDG 11) Naherholungsflächen", "(SDG 11) Local recreation areas",
  "(SDG 11) PKW mit Elektroantrieb", "(SDG 11) Electric cars",
  "(SDG 11) Pkw-Dichte", "(SDG 11) Car density",
  "(SDG 11) Verunglückte im Verkehr", "(SDG 11) Traffic accident casualties",
  "(SDG 11) Wohnfläche", "(SDG 11) Living space",
  "(SDG 11) Wohnungsnahe Grundversorgung - Supermarkt", "(SDG 11) Local basic supply -- supermarket",
  "(SDG 12) Abfallmenge", "(SDG 12) Waste volume",
  "(SDG 12) Trinkwasserverbrauch - Private Haushalte", "(SDG 12) Drinking water consumption -- private households",
  "(SDG 16) Liquiditätskredite", "(SDG 16) Liquidity loans",
  "(SDG 16) Steuereinnahmen", "(SDG 16) Tax revenue",
  "(SDG 16) Straftaten", "(SDG 16) Criminal offenses",
  "(SDG 2) Stickstoffüberschuss der Landwirtschaft", "(SDG 2) Agricultural nitrogen surplus",
  "(SDG 3) Personal in Pflegediensten", "(SDG 3) Staff in care services",
  "(SDG 3) Pflegeheimplätze", "(SDG 3) Nursing home places",
  "(SDG 3) Vorzeitige Sterblichkeit", "(SDG 3) Premature mortality",
  "(SDG 3) Wohnungsnahe Grundversorgung - Apotheke", "(SDG 3) Local basic supply -- pharmacy",
  "(SDG 3) Wohnungsnahe Grundversorgung - Hausarzt", "(SDG 3) Local basic supply -- family doctor",
  "(SDG 4) Betreuung von Kindern (unter 3-Jährige)", "(SDG 4) Childcare (under 3 years)",
  "(SDG 4) Integrative Kindertageseinrichtungen", "(SDG 4) Integrative childcare facilities",
  "(SDG 4) Schulabbrecherquote", "(SDG 4) School dropout rate",
  "(SDG 4) Wohnungsnahe Grundversorgung - Grundschule", "(SDG 4) Local basic supply -- primary school",
  "(SDG 5) Frauenanteil im Stadtrat bzw. Kreistag", "(SDG 5) Share of women in council",
  "(SDG 5) Verhältnis der Beschäftigungsquote von Frauen zu Männern", "(SDG 5) Female to male employment rate ratio",
  "(SDG 6) Abwasserbehandlung", "(SDG 6) Wastewater treatment",
  "(SDG 7) Ladesäuleninfrastruktur", "(SDG 7) Charging station infrastructure",
  "(SDG 8) Beschäftigungsquote - 15- bis 64-Jährige", "(SDG 8) Employment rate -- aged 15-64",
  "(SDG 8) Beschäftigungsquote - 55 Jahre und älter", "(SDG 8) Employment rate -- aged 55+",
  "(SDG 8) Bruttoinlandsprodukt", "(SDG 8) Gross domestic product",
  "(SDG 8) Erwerbstätige Aufstocker", "(SDG 8) Employed income top-up recipients",
  "(SDG 8) Langzeitarbeitslosenquote", "(SDG 8) Long-term unemployment rate",
  "(SDG 9) Breitbandversorgung privater Haushalte", "(SDG 9) Broadband coverage for private households",
  "(SDG 9) Existenzgründungen", "(SDG 9) Business start-ups",
  "(SDG 9) Hochqualifizierte", "(SDG 9) Highly qualified",

  # Siedlungsstruktur (5 variables)
  "Bevölkerung in Mittelzentren", "Population in medium-sized centers",
  "Bevölkerung in Oberzentren", "Population in major centers",
  "Einwohner-Arbeitsplatz-Dichte", "Inhabitant-workplace density",
  "Einwohnerdichte", "Population density",
  "Ländlichkeit", "Rurality",

  # Sozialleistungen -- Bedarfsgemeinschaften (10 variables)
  "Alleinerziehende erwerbsfähige Leistungsberechtigte", "Single-parent employable benefit recipients",
  "Bedarfsgemeinschaften mit Kindern", "Benefit households with children",
  "Einpersonen-Bedarfsgemeinschaften", "Single-person benefit households",
  "Erwerbsfähige Leistungsberechtigte", "Employable benefit recipients",
  "Erwerbsfähige Leistungsberechtigte (Frauen)", "Employable benefit recipients (women)",
  "Große Bedarfsgemeinschaften", "Large benefit households",
  "Junge erwerbsfähige Leistungsberechtigte", "Young employable benefit recipients",
  "Kinderarmut", "Child poverty",
  "Personen in Bedarfsgemeinschaften", "Persons in benefit households",
  "Ältere erwerbsfähige Leistungsberechtigte", "Older employable benefit recipients",

  # Sozialleistungen -- Leistungsempfänger (13 variables)
  "Asylbewerber", "Asylum seekers",
  "Elterngeldbezieher (Mütter)", "Parental allowance recipients (mothers)",
  "Elterngeldbezieher (Väter)", "Parental allowance recipients (fathers)",
  "Elterngeldbezieher (Väter) im ersten Lebensjahr", "Parental allowance recipients (fathers) in first year",
  "Empfänger von Grundsicherung im Alter (Altersarmut)", "Basic income support recipients in old age",
  "Empfänger von Mindestsicherungen", "Minimum income support recipients",
  "Männliche Empfänger von Grundsicherung im Alter (Altersarmut)", "Male basic income support recipients in old age",
  "SGB II - Quote", "Social code II rate",
  "Weibliche Empfänger von Grundsicherung im Alter (Altersarmut)", "Female basic income support recipients in old age",
  "Weibliche SGB II-Empfänger", "Female social code II recipients",
  "Wohngeldhaushalte", "Housing benefit households",
  "Wohngeldhaushalte (Lastenzuschuss)", "Housing benefit households (owner subsidy)",
  "Wohngeldhaushalte (Mietzuschuss)", "Housing benefit households (rent subsidy)",

  # Sozialleistungen -- Transferleistungen (9 variables)
  "ALG I-Leistungen", "Unemployment benefit I payments",
  "ALG I-Leistungen Frauen", "Unemployment benefit I payments (women)",
  "ALG I-Leistungen Männer", "Unemployment benefit I payments (men)",
  "ALG II-Leistungen (Höhe)", "Unemployment benefit II amount",
  "ALG II-Leistungen an SGBII", "Unemployment benefit II payments under SGB II",
  "Leistungen für Unterkunft (Höhe)", "Housing cost payments (amount)",
  "Leistungen für Unterkunft an SGBII", "Housing cost payments under SGB II",
  "Leistungen für Wohngeld", "Housing benefit payments",
  "SGBII-Leistungen", "Social code II payments",

  # Verkehr und Erreichbarkeit -- Erreichbarkeit (21 variables)
  "Bandbreitenverfügbarkeit mindestens 1.000 Mbit/s", "Broadband availability >= 1,000 Mbit/s",
  "Bandbreitenverfügbarkeit mindestens 100 Mbit/s", "Broadband availability >= 100 Mbit/s",
  "Bandbreitenverfügbarkeit mindestens 50 Mbit/s", "Broadband availability >= 50 Mbit/s",
  "Entfernung zum Hausarzt", "Distance to family doctor",
  "Entfernung zum Supermarkt/Discounter", "Distance to supermarket/discounter",
  "Entfernung zur Apotheke", "Distance to pharmacy",
  "Entfernung zur Grundschule", "Distance to primary school",
  "Entfernung zur ÖV Haltestelle", "Distance to public transport stop",
  "Erreichbarkeit von Autobahnen", "Highway accessibility",
  "Erreichbarkeit von Flughäfen", "Airport accessibility",
  "Erreichbarkeit von IC/EC/ICE-Bahnhöfen", "IC/EC/ICE train station accessibility",
  "Erreichbarkeit von Mittelzentren", "Accessibility of medium-sized centers",
  "Erreichbarkeit von Oberzentren", "Accessibility of major centers",
  "Nahversorgung Apotheke", "Local supply -- pharmacy",
  "Nahversorgung Grundschule", "Local supply -- primary school",
  "Nahversorgung Hausarzt", "Local supply -- family doctor",
  "Nahversorgung Supermarkt/Discounter", "Local supply -- supermarket/discounter",
  "Nahversorgung ÖV Haltestelle", "Local supply -- public transport stop",

  # Verkehr und Erreichbarkeit -- Pendler (3 variables)
  "Auspendler", "Out-commuters",
  "Einpendler", "In-commuters",
  "Pendlersaldo", "Commuter balance",

  # Verkehr und Erreichbarkeit -- Straßenverkehr (12 variables)
  "Getötete im Straßenverkehr", "Road traffic fatalities",
  "Ladepunkte je 100 Elektrofahrzeuge (BEV)", "Charging points per 100 electric vehicles (BEV)",
  "Ladepunkte je 100.000 Einwohner", "Charging points per 100,000 inhabitants",
  "Pkw Benzin", "Petrol cars",
  "Pkw Diesel", "Diesel cars",
  "Pkw Elektro (BEV)", "Electric cars (BEV)",
  "Pkw Gas", "Gas-powered cars",
  "Pkw Hybrid insgesamt", "Hybrid cars (total)",
  "Pkw Plug-In-Hybrid (PHEV)", "Plug-in hybrid cars (PHEV)",
  "Pkw-Dichte", "Car density",
  "Straßenverkehrsunfälle", "Road traffic accidents",
  "Verunglückte im Straßenverkehr", "Road traffic casualties",

  # Wirtschaft -- Fremdenverkehr (4 variables)
  "Ausländische Gäste in Beherbergungsbetrieben", "Foreign guests in accommodation establishments",
  "Gästeübernachtungen in Beherbergungsbetrieben", "Guest overnight stays in accommodation establishments",
  "Schlafgelegenheiten in Beherbergungsbetrieben", "Beds in accommodation establishments",
  "Verweildauer in Beherbergungsbetrieben", "Length of stay in accommodation establishments",

  # Wirtschaft -- Wirtschaftliche Leistung (21 variables)
  "Anteil Bruttowertschöpfung Primärer Sektor", "Gross value added share -- primary sector",
  "Anteil Bruttowertschöpfung Sekundärer Sektor", "Gross value added share -- secondary sector",
  "Anteil Bruttowertschöpfung Tertiärer Sektor", "Gross value added share -- tertiary sector",
  "Auslandsumsatz im Bergbau u. Verarb. Gewerbe", "Foreign revenue in mining and manufacturing",
  "Bruttoinlandsprodukt je Einwohner", "GDP per inhabitant",
  "Bruttoinlandsprodukt je Erwerbstätigen", "GDP per employed person",
  "Bruttowertschöpfung je Erwerbstätigen", "Gross value added per employed person",
  "Bruttowertschöpfung je Erwerbstätigen Primärer Sektor", "Gross value added per employed -- primary sector",
  "Bruttowertschöpfung je Erwerbstätigen Sekundärer Sektor", "Gross value added per employed -- secondary sector",
  "Bruttowertschöpfung je Erwerbstätigen Tertiärer Sektor", "Gross value added per employed -- tertiary sector",
  "Großunternehmen", "Large enterprises",
  "Investitionen im Bergbau und Verarb. Gewerbe", "Investment in mining and manufacturing",
  "Kleinbetriebe", "Small establishments",
  "Kleinstbetriebe", "Micro establishments",
  "Mittlere Unternehmen", "Medium-sized enterprises",
  "Umsatz Bauhauptgewerbe", "Revenue in main construction industry",
  "Umsatz im Bergbau u. Verarb. Gewerbe", "Revenue in mining and manufacturing",
  "Umsatz im Handwerk", "Revenue in skilled trades",
  "Unternehmensinsolvenzen", "Business insolvencies",
  "Unternehmensinsolvenzen Gläubigerforderungen", "Business insolvency creditor claims",

  # Zentrale Orte Monitoring -- Bildungsversorgungs- und Kulturfunktion (30 variables)
  "Allgemeinbildende Schulen", "General education schools",
  "Allgemeinbildende Schulen mit Förderschwerpunkt", "General education schools with special needs focus",
  "Berufsbildende Schulen", "Vocational schools",
  "Bibliotheken", "Libraries",
  "Bibliotheksentleihungen", "Library loans",
  "Bäder", "Swimming pools",
  "Grundschulen", "Primary schools",
  "Hochschulen gesamt", "Universities (total)",
  "Kindertagesstätten", "Daycare centers",
  "Kinos", "Cinemas",
  "Kinositzplätze", "Cinema seats",
  "Kinosäle", "Cinema screens",
  "Nicht öffentliche Bäder", "Non-public swimming pools",
  "Schüler an Grundschulen", "Primary school students",
  "Schüler an Grundschulen je 1.000 Einwohner", "Primary school students per 1,000 inhabitants",
  "Schüler an allgemeinbildenden Schulen", "General education school students",
  "Schüler an allgemeinbildenden Schulen je 1.000 Einwohner", "General education students per 1,000 inhabitants",
  "Schüler an allgemeinbildenden Schulen mit Förderschwerpunkt", "Special needs students in general education",
  "Schüler an allgemeinbildenden Schulen mit Förderschwerpunkt je 1.000 Einwohner", "Special needs students per 1,000 inhabitants",
  "Schüler an berufsbildenden Schulen", "Vocational school students",
  "Schüler an berufsbildenden Schulen je 1.000 Einwohner", "Vocational students per 1,000 inhabitants",
  "Schüler an weiterführenden Schulen", "Secondary school students",
  "Schüler an weiterführenden Schulen je 1.000 Einwohner", "Secondary students per 1,000 inhabitants",
  "Spezialbibliotheken", "Special libraries",
  "Studierende an Fachhochschulen je 1.000 Einwohner", "University of applied sciences students per 1,000 inhabitants",
  "Studierende an Hochschulen", "University students",
  "Studierende an Hochschulen je 1.000 Einwohner", "University students per 1,000 inhabitants",
  "Weiterführende Schulen", "Secondary schools",
  "Wissenschaftliche Bibliotheken", "Academic libraries",
  "Öffentliche Bibliotheken", "Public libraries",
  "Öffentliche Bäder", "Public swimming pools",

  # Zentrale Orte Monitoring -- Gesundheitsversorgungsfunktion (14 variables)
  "Apotheken", "Pharmacies",
  "Einwohner je Arzt", "Inhabitants per physician",
  "Krankenhäuser der Basisnotfallversorgung", "Hospitals with basic emergency care",
  "Krankenhäuser der Erweiterten Notfallversorgung", "Hospitals with extended emergency care",
  "Krankenhäuser der Umfassenden Notfallversorgung", "Hospitals with comprehensive emergency care",
  "Krankenhäuser insgesamt", "Total hospitals",
  "Krankenhäuser mit Nofallversorgung Kinder", "Hospitals with pediatric emergency care",
  "Krankenhäuser mit Schlaganfallversorgung", "Hospitals with stroke care",
  "Krankenhäuser mit Schwerverletztenversorgung", "Hospitals with major trauma care",
  "Krankenhäuser mit Spezialversorgung", "Hospitals with specialized care",
  "Krankenhäuser mit Versorgung für akute Herzerkrankungen", "Hospitals with acute cardiac care",
  "Krankenhäuser ohne allg. stationäre Notfallversorgung", "Hospitals without general inpatient emergency care",
  "Seniorenheime", "Senior homes",

  # Zentrale Orte Monitoring -- Rahmendaten Zentraler Orte (2 variables)
  "Bevölkerung", "Population",
  "Bevölkerungsentwicklung", "Population development",

  # Zentrale Orte Monitoring -- Verkehrs- und Kommunikationsfunktion (18 variables)
  "Bahn-Abfahrten", "Train departures",
  "Bahnhaltestellen", "Train stops",
  "Bus-Abfahrten", "Bus departures",
  "Bushaltestellen", "Bus stops",
  "Erreichbarkeit Autobahnen", "Highway accessibility",
  "Erreichbarkeit Flughäfen", "Airport accessibility",
  "Erreichbarkeit KV-Terminal", "Combined transport terminal accessibility",
  "Postagenturen der Deutschen Post AG", "German Post agencies",
  "Postfilialen der Deutschen Post AG", "German Post branches",
  "Postfillialen insgesamt", "Total post offices",
  "U-/Strassenbahnhaltestellen", "Metro/tram stops",
  "U-/Straßenbahn-Abfahrten", "Metro/tram departures",
  "hochfrequentierte Bahnhaltestellen", "High-frequency train stops",
  "hochfrequentierte Bushaltestellen", "High-frequency bus stops",
  "hochfrequentierte U-/Strassenbahnhaltestellen", "High-frequency metro/tram stops",
  "hochfrequentierte ÖV-Haltestellen", "High-frequency public transport stops",
  "ÖV-Abfahrten", "Public transport departures",
  "ÖV-Haltestellen", "Public transport stops",

  # Zentrale Orte Monitoring -- Wirtschafts- und Arbeitsmarktfunktion (9 variables)
  "Arbeitslosigkeit", "Unemployment",
  "Arbeitsplatzzentralität", "Workplace centrality",
  "Beschäftigtendichte (AO)", "Employee density (at workplace)",
  "Beschäftigtendichte (WO)", "Employee density (at residence)",
  "sozialversicherungspflichtig Beschäftigte Auspendler", "Insured employees -- out-commuters",
  "sozialversicherungspflichtig Beschäftigte Binnenpendler ", "Insured employees -- internal commuters",
  "sozialversicherungspflichtig Beschäftigte Einpendler", "Insured employees -- in-commuters",
  "sozialversicherungspflichtig Beschäftigte am Arbeitsort", "Insured employees at workplace",
  "sozialversicherungspflichtig Beschäftigte am Wohnort", "Insured employees at residence",

  # Zentrale Orte Monitoring -- Öffentliche Verwaltungsfunktion (17 variables)
  "Amtsgerichte", "District courts",
  "Arbeitsagenturen", "Employment agencies",
  "Arbeitsmarktverwaltungseinrichtungen", "Labor market administration facilities",
  "Finanzämter", "Tax offices",
  "Gemeindeverwaltungen", "Municipal administrations",
  "Gerichte", "Courts",
  "Jobcenter", "Job centers",
  "Kreisverwaltungen", "District administrations",
  "Landgerichte", "Regional courts",
  "Nebenstelle eines Amtsgerichts", "District court branch office",
  "Nebenstelle eines Landgerichts", "Regional court branch office",
  "Nebenstelle eines Oberlandesgerichts", "Higher regional court branch office",
  "Oberlandesgerichte", "Higher regional courts",
  "Polizeidienststellen", "Police stations",
  "Polizeidienststellen, durchgehend besetzt", "Police stations -- 24-hour staffed",
  "Polizeidienststellen, teilweise besetzt", "Police stations -- partially staffed",
  "Sonstige Arbeitsmarktverwaltungseinrichtungen", "Other labor market administration facilities",

  # Öffentliche Finanzen (11 variables)
  "Ausgaben für Sachinvestitionen", "Capital investment expenditure",
  "Einkommensteuer", "Income tax",
  "Gewerbesteuer", "Trade tax",
  "Kassenkredite", "Cash loans",
  "Kommunale Schulden", "Municipal debt",
  "Personal der Kommunen", "Municipal personnel",
  "Schlüsselzuweisungen", "General grants",
  "Steuereinnahmen", "Tax revenue",
  "Steuerkraft", "Tax capacity",
  "Umsatzsteuer", "Sales tax",
  "Zuweisungen für Investitionsfördermaßnahmen", "Investment promotion grants"
)

cat("  Loaded", nrow(translations_en), "translations\n\n")

# STEP 4: Join translations with matched data ---------------------------------
cat("STEP 4: Joining translations...\n")

df_matched <- df_matched %>%
  filter(!is.na(KurznamePlus)) %>%
  left_join(translations_en, by = c("KurznamePlus" = "german_name"))

# Check for missing translations
missing_trans <- df_matched %>% filter(is.na(english_name))
if (nrow(missing_trans) > 0) {
  cat("  WARNING:", nrow(missing_trans), "variables without translations:\n")
  print(missing_trans$KurznamePlus)
} else {
  cat("  All", nrow(df_matched), "variables have translations\n")
}
cat("\n")

# STEP 5: Organize by theme ---------------------------------------------------
cat("STEP 5: Organizing by thematic category...\n")

# Create theme summary
theme_summary <- df_matched %>%
  count(theme_name, theme_sub, sort = TRUE)

cat("  Thematic categories:", n_distinct(paste(df_matched$theme_name, df_matched$theme_sub)), "\n\n")

# Sort by theme, then by German name
df_table <- df_matched %>%
  arrange(theme_name, theme_sub, KurznamePlus) %>%
  mutate(row_num = row_number()) %>%
  select(row_num, KurznamePlus, english_name, theme_name, theme_sub, year_used)

# Theme name translations (German -> English)
theme_translations <- tribble(
  ~german, ~english,
  "Absolutzahlen", "Absolute Numbers",
  "Arbeitslosigkeit", "Unemployment",
  "Allgemein", "General",
  "Altersgruppen", "Age Groups",
  "Struktur", "Structure",
  "Bauen und Wohnen", "Construction and Housing",
  "Baulandmarkt und Bautätigkeit", "Land Market and Construction Activity",
  "Gebäude- und Wohnungsbestand", "Building and Housing Stock",
  "Beschäftigung und Erwerbstätigkeit", "Employment and Labor Force",
  "Atypische Beschäftigung", "Atypical Employment",
  "Qualifikation", "Qualification",
  "Wirtschafts- und Berufszweige", "Economic Sectors and Occupations",
  "Bevölkerung", "Population",
  "Altersstruktur", "Age Structure",
  "Bevölkerungsstruktur", "Population Structure",
  "Bundestagswahlen", "Federal Elections",
  "Natürliche Bevölkerungsbewegungen", "Natural Population Dynamics",
  "Sicherheit", "Security",
  "Wanderungen", "Migration",
  "Bildung", "Education",
  "Ausbildungsangebot", "Training Supply",
  "Schulische Bildung", "School Education",
  "Flächennutzung und Umwelt", "Land Use and Environment",
  "Flächennutzung", "Land Use",
  "Umwelt", "Environment",
  "Medizinische und soziale Versorgung", "Medical and Social Care",
  "Medizinische Versorgung", "Medical Care",
  "Soziale Versorgung", "Social Care",
  "Privateinkommen, Private Schulden", "Private Income and Debt",
  "Raumwirksame Mittel", "Spatially Effective Funds",
  "SDG-Indikatoren für Kommunen", "SDG Indicators for Municipalities",
  "Siedlungsstruktur", "Settlement Structure",
  "Sozialleistungen", "Social Benefits",
  "Bedarfsgemeinschaften", "Benefit Households",
  "Leistungsempfänger", "Benefit Recipients",
  "Transferleistungen", "Transfer Payments",
  "Verkehr und Erreichbarkeit", "Transport and Accessibility",
  "Erreichbarkeit", "Accessibility",
  "Pendler", "Commuters",
  "Straßenverkehr", "Road Traffic",
  "Wirtschaft", "Economy",
  "Fremdenverkehr", "Tourism",
  "Wirtschaftliche Leistung", "Economic Performance",
  "Zentrale Orte Monitoring", "Central Places Monitoring",
  "Bildungsversorgungs- und Kulturfunktion", "Education and Culture Function",
  "Gesundheitsversorgungsfunktion", "Healthcare Function",
  "Rahmendaten Zentraler Orte ", "Framework Data for Central Places",
  "Verkehrs- und Kommunikationsfunktion", "Transport and Communication Function",
  "Wirtschafts- und Arbeitsmarktfunktion", "Economy and Labor Market Function",
  "Öffentliche Verwaltungsfunktion", "Public Administration Function",
  "Öffentliche Finanzen", "Public Finances"
)

# Apply theme translations
df_table <- df_table %>%
  left_join(theme_translations, by = c("theme_name" = "german")) %>%
  rename(theme_name_en = english) %>%
  left_join(theme_translations, by = c("theme_sub" = "german")) %>%
  rename(theme_sub_en = english)

# STEP 6: Generate LaTeX table ------------------------------------------------
cat("STEP 6: Generating LaTeX table...\n")

# Escape special LaTeX characters
escape_latex <- function(x) {
  x <- str_replace_all(x, fixed("\\"), "\\textbackslash{}")
  x <- str_replace_all(x, fixed("&"), "\\&")
  x <- str_replace_all(x, fixed("%"), "\\%")
  x <- str_replace_all(x, fixed("$"), "\\$")
  x <- str_replace_all(x, fixed("#"), "\\#")
  x <- str_replace_all(x, fixed("_"), "\\_")
  x <- str_replace_all(x, fixed("{"), "\\{")
  x <- str_replace_all(x, fixed("}"), "\\}")
  x <- str_replace_all(x, fixed("~"), "\\textasciitilde{}")
  x <- str_replace_all(x, fixed("^"), "\\textasciicircum{}")
  # Handle German special characters
  x <- str_replace_all(x, fixed("ä"), "\\\"a")
  x <- str_replace_all(x, fixed("ö"), "\\\"o")
  x <- str_replace_all(x, fixed("ü"), "\\\"u")
  x <- str_replace_all(x, fixed("Ä"), "\\\"A")
  x <- str_replace_all(x, fixed("Ö"), "\\\"O")
  x <- str_replace_all(x, fixed("Ü"), "\\\"U")
  x <- str_replace_all(x, fixed("ß"), "\\ss{}")
  return(x)
}

# Apply escaping
df_table <- df_table %>%
  mutate(
    KurznamePlus_tex = escape_latex(KurznamePlus),
    english_name_tex = escape_latex(english_name),
    theme_name_tex = escape_latex(theme_name),
    theme_sub_tex = escape_latex(theme_sub),
    theme_name_en_tex = escape_latex(theme_name_en),
    theme_sub_en_tex = escape_latex(theme_sub_en)
  )

# Create LaTeX table content
latex_header <- c(
  "\\begin{longtable}{p{0.8cm}p{\\dimexpr0.5\\textwidth-0.4cm-2\\tabcolsep}p{\\dimexpr0.5\\textwidth-0.4cm-2\\tabcolsep}}",
  "\\caption{\\ac{INKAR} regional indicators used in the analysis. The table lists all 540 indicators organized by thematic category, showing original German names with English translations. For detailed descriptions, refer to the official \\ac{INKAR} documentation at \\url{https://www.inkar.de/}.}",
  "\\label{tbl:inkar_variables} \\\\",
  "\\cmidrule{1-3}",
  "\\textbf{\\#} & \\textbf{German Name} & \\textbf{English Translation} \\\\",
  "\\cmidrule{1-3}",
  "\\endfirsthead",
  "\\multicolumn{3}{c}{\\tablename\\ \\thetable{} -- continued from previous page} \\\\",
  "\\cmidrule{1-3}",
  "\\textbf{\\#} & \\textbf{German Name} & \\textbf{English Translation} \\\\",
  "\\cmidrule{1-3}",
  "\\endhead",
  "\\cmidrule{1-3}",
  "\\multicolumn{3}{r}{Continued on next page} \\\\",
  "\\endfoot",
  "\\cmidrule{1-3}",
  "\\endlastfoot",
  ""
)

# Group by theme and create rows
latex_rows <- c()
row_counter <- 0

# Create unique theme keys for iteration
df_table <- df_table %>%
  mutate(theme_key = paste0(theme_name, " -- ", theme_sub))

theme_keys <- unique(df_table$theme_key)

for (theme_key in theme_keys) {
  theme_data <- df_table %>%
    filter(theme_key == !!theme_key)

  # Get German and English theme names
  theme_de <- paste0(theme_data$theme_name_tex[1], " -- ", theme_data$theme_sub_tex[1])
  theme_en <- paste0(theme_data$theme_name_en_tex[1], " -- ", theme_data$theme_sub_en_tex[1])

  # Add theme header
  latex_rows <- c(latex_rows,
                  "\\cmidrule{1-3}",
                  paste0("\\multicolumn{3}{p{\\dimexpr\\textwidth-2\\tabcolsep}}{\\textit{", theme_de, " (", theme_en, ")}} \\\\"),
                  "\\cmidrule{1-3}")

  # One row per variable
  for (i in 1:nrow(theme_data)) {
    row_counter <- row_counter + 1
    latex_rows <- c(latex_rows,
                    paste0(row_counter, " & ",
                           theme_data$KurznamePlus_tex[i], " & ",
                           theme_data$english_name_tex[i], " \\\\"))
  }
}

latex_footer <- c(
  "\\end{longtable}"
)

# Combine all parts
latex_content <- c(latex_header, latex_rows, latex_footer)

# STEP 7: Save output ---------------------------------------------------------
cat("STEP 7: Saving LaTeX table...\n")

output_dir <- file.path(data_path, "Tex", "Tables")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

output_file <- file.path(output_dir, "tbl_inkar_variables.tex")
writeLines(latex_content, output_file)

cat("  Saved:", output_file, "\n")
cat("  Lines:", length(latex_content), "\n\n")

# Also save a CSV for reference
csv_file <- file.path(output_dir, "tbl_inkar_variables.csv")
df_table %>%
  select(row_num, german_name = KurznamePlus, english_name, theme_name, theme_sub, year_used) %>%
  write_csv(csv_file)
cat("  Also saved CSV:", csv_file, "\n\n")

# Summary ---------------------------------------------------------------------
cat("==================================================\n")
cat("INKAR Appendix Table Generated\n")
cat("==================================================\n\n")

cat("Summary:\n")
cat("  Total variables:", nrow(df_table), "\n")
cat("  Thematic categories:", n_distinct(paste(df_table$theme_name, df_table$theme_sub)), "\n")
cat("  Variables with 2017 data:", sum(df_table$year_used == 2017), "\n")
cat("  Variables with other years:", sum(df_table$year_used != 2017), "\n\n")

cat("Output files:\n")
cat("  1. LaTeX table:", output_file, "\n")
cat("  2. CSV reference:", csv_file, "\n\n")

cat("To include in manuscript, add:\n")
cat("  \\input{data/Tex/Tables/tbl_inkar_variables}\n")

################################################################################
# END OF SCRIPT
################################################################################
