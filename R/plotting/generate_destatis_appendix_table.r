################################################################################
# Generate Census/Regional Statistics Appendix Table
#
# Purpose: Create a LaTeX longtable listing all Destatis tables with:
#          - Table code (Destatis classification)
#          - Description (German + English)
#          - Organized by thematic category
#
# Input:  Hardcoded table definitions (38 tables in 8 categories)
#         Based on census_data_acquisition.r configuration
#
# Output: data/Tex/Tables/tbl_admin_stats_tables.tex
#
# Note: This script reproduces the table that was originally created directly
#       in the LaTeX document. Created for documentation and consistency with
#       the other appendix table generators (generate_mid_appendix_table.r,
#       generate_inkar_appendix_table.r).
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
cat("Generate Census/Regional Statistics Appendix Table\n")
cat("==================================================\n\n")

# STEP 1: Define all census tables --------------------------------------------
cat("STEP 1: Defining census table metadata...\n")

# Define all 38 Destatis tables organized by thematic category
# This matches the configuration in census_data_acquisition.r
census_tables <- tribble(
  ~table_code, ~category, ~description_de, ~description_en,

  # Category 1: Population & Households (3 tables)
  "12411-01-01-4", "Population & Households",
  "Bevölkerung: Deutschland, Stichtag, Geschlecht, Kreise",
  "Population by sex, reference date, districts",

  "12411-09-01-4", "Population & Households",
  "Bevölkerung: Deutschland, Stichtag, Geschlecht, Altersgruppen",
  "Population by sex and age groups, reference date, districts",

  "12211-Z-10", "Population & Households",
  "Haushalte nach Haushaltsgröße",
  "Households by household size",

  # Category 2: Income & Economic Activity (3 tables)
  "73111-01-01-4", "Income & Economic Activity",
  "Lohn- und Einkommensteuerpflichtige, Gesamtbetrag der Einkünfte, Lohn- und Einkommensteuer",
  "Income tax payers, total income, income tax",

  "82000-07-01-4", "Income & Economic Activity",
  "Verfügbares Einkommen der privaten Haushalte",
  "Disposable income of private households",

  "71231-01-03-4", "Income & Economic Activity",
  "Realsteuervergleich: Hebesätze der Realsteuern",
  "Municipal tax rates comparison",

  # Category 3: Mobility & Transport (9 tables)
  "46251-0020", "Mobility & Transport",
  "Fahrzeugbestand: Kreise, Quartale, Fahrzeugklassen",
  "Vehicle registrations by districts, quarters, vehicle classes",

  "13111-01-03-4", "Mobility & Transport",
  "Erwerbstätige am Arbeitsort: Geschlecht, Nationalität, Kreise",
  "Employed persons at workplace by sex, nationality, districts",

  "13111-02-02-4", "Mobility & Transport",
  "Erwerbstätige am Wohnort: Geschlecht, Nationalität, Kreise",
  "Employed persons at residence by sex, nationality, districts",

  "13111-07-05-4", "Mobility & Transport",
  "Sozialversicherungspflichtig Beschäftigte am Arbeitsort nach Geschlecht, Nationalität, WZ",
  "Employees at workplace by sex, nationality, economic sector",

  "13111-11-04-4", "Mobility & Transport",
  "Sozialversicherungspflichtig Beschäftigte am Arbeitsort nach Geschlecht, Nationalität, Ausbildung",
  "Employees at workplace by sex, nationality, education level",

  "13111-04-02-4", "Mobility & Transport",
  "Sozialversicherungspflichtig Beschäftigte am Wohnort nach Geschlecht, Nationalität, Beschäftigungsumfang",
  "Employees at residence by sex, nationality, full-time/part-time",

  "13111-06-02-4", "Mobility & Transport",
  "Sozialversicherungspflichtig Beschäftigte am Wohnort nach Geschlecht, Nationalität, Altersgruppen",
  "Employees at residence by sex, nationality, age groups",

  "13111-12-03-4", "Mobility & Transport",
  "Sozialversicherungspflichtig Beschäftigte am Wohnort nach Geschlecht, Nationalität, Ausbildung",
  "Employees at residence by sex, nationality, education level",

  "12711-04-02-4", "Mobility & Transport",
  "Zu- und Fortzüge über Kreisgrenzen",
  "In-migration and out-migration across district boundaries",

  # Category 4: Tourism (4 tables)
  "45412-03-01-4", "Tourism",
  "Gästeübernachtungen, Gästeankünfte nach Herkunft -- Jahressumme bis 2017",
  "Guest overnight stays and arrivals by origin, annual total until 2017",

  "45412-01-03-4", "Tourism",
  "Beherbergungsbetriebe, Schlafgelegenheiten, Gästeankünfte, Gästeübernachtungen ab 2018",
  "Accommodation establishments, beds, guest arrivals, overnight stays from 2018",

  "45412-02-02-4", "Tourism",
  "Beherbergungsbetriebe nach Betriebsarten ab 2018",
  "Accommodation establishments by type from 2018",

  "45412-03-02-4", "Tourism",
  "Gästeankünfte und Gästeübernachtungen nach Herkunftsgebieten ab 2018",
  "Guest arrivals and overnight stays by region of origin from 2018",

  # Category 5: Demographics (7 tables)
  "12411-07-01-4", "Demographics",
  "Durchschnittsalter der Bevölkerung: Kreise, Stichtag",
  "Mean age of population, districts, reference date",

  "12411-08-01-4", "Demographics",
  "Jugend- und Altenquotient: Kreise, Stichtag",
  "Youth and old-age dependency ratios, districts, reference date",

  "12411-10-01-4", "Demographics",
  "Medianalter der Bevölkerung nach Geschlecht: Kreise",
  "Median age of population by sex, districts",

  "12411-11-01-4", "Demographics",
  "Bevölkerung: Nationalität Deutsche/Ausländer, Geschlecht",
  "Population by nationality (German/foreign), sex",

  "12711-01-03-4", "Demographics",
  "Zu- und Fortzüge über Gemeindegrenzen",
  "In-migration and out-migration across municipality boundaries",

  "12612-01-01-4", "Demographics",
  "Lebendgeborene nach Geschlecht: Kreise",
  "Live births by sex, districts",

  "12613-01-01-4", "Demographics",
  "Gestorbene nach Geschlecht: Kreise",
  "Deaths by sex, districts",

  # Category 6: Employment & Economy (4 tables)
  "13111-03-02-4", "Employment & Economy",
  "Erwerbstätige: Geschlecht, Stellung im Beruf, Kreise",
  "Employed persons by sex, occupational status, districts",

  "13111-05-03-4", "Employment & Economy",
  "Erwerbstätige: Geschlecht, Altersgruppen, Kreise",
  "Employed persons by sex, age groups, districts",

  "13211-02-05-4", "Employment & Economy",
  "Arbeitslose und Arbeitslosenquote: Kreise",
  "Unemployed persons and unemployment rate, districts",

  "13312-01-05-4", "Employment & Economy",
  "Erwerbstätige: Wirtschaftsbereiche, Kreise",
  "Employed persons by economic sector, districts",

  # Category 7: Infrastructure & Built Environment (6 tables)
  "11111-01-01-4", "Infrastructure & Built Environment",
  "Fläche insgesamt: Kreise, Stichtag",
  "Total area, districts, reference date",

  "31111-01-02-4", "Infrastructure & Built Environment",
  "Baugenehmigungen: Wohngebäude, Kreise",
  "Building permits for residential buildings, districts",

  "31121-01-02-4", "Infrastructure & Built Environment",
  "Baufertigstellungen: Wohngebäude, Kreise",
  "Building completions for residential buildings, districts",

  "31231-02-01-4", "Infrastructure & Built Environment",
  "Wohngebäude- und Wohnungsbestand: Kreise",
  "Residential building and dwelling stock, districts",

  "33111-01-02-4", "Infrastructure & Built Environment",
  "Bodenfläche nach Art der tatsächlichen Nutzung: Kreise",
  "Land area by type of actual use, districts",

  "33111-02-01-4", "Infrastructure & Built Environment",
  "Siedlungs- und Verkehrsfläche nach Nutzungsarten: Kreise",
  "Settlement and transport area by type of use, districts",

  # Category 8: Education & Social Services (2 tables)
  "21111-01-03-4", "Education & Social Services",
  "Allgemeinbildende Schulen: Schulen, Schüler/-innen, Kreise",
  "General education schools: schools, students, districts",

  "22541-01-04-4", "Education & Social Services",
  "Tageseinrichtungen für Kinder: Einrichtungen, Plätze, Kreise",
  "Childcare facilities: establishments, places, districts"
)

cat("  Total tables defined:", nrow(census_tables), "\n")
cat("  Categories:", n_distinct(census_tables$category), "\n\n")

# STEP 2: Summary by category -------------------------------------------------
cat("STEP 2: Summarizing by category...\n")

category_summary <- census_tables %>%
  count(category) %>%
  arrange(match(category, c(
    "Population & Households",
    "Income & Economic Activity",
    "Mobility & Transport",
    "Tourism",
    "Demographics",
    "Employment & Economy",
    "Infrastructure & Built Environment",
    "Education & Social Services"
  )))

for (i in 1:nrow(category_summary)) {
  cat("  ", category_summary$category[i], ": ", category_summary$n[i], " tables\n", sep = "")
}
cat("\n")

# STEP 3: Generate LaTeX table ------------------------------------------------
cat("STEP 3: Generating LaTeX table...\n")

# Define category order (matches the manuscript structure)
category_order <- c(
  "Population & Households",
  "Income & Economic Activity",
  "Mobility & Transport",
  "Tourism",
  "Demographics",
  "Employment & Economy",
  "Infrastructure & Built Environment",
  "Education & Social Services"
)

# Escape special LaTeX characters in descriptions
escape_latex <- function(x) {
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

# Apply escaping to descriptions
census_tables <- census_tables %>%
  mutate(
    description_de_tex = escape_latex(description_de),
    description_en_tex = escape_latex(description_en),
    # Combined description: English (German in parentheses)
    description_combined = paste0(description_en_tex, " (", description_de_tex, ")")
  )

# LaTeX header
latex_header <- c(
  "\\begin{longtable}{@{}p{2cm}p{\\dimexpr\\textwidth-2cm-4\\tabcolsep}@{}}",
  "\\caption{Destatis tables acquired as candidate auxiliary variables. Table codes follow the Destatis classification scheme. All tables refer to the 2017 reference year at district level (\\emph{Kreise}).}",
  "\\label{tbl:admin_stats_tables} \\\\",
  "\\cmidrule{1-2}",
  "\\textbf{Table Code} & \\textbf{Description} \\\\",
  "\\cmidrule{1-2}",
  "\\endfirsthead",
  "\\multicolumn{2}{c}{\\tablename\\ \\thetable{} -- continued from previous page} \\\\",
  "\\cmidrule{1-2}",
  "\\textbf{Table Code} & \\textbf{Description} \\\\",
  "\\cmidrule{1-2}",
  "\\endhead",
  "\\cmidrule{1-2}",
  "\\multicolumn{2}{r}{Continued on next page} \\\\",
  "\\endfoot",
  "\\cmidrule{1-2}",
  "\\endlastfoot"
)

# Generate rows grouped by category
latex_rows <- c()

for (cat_name in category_order) {
  cat_data <- census_tables %>%
    filter(category == cat_name)

  n_tables <- nrow(cat_data)

  # Add category header (escape & in category name for LaTeX)
  cat_name_tex <- str_replace_all(cat_name, fixed("&"), "\\&")
  latex_rows <- c(latex_rows,
                  paste0("% ", cat_name),
                  paste0("\\multicolumn{2}{p{\\dimexpr\\textwidth-2\\tabcolsep}}{\\textit{",
                         cat_name_tex, " (", n_tables, " tables)}} \\\\"),
                  "\\cmidrule{1-2}")

  # Add table rows
  for (i in 1:nrow(cat_data)) {
    latex_rows <- c(latex_rows,
                    paste0(cat_data$table_code[i], " & ",
                           cat_data$description_combined[i], " \\\\"))
  }

  # Add separator after category (except last)
  if (cat_name != category_order[length(category_order)]) {
    latex_rows <- c(latex_rows, "\\cmidrule{1-2}")
  }
}

latex_footer <- c(
  "\\end{longtable}"
)

# Combine all parts
latex_content <- c(latex_header, latex_rows, latex_footer)

# STEP 4: Save output ---------------------------------------------------------
cat("STEP 4: Saving LaTeX table...\n")

output_dir <- file.path(data_path, "Tex", "Tables")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

output_file <- file.path(output_dir, "tbl_admin_stats_tables.tex")
writeLines(latex_content, output_file)

cat("  Saved:", output_file, "\n")
cat("  Lines:", length(latex_content), "\n\n")

# Also save CSV for reference
csv_file <- file.path(output_dir, "tbl_admin_stats_tables.csv")
census_tables %>%
  select(table_code, category, description_de, description_en) %>%
  write_csv(csv_file)
cat("  Also saved CSV:", csv_file, "\n\n")

# Summary ---------------------------------------------------------------------
cat("==================================================\n")
cat("Census/Regional Statistics Appendix Table Generated\n")
cat("==================================================\n\n")

cat("Summary:\n")
cat("  Total tables:", nrow(census_tables), "\n")
cat("  Thematic categories:", n_distinct(census_tables$category), "\n\n")

cat("Tables by category:\n")
for (i in 1:nrow(category_summary)) {
  cat("  ", sprintf("%-35s", category_summary$category[i]), ": ",
      category_summary$n[i], "\n", sep = "")
}

cat("\nOutput files:\n")
cat("  1. LaTeX table:", output_file, "\n")
cat("  2. CSV reference:", csv_file, "\n\n")

cat("To include in manuscript, add:\n")
cat("  \\input{../data/Tex/Tables/tbl_admin_stats_tables}\n\n")

cat("NOTE: The manuscript currently has this table hardcoded in the .tex file.\n")
cat("      This script is provided for documentation and consistency purposes.\n")
cat("      To use the generated version, replace the hardcoded table with:\n")
cat("      {\\small\n")
cat("      \\input{../data/Tex/Tables/tbl_admin_stats_tables}\n")
cat("      }% end \\small\n")

################################################################################
# END OF SCRIPT
################################################################################
