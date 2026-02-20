################################################################################
# Generate Final Variable Set Table
#
# Purpose: Create a LaTeX longtable listing all variables in the final
#          (winning) model from the 3-phase variable selection pipeline.
#          The table shows:
#          - Variable name (cleaned)
#          - Type (Main, Poly, Inter)
#          - Source (MiD, INKAR, Destatis, Mode)
#          - English description
#
# Input:
#   - data/output/variables/phase2_sparseR_results_baseline.rds (winning configuration)
#   - English translations from existing appendix generators
#
# Output:
#   - data/Tex/Tables/tbl_final_varset.tex
#
# Created: 2026-01-23
# Updated: 2026-02-04 - Use baseline configuration (winning model)
################################################################################

# Load packages ---------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  stringr,
  readxl
)

# Set paths -------------------------------------------------------------------
data_path <- "./data"

cat("\n==================================================\n")
cat("Generate Final Variable Set Table\n")
cat("==================================================\n\n")

# ==============================================================================
# STEP 1: Load final variable set from Phase 2 results
# ==============================================================================
cat("STEP 1: Loading Phase 2 sparseR results (baseline configuration)...\n")

# Load baseline configuration (the winning model from 5-candidate comparison)
phase2 <- readRDS(file.path(data_path, "output/variables/phase2_sparseR_results_baseline.rds"))

# Get the winning model's variable categories
main_effects <- phase2$categories_min$main_effects
polynomials <- phase2$categories_min$polynomials
interactions <- phase2$categories_min$interactions

cat("  Main effects: ", length(main_effects), "\n", sep = "")
cat("  Polynomials: ", length(polynomials), "\n", sep = "")
cat("  Interactions: ", length(interactions), "\n", sep = "")
cat("  Total: ", length(main_effects) + length(polynomials) + length(interactions), "\n\n", sep = "")

# ==============================================================================
# STEP 2: Parse variable names to extract base variable and type
# ==============================================================================
cat("STEP 2: Parsing variable names...\n")

# Function to parse a sparseR variable name
parse_varname <- function(varname) {
  # Initialize
  base_var <- varname
  var_type <- "Main"
  source <- "Unknown"
  poly_order <- 1  # Track polynomial order for display

  # Check if interaction (contains ":")
  if (str_detect(varname, ":")) {
    var_type <- "Inter"
    # Extract both parts
    parts <- str_split(varname, ":", simplify = TRUE)
    base_var <- paste(parts[1], "×", parts[2])

    # For interactions, determine source of both parts and combine
    part1 <- parts[1]
    part2 <- parts[2]

    get_source <- function(x) {
      if (str_detect(x, "^mode_C[123]")) return("MiD")
      if (str_detect(x, "^mid_")) return("MiD")
      if (str_detect(x, "^inkar_")) return("INKAR")
      if (str_detect(x, "^census_|^destatis_")) return("Destatis")
      return("Unknown")
    }

    source1 <- get_source(part1)
    source2 <- get_source(part2)

    # Combine sources (e.g., "MiD × INKAR")
    if (source1 == source2) {
      source <- source1
    } else {
      source <- paste0(source1, " × ", source2)
    }
  } else if (str_detect(varname, "_poly_([2-9])$")) {
    # Polynomial term (squared, cubed, etc.)
    var_type <- "Poly"
    poly_order <- as.numeric(str_extract(varname, "[2-9]$"))
    base_var <- str_remove(varname, "_poly_[2-9]$")
  } else if (str_detect(varname, "_poly_1$")) {
    # Main effect (linear term)
    var_type <- "Main"
    poly_order <- 1
    base_var <- str_remove(varname, "_poly_1$")
  }

  # Determine source (skip for interactions - already set above)
  if (var_type != "Inter") {
    if (str_detect(base_var, "^mode_C[123]")) {
      source <- "MiD"  # Mode contrasts come from MiD survey
    } else if (str_detect(base_var, "^mid_")) {
      source <- "MiD"
    } else if (str_detect(base_var, "^inkar_")) {
      source <- "INKAR"
    } else if (str_detect(base_var, "^census_|^destatis_")) {
      source <- "Destatis"
    }
  }

  return(tibble(
    raw_name = varname,
    base_var = base_var,
    var_type = var_type,
    source = source,
    poly_order = poly_order
  ))
}

# Parse all variables
df_main <- map_dfr(main_effects, parse_varname)
df_poly <- map_dfr(polynomials, parse_varname)
df_inter <- map_dfr(interactions, parse_varname)

df_vars <- bind_rows(df_main, df_poly, df_inter)

cat("  Parsed ", nrow(df_vars), " variables\n", sep = "")
cat("  By source:\n")
print(table(df_vars$source))
cat("\n")

# ==============================================================================
# STEP 3: Load appendix table lookups for row numbers
# ==============================================================================
cat("STEP 3: Loading appendix table lookups for row numbers...\n")

# --- Load MiD appendix table CSV and add row numbers ---
mid_csv_file <- file.path(data_path, "Tex", "Tables", "tbl_mid_variables.csv")
if (file.exists(mid_csv_file)) {
  mid_lookup <- read_csv(mid_csv_file, show_col_types = FALSE) %>%
    mutate(row_num = row_number())  # Row number matches LaTeX table
  cat("  Loaded MiD lookup: ", nrow(mid_lookup), " variables\n", sep = "")
} else {
  mid_lookup <- tibble(Variable = character(), row_num = integer())
  cat("  WARNING: MiD appendix CSV not found\n")
}

# --- Load INKAR appendix table CSV (already has row_num) ---
inkar_csv_file <- file.path(data_path, "Tex", "Tables", "tbl_inkar_variables.csv")
if (file.exists(inkar_csv_file)) {
  inkar_lookup <- read_csv(inkar_csv_file, show_col_types = FALSE)
  cat("  Loaded INKAR lookup: ", nrow(inkar_lookup), " variables\n", sep = "")
} else {
  inkar_lookup <- tibble(german_name = character(), row_num = integer())
  cat("  WARNING: INKAR appendix CSV not found\n")
}

# --- Load Census/Destatis appendix table CSV ---
census_csv_file <- file.path(data_path, "Tex", "Tables", "tbl_admin_stats_tables.csv")
if (file.exists(census_csv_file)) {
  census_lookup <- read_csv(census_csv_file, show_col_types = FALSE)
  cat("  Loaded Census lookup: ", nrow(census_lookup), " tables\n", sep = "")
} else {
  census_lookup <- tibble(table_code = character())
  cat("  WARNING: Census appendix CSV not found\n")
}

# --- Load Census variable translations CSV ---
census_trans_file <- file.path(data_path, "Tex", "Tables", "tbl_census_translations.csv")
if (file.exists(census_trans_file)) {
  census_translations <- read_csv(census_trans_file, show_col_types = FALSE)
  cat("  Loaded Census translations: ", nrow(census_translations), " variables\n", sep = "")
} else {
  census_translations <- tibble(variable_name = character(), english_name = character())
  cat("  WARNING: Census translations CSV not found - will use fallback descriptions\n")
}

# --- Load INKAR metadata for variable name matching ---
inkar_meta_file <- file.path(data_path, "temp", "inkar_variable_metadata.rds")
if (file.exists(inkar_meta_file)) {
  inkar_meta <- readRDS(inkar_meta_file) %>%
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
    # Join with inkar_lookup to get row numbers AND English names
    left_join(
      inkar_lookup %>% select(german_name, row_num, english_name),
      by = c("KurznamePlus" = "german_name")
    )
  cat("  Loaded INKAR metadata with English names\n")
} else {
  inkar_meta <- tibble(var_clean = character(), row_num = integer(), english_name = character())
  cat("  WARNING: INKAR metadata not found\n")
}

cat("\n")

# ==============================================================================
# STEP 4: Add descriptions to variables
# ==============================================================================
cat("STEP 4: Adding descriptions...\n")

# Function to generate interaction description
make_interaction_desc <- function(raw_name) {
  parts <- str_split(raw_name, ":", simplify = TRUE)
  part1 <- parts[1]
  part2 <- parts[2]

  # Get mode name from first part (with German abbreviations)
  mode_name <- case_when(
    str_detect(part1, "mode_C1") ~ "Walking (Fuss)",
    str_detect(part1, "mode_C2") ~ "Cycling (Rad)",
    str_detect(part1, "mode_C3") ~ "Car (MIV)",
    TRUE ~ part1
  )

  # Clean covariate name from second part
  covar_name <- str_remove(part2, "^(mid_|inkar_|census_)")
  covar_name <- str_replace_all(covar_name, "_", " ")

  paste0(mode_name, " × ", covar_name)
}

# Function to get MiD description - look up English label in appendix table
make_mid_desc <- function(base_var) {
  # Extract the core variable name for lookup
  clean <- str_remove(base_var, "^mid_")
  clean <- str_remove(clean, "_(mean|prop_[0-9]+)$")

  # Look up English label in mid_lookup
  match <- mid_lookup %>%
    filter(str_to_upper(Variable) == str_to_upper(clean))

  if (nrow(match) > 0 && "Label_EN" %in% names(match)) {
    return(match$Label_EN[1])
  }

  # Fallback: return cleaned variable name
  str_replace_all(clean, "_", " ")
}

# Function to get INKAR description - look up English name via metadata
make_inkar_desc <- function(base_var) {
  # Look up English name via inkar_meta (joined with inkar_lookup)
  match <- inkar_meta %>%
    filter(var_clean == base_var)

  if (nrow(match) > 0 && !is.na(match$english_name[1])) {
    return(match$english_name[1])
  }

  # Fallback: return cleaned variable name
  clean <- str_remove(base_var, "^inkar_")
  str_replace_all(clean, "_", " ")
}

# Function to get Census/Destatis description - look up English translation
make_census_desc <- function(base_var) {
  # Look up in census_lookup (loaded in STEP 3)
  if (exists("census_translations") && nrow(census_translations) > 0) {
    match <- census_translations %>%
      filter(variable_name == base_var)
    if (nrow(match) > 0 && !is.na(match$english_name[1])) {
      return(match$english_name[1])
    }
  }

  # Fallback: clean variable name
  clean <- str_remove(base_var, "^census_")
  clean <- str_replace_all(clean, "_", " ")
  str_to_title(clean)
}

# Apply descriptions row by row
df_vars <- df_vars %>%
  rowwise() %>%
  mutate(
    description = case_when(
      # Mode contrasts (sum-to-zero coding, public transit coded as -1)
      base_var == "mode_C1" ~ "Walking (Fuss) contrast, mode_C1, OEPV = -1",
      base_var == "mode_C2" ~ "Cycling (Rad) contrast, mode_C2, OEPV = -1",
      base_var == "mode_C3" ~ "Car (MIV) contrast, mode_C3, OEPV = -1",

      # Interactions
      var_type == "Inter" ~ make_interaction_desc(raw_name),

      # MiD variables
      source == "MiD" ~ make_mid_desc(base_var),

      # INKAR variables
      source == "INKAR" ~ make_inkar_desc(base_var),

      # Census variables
      source == "Destatis" ~ make_census_desc(base_var),

      TRUE ~ str_replace_all(base_var, "_", " ")
    )
  ) %>%
  ungroup()

cat("  Descriptions added\n\n")

# ==============================================================================
# STEP 5: Format for LaTeX table
# ==============================================================================
cat("STEP 5: Formatting for LaTeX...\n")

# Escape special LaTeX characters
escape_latex <- function(x) {
  # Protect existing LaTeX math expressions ($...$) before escaping
  # Use placeholder that won't appear in text
  math_pattern <- "\\$[^\\$]+\\$"
  math_matches <- str_extract_all(x, math_pattern)[[1]]

  # Replace math expressions with unique placeholders
  if (length(math_matches) > 0) {
    for (i in seq_along(math_matches)) {
      x <- str_replace(x, fixed(math_matches[i]), paste0("__MATH", i, "__"))
    }
  }

  # Escape special characters (skip $ since we protected math)
  x <- str_replace_all(x, fixed("&"), "\\&")
  x <- str_replace_all(x, fixed("%"), "\\%")
  x <- str_replace_all(x, fixed("#"), "\\#")
  x <- str_replace_all(x, fixed("_"), "\\_")
  x <- str_replace_all(x, fixed("~"), "\\textasciitilde{}")
  x <- str_replace_all(x, fixed("^"), "\\textasciicircum{}")
  # Less-than and greater-than (use math mode)
  x <- str_replace_all(x, fixed("<"), "$<$")
  x <- str_replace_all(x, fixed(">"), "$>$")
  # German characters
  x <- str_replace_all(x, fixed("ä"), "\\\"a")
  x <- str_replace_all(x, fixed("ö"), "\\\"o")
  x <- str_replace_all(x, fixed("ü"), "\\\"u")
  x <- str_replace_all(x, fixed("Ä"), "\\\"A")
  x <- str_replace_all(x, fixed("Ö"), "\\\"O")
  x <- str_replace_all(x, fixed("Ü"), "\\\"U")
  x <- str_replace_all(x, fixed("ß"), "\\ss{}")

  # Restore math expressions (after underscore escaping, need to fix escaped placeholder)
  if (length(math_matches) > 0) {
    for (i in seq_along(math_matches)) {
      # Placeholder got escaped to \_\_MATH1\_\_ so match that
      x <- str_replace(x, fixed(paste0("\\_\\_MATH", i, "\\_\\_")), math_matches[i])
    }
  }

  return(x)
}

# Helper function to get polynomial superscript
get_poly_superscript <- function(order) {
  # Unicode superscripts for orders 2-9
  superscripts <- c("²", "³", "⁴", "⁵", "⁶", "⁷", "⁸", "⁹")
  if (order >= 2 && order <= 9) {
    return(superscripts[order - 1])
  }
  return("")  # No superscript for order 1
}

# Clean up variable names for display
df_vars <- df_vars %>%
  rowwise() %>%
  mutate(
    # Create display name (remove prefixes for cleaner look)
    display_name = base_var,
    display_name = str_remove(display_name, "^(mid_|inkar_|census_)"),
    display_name = str_remove(display_name, "_(mean|prop_[0-9]+)$"),
    # Replace underscores with spaces for readability
    display_name = str_replace_all(display_name, "_", " "),

    # Replace mode contrast codes with actual mode names (with German abbreviations)
    display_name = case_when(
      display_name == "mode C1" ~ "Walking (Fuss)",
      display_name == "mode C2" ~ "Cycling (Rad)",
      display_name == "mode C3" ~ "Car (MIV)",
      TRUE ~ display_name
    ),

    # Add polynomial superscript for squared/cubed terms
    display_name = if (poly_order >= 2) {
      paste0(display_name, get_poly_superscript(poly_order))
    } else {
      display_name
    },

    # Escape for LaTeX (no underscores left to escape in display_name)
    display_name_tex = escape_latex(display_name),
    # Escape description, then convert markers back to LaTeX commands
    description_tex = escape_latex(description),
    description_tex = str_replace_all(description_tex, fixed("CREF\\_START"), "\\Cref{"),
    description_tex = str_replace_all(description_tex, fixed("CREF\\_END"), "}"),
    description_tex = str_replace_all(description_tex, fixed("UNDERSCORE\\_LITERAL"), "_"),
    description_tex = str_replace_all(description_tex, fixed("HASH"), "\\#")
  ) %>%
  ungroup()

# Sort by type, then source, then name
df_vars <- df_vars %>%
  mutate(
    type_order = case_when(
      var_type == "Main" ~ 1,
      var_type == "Poly" ~ 2,
      var_type == "Inter" ~ 3
    ),
    source_order = case_when(
      source == "MiD" ~ 1,
      source == "INKAR" ~ 2,
      source == "Destatis" ~ 3,
      TRUE ~ 4
    )
  ) %>%
  arrange(type_order, source_order, display_name)

# ==============================================================================
# STEP 6: Generate LaTeX longtable
# ==============================================================================
cat("STEP 6: Generating LaTeX longtable...\n")

# Table header - use raggedright in all p{} columns for left alignment
latex_header <- c(
  "\\begin{longtable}{@{}>{{\\raggedright\\arraybackslash}}p{4.5cm}>{{\\raggedright\\arraybackslash}}p{1.2cm}>{{\\raggedright\\arraybackslash}}p{1.2cm}>{{\\raggedright\\arraybackslash}}p{\\dimexpr\\textwidth-6.9cm-6\\tabcolsep}@{}}",
  paste0("\\caption{Final variable set from the winning baseline $\\lambda_{\\min}$ model (", nrow(df_vars), " variables). Variables are grouped by type: main effects, polynomial (squared) terms, and mode $\\times$ covariate interactions.}"),
  "\\label{tbl:final_varset} \\\\",
  "\\hline",
  "\\textbf{Variable} & \\textbf{Type} & \\textbf{Source} & \\textbf{Description} \\\\",
  "\\hline",
  "\\endfirsthead",
  "\\multicolumn{4}{c}{\\tablename\\ \\thetable{} -- continued from previous page} \\\\",
  "\\hline",
  "\\textbf{Variable} & \\textbf{Type} & \\textbf{Source} & \\textbf{Description} \\\\",
  "\\hline",
  "\\endhead",
  "\\hline",
  "\\multicolumn{4}{r}{Continued on next page} \\\\",
  "\\endfoot",
  "\\hline",
  "\\endlastfoot"
)

# Generate rows grouped by type
latex_rows <- c()
current_type <- ""

for (i in 1:nrow(df_vars)) {
  row <- df_vars[i, ]

  # Add section header if type changes
  if (row$var_type != current_type) {
    current_type <- row$var_type
    n_in_type <- sum(df_vars$var_type == current_type)

    latex_rows <- c(latex_rows,
                    paste0("\\multicolumn{4}{l}{\\textit{", current_type,
                           " effects (", n_in_type, " variables)}} \\\\"),
                    "\\hline")
  }

  # Add row with horizontal line for separation
  latex_rows <- c(latex_rows,
                  paste0(row$display_name_tex, " & ",
                         row$var_type, " & ",
                         row$source, " & ",
                         row$description_tex, " \\\\"),
                  "\\hline")
}

# Table footer
latex_footer <- c(
  "\\end{longtable}"
)

# Combine
latex_content <- c(latex_header, latex_rows, latex_footer)

cat("  Generated ", length(latex_rows), " data rows\n", sep = "")
cat("  Total lines: ", length(latex_content), "\n\n", sep = "")

# ==============================================================================
# STEP 7: Save output
# ==============================================================================
cat("STEP 7: Saving LaTeX table...\n")

output_dir <- "./manuscript"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

output_file <- file.path(output_dir, "tbl_final_varset.tex")
writeLines(latex_content, output_file)
cat("  Saved: ", output_file, "\n\n", sep = "")

# Also save CSV for reference
csv_file <- file.path(data_path, "Tex", "Tables", "tbl_final_varset.csv")
df_vars %>%
  select(raw_name, var_type, source, base_var, description) %>%
  write_csv(csv_file)
cat("  Also saved CSV: ", csv_file, "\n\n", sep = "")

# ==============================================================================
# Summary
# ==============================================================================
cat("==================================================\n")
cat("Final Variable Set Table Generated\n")
cat("==================================================\n\n")

cat("Summary:\n")
cat("  Total variables: ", nrow(df_vars), "\n", sep = "")
cat("\nBy type:\n")
print(table(df_vars$var_type))
cat("\nBy source:\n")
print(table(df_vars$source))

cat("\nOutput files:\n")
cat("  1. LaTeX table: ", output_file, "\n", sep = "")
cat("  2. CSV reference: ", csv_file, "\n\n", sep = "")

cat("To include in manuscript appendix, add:\n")
cat("  \\input{../data/Tex/Tables/tbl_final_varset}\n\n")

################################################################################
# END OF SCRIPT
################################################################################
