# ==============================================================================
# Census Data Processing Functions
# ==============================================================================
# Purpose: Helper functions for processing German census data from Destatis
# Date: 2025-11-24
# ==============================================================================

# Clean German text for column names ------------------------------------------
# Converts umlauts to ASCII equivalents: ä→ae, ö→oe, ü→ue, ß→ss
clean_german_names <- function(text) {
  text <- gsub("ä", "ae", text, ignore.case = FALSE)
  text <- gsub("Ä", "Ae", text, ignore.case = FALSE)
  text <- gsub("ö", "oe", text, ignore.case = FALSE)
  text <- gsub("Ö", "Oe", text, ignore.case = FALSE)
  text <- gsub("ü", "ue", text, ignore.case = FALSE)
  text <- gsub("Ü", "Ue", text, ignore.case = FALSE)
  text <- gsub("ß", "ss", text, ignore.case = FALSE)
  # Then remove any remaining non-alphanumeric characters
  text <- gsub("[^A-Za-z0-9]", "_", text)
  # Convert to lowercase
  text <- tolower(text)
  return(text)
}

# Extract 2017 data from tables with multiple years ---------------------------
extract_year_2017 <- function(data) {

  # Check if "time" column exists (standard in restatis output)
  if ("time" %in% names(data)) {
    data <- data %>%
      filter(grepl("2017", time))
  }

  # Fallback: Check if "year" or "jahr" column exists
  else if (any(grepl("^(year|jahr)$", names(data), ignore.case = TRUE))) {
    year_col <- names(data)[grepl("^(year|jahr)$", names(data), ignore.case = TRUE)][1]
    data <- data %>%
      filter(.data[[year_col]] == 2017)
  }

  # Fallback: Check if "stichtag", "datum", or "date" column exists
  else if (any(grepl("stichtag|datum|date", names(data), ignore.case = TRUE))) {
    date_col <- names(data)[grepl("stichtag|datum|date", names(data), ignore.case = TRUE)][1]
    data <- data %>%
      filter(grepl("2017", .data[[date_col]]))
  }

  return(data)
}

# Aggregate Gemeinden (municipality) data to Kreise (district) level ----------
aggregate_gemeinden_to_kreise <- function(gemeinden_data) {

  # Gemeinden AGS codes: first 5 digits = Kreis code
  gemeinden_data %>%
    mutate(ags5 = substr(ags, 1, 5)) %>%
    group_by(ags5) %>%
    summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>%
    ungroup()
}

# ==============================================================================
# Merge all census tables into single district-level dataset
# ==============================================================================

merge_all_census_tables <- function(table_list) {

  # Remove NULL entries (failed downloads)
  table_list <- table_list[!sapply(table_list, is.null)]

  if (length(table_list) == 0) {
    stop("No tables were successfully downloaded")
  }

  # Start with base population table
  base_code <- "12411-01-01-4"

  if (!base_code %in% names(table_list)) {
    stop("Base population table ", base_code, " not found")
  }

  base <- table_list[[base_code]] %>%
    extract_year_2017()

  # Find AGS column (restatis uses 1_variable_attribute_code for district codes)
  ags_col <- NULL
  if ("1_variable_attribute_code" %in% names(base)) {
    ags_col <- "1_variable_attribute_code"
  } else if (any(grepl("^(ags|AGS|rs)", names(base), ignore.case = TRUE))) {
    ags_col <- names(base)[grepl("^(ags|AGS|rs)", names(base), ignore.case = TRUE)][1]
  }

  if (is.null(ags_col)) {
    stop("Could not find AGS column in base table")
  }

  # Create base with ags5 and value column
  # Filter to keep only 5-digit codes (Kreise level)
  # Pivot wider to create one column per disaggregation category
  base <- base %>%
    rename(ags5 = !!ags_col) %>%
    mutate(
      ags5 = as.character(ags5),
      value = as.numeric(value)
    ) %>%
    filter(nchar(ags5) == 5)

  # Get statistic name for column naming
  stat_name <- if ("value_variable_label" %in% names(base)) {
    clean_german_names(base$value_variable_label[1])
  } else if ("statistics_label" %in% names(base)) {
    clean_german_names(base$statistics_label[1])
  } else {
    "pop"
  }

  # Find all disaggregation variables (2_variable, 3_variable, etc.)
  # Exclude 1_variable_attribute_label because that's the district dimension (rows)
  disagg_vars <- grep("^[2-9]_variable_attribute_label$", names(base), value = TRUE)

  if (length(disagg_vars) > 0) {
    # Check if there's actual variation in any disaggregation variable
    has_variation <- any(sapply(disagg_vars, function(v) {
      length(unique(base[[v]])) > 1
    }))

    if (has_variation) {
      # Identify which disaggregation variables have variation
      vars_with_variation <- disagg_vars[sapply(disagg_vars, function(v) {
        length(unique(base[[v]])) > 1
      })]

      # Create combined column names from all disaggregation variables with variation
      if (length(vars_with_variation) > 0) {
        # Build var_name by pasting stat_name with all varying disaggregation labels
        # Extract the columns we need before mutate
        disagg_cols <- base[, vars_with_variation, drop = FALSE]

        base <- base %>%
          mutate(
            var_name = do.call(
              paste,
              c(
                list(stat_name),
                lapply(seq_along(vars_with_variation), function(i) {
                  clean_german_names(disagg_cols[[i]])
                }),
                sep = "_"
              )
            )
          ) %>%
          select(ags5, var_name, value) %>%
          pivot_wider(names_from = var_name, values_from = value, values_fn = sum)
      } else {
        # Shouldn't happen but fallback to aggregation
        warning("BASE TABLE: Disaggregation variables found but none have variation - aggregating '", stat_name, "'")
        base <- base %>%
          group_by(ags5) %>%
          summarise(!!stat_name := sum(value, na.rm = TRUE), .groups = "drop")
      }
    } else {
      # No variation in disaggregation variables, just aggregate
      message("BASE TABLE: No variation in disaggregation variables for '", stat_name, "' - aggregating to single column")
      base <- base %>%
        group_by(ags5) %>%
        summarise(!!stat_name := sum(value, na.rm = TRUE), .groups = "drop")
    }
  } else {
    # No disaggregation variables at all, just aggregate
    message("BASE TABLE: No disaggregation variables found for '", stat_name, "' - aggregating to single column")
    base <- base %>%
      group_by(ags5) %>%
      summarise(!!stat_name := sum(value, na.rm = TRUE), .groups = "drop")
  }

  # Iteratively left_join all other tables
  for (table_code in names(table_list)) {

    if (table_code != base_code && !is.null(table_list[[table_code]])) {

      tryCatch({
        #if(table_code == "12211-Z-10"){browser()}

        # Extract 2017 data for this table
        table_data <- table_list[[table_code]] %>%
          extract_year_2017()

        # Diagnostic: Check for unexpected NA values or data quality issues
        value_col_exists <- "value" %in% names(table_data)
        if (value_col_exists) {
          n_na <- sum(is.na(as.numeric(table_data$value)))
          if (n_na > 0) {
            message("Table ", table_code, ": ", n_na, " NA values in 'value' column (",
                    round(100 * n_na / nrow(table_data), 1), "%)")
          }
        }

        # Find AGS column (restatis uses 1_variable_attribute_code for district codes)
        ags_col <- NULL
        if ("1_variable_attribute_code" %in% names(table_data)) {
          ags_col <- "1_variable_attribute_code"
        } else if (any(grepl("^(ags|AGS|rs)", names(table_data), ignore.case = TRUE))) {
          ags_col <- names(table_data)[grepl("^(ags|AGS|rs)", names(table_data), ignore.case = TRUE)][1]
        }

        if (!is.null(ags_col)) {

          # Check if 1_variable_attribute_code contains district-level data
          # District codes (Kreise) are 5 digits, state codes (Bundesländer) are 2 digits
          ags_sample <- table_data[[ags_col]][!is.na(table_data[[ags_col]])]
          ags_sample <- as.character(ags_sample)

          # Count how many codes are 5 digits vs other lengths
          code_lengths <- nchar(ags_sample)
          n_five_digit <- sum(code_lengths == 5, na.rm = TRUE)
          n_total <- length(code_lengths)
          pct_five_digit <- n_five_digit / n_total

          # Skip table if less than 80% of codes are 5 digits
          # (allows for some state codes mixed in, but catches purely state-level tables)
          if (pct_five_digit < 0.8) {
            warning("Table ", table_code, ": Skipping - data is not at district (Kreise) level. ",
                    "Only ", round(pct_five_digit * 100, 1), "% of codes are 5-digit district codes. ",
                    "Most likely at state (Bundesländer) or other aggregation level.")
            next  # Skip to next table
          }

          # Rename AGS column and keep only 5-digit codes (Kreise level)
          table_data <- table_data %>%
            rename(ags5 = !!ags_col) %>%
            mutate(
              ags5 = as.character(ags5),
              value = as.numeric(value)
            ) %>%
            filter(nchar(ags5) == 5)

          # Final check: if filtering resulted in no rows, skip table
          if (nrow(table_data) == 0) {
            warning("Table ", table_code, ": Skipping - no 5-digit district codes found after filtering")
            next
          }

          # Get statistic name for column naming
          stat_name <- if ("value_variable_label" %in% names(table_data)) {
            clean_german_names(table_data$value_variable_label[1])
          } else if ("statistics_label" %in% names(table_data)) {
            clean_german_names(table_data$statistics_label[1])
          } else {
            gsub("-", "_", table_code)  # Fallback to table code
          }

          # Find all disaggregation variables (2_variable, 3_variable, etc.)
          # Exclude 1_variable_attribute_label because that's the district dimension (rows)
          disagg_vars <- grep("^[2-9]_variable_attribute_label$", names(table_data), value = TRUE)

          if (length(disagg_vars) > 0) {
            # Check if there's actual variation in any disaggregation variable
            has_variation <- any(sapply(disagg_vars, function(v) {
              length(unique(table_data[[v]])) > 1
            }))

            if (has_variation) {
              # Identify which disaggregation variables have variation
              vars_with_variation <- disagg_vars[sapply(disagg_vars, function(v) {
                length(unique(table_data[[v]])) > 1
              })]

              # Create combined column names from all disaggregation variables with variation
              if (length(vars_with_variation) > 0) {
                # Extract the columns we need before mutate
                disagg_cols <- table_data[, vars_with_variation, drop = FALSE]

                # Build var_name by pasting stat_name with all varying disaggregation labels
                table_data <- table_data %>%
                  mutate(
                    var_name = do.call(
                      paste,
                      c(
                        list(stat_name),
                        lapply(seq_along(vars_with_variation), function(i) {
                          clean_german_names(disagg_cols[[i]])
                        }),
                        sep = "_"
                      )
                    )
                  ) %>%
                  select(ags5, var_name, value) %>%
                  pivot_wider(names_from = var_name, values_from = value, values_fn = sum)
              } else {
                # Shouldn't happen but fallback to aggregation
                warning("Table ", table_code, ": Disaggregation variables found but none have variation - aggregating '", stat_name, "'")
                table_data <- table_data %>%
                  group_by(ags5) %>%
                  summarise(!!stat_name := sum(value, na.rm = TRUE), .groups = "drop")
              }
            } else {
              # No variation in disaggregation variables, just aggregate
              message("Table ", table_code, ": No variation in disaggregation variables for '", stat_name, "' - aggregating to single column")
              table_data <- table_data %>%
                group_by(ags5) %>%
                summarise(!!stat_name := sum(value, na.rm = TRUE), .groups = "drop")
            }
          } else {
            # No disaggregation variables at all, just aggregate
            message("Table ", table_code, ": No disaggregation variables found for '", stat_name, "' - aggregating to single column")
            table_data <- table_data %>%
              group_by(ags5) %>%
              summarise(!!stat_name := sum(value, na.rm = TRUE), .groups = "drop")
          }

          base <- base %>%
            left_join(table_data, by = "ags5")
        }

      }, error = function(e) {
        warning("Failed to merge table ", table_code, ": ", e$message)
      })
    }
  }

  return(base)
}

# ==============================================================================
# END OF SCRIPT
# ==============================================================================
