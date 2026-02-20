# ==============================================================================
# MiD Data Aggregation Functions (Refactored)
# ==============================================================================
# Purpose: Aggregate MiD survey data to district level (ags5) or
#          district × mode level (ags5 × hvm)
# Author: Michael Mühlbauer (refactored from Variable_Selection_Functions.R)
# Date: 2025-12-05
#
# These functions work with VarCoding Excel files that specify:
# - VarCode: 0 = skip, 1 = mean, 2 = proportion
# - FilterVar: Error codes to exclude (comma-separated)
# - PseudoMetric: Whether variable is a pseudometric (yes/no)
#
# Key improvements from original:
# - Support for mode-specific aggregation (ags5 × hvm domains)
# - Better documentation and parameter validation
# - Cleaner handling of missing domains
# - More efficient data structures
# ==============================================================================

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, srvyr)

# ==============================================================================
# FUNCTION: prop_fun
# ==============================================================================
# Calculate weighted proportions for categorical variables by domain
#
# Parameters:
#   data: Survey design object (from srvyr::as_survey_design)
#   ags: Domain identifier column name (e.g., "ags5" or "ags5_hvm")
#   groupvar: Categorical variable name to calculate proportions for
#   FilterVar: Error codes to exclude (comma-separated string or "none")
#
# Returns:
#   Data frame with one row per domain, columns for each category proportion
#   Column names: {groupvar}_prop_{category}
#
# Example:
#   prop_fun(trip_design, ags = "ags5", groupvar = "hvm", FilterVar = "703,99")
#   Returns: ags5, hvm_prop_1, hvm_prop_2, hvm_prop_3, hvm_prop_4
# ==============================================================================

prop_fun <- function(data, ags, groupvar, FilterVar) {

  # Get all unique domain values for filling in unsampled domains
  AgsContr <- data$variables %>%
    dplyr::select(tidyselect::all_of(ags)) %>%
    as.vector() %>%
    unlist() %>%
    unique() %>%
    sort()

  # Parse filter codes
  FilterVar <- unique(strsplit(as.character(FilterVar), split = ","))

  # Calculate proportions by domain and category
  # NOTE: Use direct weighted proportions instead of survey_prop() for speed
  # We don't need variance estimates for covariates

  # Extract weight variable name from survey design
  weight_var <- names(data$allprob)[1]  # Get weight column name

  df <- data$variables %>%
    # Apply filter if specified
    {
      if (FilterVar == "none") {
        .
      } else {
        dplyr::filter(., !(.data[[groupvar]] %in% as.numeric(unlist(FilterVar))))
      }
    } %>%
    # Group by domain and category, calculate weighted sums
    group_by(.data[[ags]], .data[[groupvar]]) %>%
    summarize(weight_sum = sum(.data[[weight_var]], na.rm = TRUE), .groups = "drop_last") %>%
    # Calculate proportions within each domain
    mutate(prop = weight_sum / sum(weight_sum)) %>%
    ungroup() %>%
    select(-weight_sum) %>%
    # Pivot categories to columns
    tidyr::pivot_wider(
      names_from = all_of(groupvar),
      values_from = prop,
      values_fill = 0
    ) %>%
    # Rename columns with prefix
    rename_with(
      .fn = function(x) {
        paste0(groupvar, "_", "prop", "_", x)
      },
      .cols = 2:last_col()
    )

  # Identify unsampled domains
  NoObs <- which(!AgsContr %in% df[[ags]])
  ags_NoObs <- AgsContr[NoObs]

  # Create rows for unsampled domains (filled with 0)
  if (length(NoObs) > 0) {
    df_addon <- matrix(nrow = length(NoObs), ncol = length(df)) %>%
      as.data.frame() %>%
      setNames(nm = names(df)) %>%
      mutate(!!ags := .env[["ags_NoObs"]]) %>%
      mutate(across(2:length(df), ~ 0))

    df <- rbind(df, df_addon) %>% arrange(.by_group = TRUE)
  }

  # Reorder columns alphabetically (excluding domain ID)
  Agg <- df[, c(1, 1 + order(names(df)[-1]))]

  return(Agg)
}

# ==============================================================================
# FUNCTION: mean_fun
# ==============================================================================
# Calculate weighted means for continuous variables by domain
#
# Parameters:
#   data: Survey design object (from srvyr::as_survey_design)
#   ags: Domain identifier column name (e.g., "ags5" or "ags5_hvm")
#   var: Continuous variable name to calculate mean for
#   FilterVar: Error codes to exclude (comma-separated string or "none")
#
# Returns:
#   Data frame with one row per domain, one column for mean
#   Column name: {var}_mean
#
# Example:
#   mean_fun(trip_design, ags = "ags5", var = "wegkm_imp", FilterVar = "70701")
#   Returns: ags5, wegkm_imp_mean
# ==============================================================================

mean_fun <- function(data, ags, var, FilterVar) {

  # Get all unique domain values
  AgsContr <- data$variables %>%
    select(tidyselect::all_of(ags)) %>%
    as.vector() %>%
    unlist() %>%
    unique() %>%
    sort()

  # Parse filter codes
  FilterVar <- unique(strsplit(as.character(FilterVar), split = ","))

  # Calculate means by domain
  # NOTE: Use direct weighted mean instead of survey_mean() for speed
  # We don't need variance estimates for covariates

  # Extract weight variable name from survey design
  weight_var <- names(data$allprob)[1]  # Get weight column name

  df <- data$variables %>%
    # Apply filter if specified
    {
      if (FilterVar == "none") {
        .
      } else {
        filter(., !(.data[[var]] %in% as.numeric(unlist(FilterVar))))
      }
    } %>%
    # Group by domain
    group_by(.data[[ags]]) %>%
    # Calculate weighted mean directly (much faster than survey_mean)
    summarize(
      mean = weighted.mean(.data[[var]], w = .data[[weight_var]], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    # Rename column
    rename_with(
      .fn = function(x) {
        paste0(var, "_", x)
      },
      .cols = 2:last_col()
    )

  # Identify unsampled domains
  NoObs <- which(!AgsContr %in% df[[ags]])
  ags_NoObs <- AgsContr[NoObs]

  # Create rows for unsampled domains (filled with 0)
  if (length(NoObs) > 0) {
    df_addon <- matrix(nrow = length(NoObs), ncol = length(df)) %>%
      as.data.frame() %>%
      setNames(nm = names(df)) %>%
      mutate(!!ags := .env[["ags_NoObs"]]) %>%
      mutate(across(2:length(df), ~ 0))

    df <- rbind(df, df_addon) %>% arrange(.by_group = TRUE)
  }

  return(df)
}

# ==============================================================================
# FUNCTION: aggregator_fun
# ==============================================================================
# Master aggregation function that processes multiple variables according to
# a VarCoding specification
#
# Parameters:
#   data: Survey design object (from srvyr::as_survey_design)
#   VarCoding: Data frame with columns:
#              - Variable: Variable name
#              - VarCode: 0 = skip, 1 = mean, 2 = proportion
#              - FilterVar: Error codes to exclude
#   ags: Domain identifier column name (e.g., "ags5" or "ags5_hvm")
#
# Returns:
#   Data frame with one row per domain, columns for all aggregated variables
#
# Example:
#   VarCoding <- readxl::read_xlsx("VarCoding_MiD.xlsx")
#   aggregator_fun(trip_design, VarCoding, ags = "ags5_hvm")
# ==============================================================================

aggregator_fun <- function(data, VarCoding, ags) {

  # Initialize output with domain IDs only
  df_ag <- data %>%
    group_by(.data[[ags]]) %>%
    summarise(.groups = "drop")

  counter <- 0

  # Process each variable according to VarCode
  for (i in VarCoding$VarCode) {
    counter <- counter + 1

    # Get variable name from VarCoding (not from column index!)
    var_name <- VarCoding$Variable[counter]

    if (i == 1) {
      # Calculate mean
      df_ag <- full_join(
        df_ag,
        mean_fun(
          data,
          ags = ags,
          var = var_name,
          FilterVar = VarCoding[counter, "FilterVar"]
        ),
        by = ags
      )
    } else if (i == 2) {
      # Calculate proportions
      df_ag <- full_join(
        df_ag,
        prop_fun(
          data,
          ags = ags,
          groupvar = var_name,
          FilterVar = VarCoding[counter, "FilterVar"]
        ),
        by = ags
      )
    }
    # VarCode == 0: skip variable

    # Progress indicator
    message("Processed variable ", counter, "/", nrow(VarCoding), ": ", var_name)
  }

  return(df_ag)
}

# ==============================================================================
# FUNCTION: create_mode_specific_survey_design
# ==============================================================================
# Helper function to create survey design for mode-specific aggregation
#
# Parameters:
#   df: Data frame with trip-level data
#   mode_col: Column name containing mode indicator (e.g., "hvm_label")
#   weights_col: Column name containing survey weights (e.g., "W_HOCH")
#   id_col: Column name for cluster ID (e.g., "H_ID_Reg")
#   strata_cols: Vector of column names for stratification
#
# Returns:
#   Survey design object
# ==============================================================================

create_mode_specific_survey_design <- function(df,
                                                mode_col = "hvm_label",
                                                weights_col = "W_HOCH",
                                                id_col = "H_ID_Reg",
                                                strata_cols = c("BLAND", "mid_gemeindetyp_code")) {

  # Create combined strata variable
  df <- df %>%
    mutate(strata_combined = interaction(!!!syms(strata_cols), sep = "_"))

  # Create survey design
  design <- df %>%
    srvyr::as_survey_design(
      ids = !!sym(id_col),
      strata = strata_combined,
      weights = !!sym(weights_col),
      nest = TRUE
    )

  return(design)
}

# ==============================================================================
# FUNCTION: aggregate_by_mode
# ==============================================================================
# Convenience function to aggregate trip data to ags5 × mode level
#
# Parameters:
#   df_trips: Data frame with trip-level data
#   VarCoding: VarCoding specification data frame
#   mode_col: Column name for mode (default: "hvm_label")
#   ags_col: Column name for district (default: "ags5")
#
# Returns:
#   Data frame with ags5 × mode level aggregates
# ==============================================================================

aggregate_by_mode <- function(df_trips,
                               VarCoding,
                               mode_col = "hvm_label",
                               ags_col = "ags5") {

  # Create ags5_hvm domain identifier
  df_trips <- df_trips %>%
    mutate(ags5_hvm = paste(!!sym(ags_col), !!sym(mode_col), sep = "_"))

  # Create survey design
  trip_design <- create_mode_specific_survey_design(df_trips)

  # Aggregate using ags5_hvm as domain
  df_aggregated <- aggregator_fun(
    data = trip_design,
    VarCoding = VarCoding,
    ags = "ags5_hvm"
  )

  # Split ags5_hvm back into separate columns
  df_aggregated <- df_aggregated %>%
    separate(ags5_hvm, into = c("ags5", "hvm_label"), sep = "_", remove = FALSE)

  return(df_aggregated)
}

# ==============================================================================
# VARIANCE ESTIMATION FUNCTIONS
# ==============================================================================
# These functions compute survey-estimated variances for MiD covariates,
# which are needed for the Ybarra-Lohr (2008) measurement error check.
#
# The measurement error condition is: β'C_i β > σ²_v + ψ_i
# where C_i is the covariance matrix of estimated covariates X̂_i
#
# For computational tractability, we assume C_i is diagonal (independent covariates)
# so we only need Var(X̂_ij) for each covariate j in each domain i.
# ==============================================================================

# ==============================================================================
# FUNCTION: mean_fun_with_variance
# ==============================================================================
# Calculate weighted means AND variances for continuous variables by domain
# Uses srvyr::survey_mean() for proper variance estimation
#
# Parameters:
#   data: Survey design object (from srvyr::as_survey_design)
#   ags: Domain identifier column name (e.g., "ags5" or "ags5_hvm")
#   var: Continuous variable name to calculate mean for
#   FilterVar: Error codes to exclude (comma-separated string or "none")
#
# Returns:
#   Data frame with columns: {ags}, {var}_mean, {var}_var
# ==============================================================================

mean_fun_with_variance <- function(data, ags, var, FilterVar) {

  # Get all unique domain values
  AgsContr <- data$variables %>%
    select(tidyselect::all_of(ags)) %>%
    as.vector() %>%
    unlist() %>%
    unique() %>%
    sort()

  # Parse filter codes
  FilterVar <- unique(strsplit(as.character(FilterVar), split = ","))

  # Apply filter if specified
  if (FilterVar != "none") {
    data <- data %>%
      filter(!(.data[[var]] %in% as.numeric(unlist(FilterVar))))
  }

  # Calculate mean and variance by domain using survey methods
  df <- data %>%
    group_by(.data[[ags]]) %>%
    summarize(
      mean_val = srvyr::survey_mean(.data[[var]], na.rm = TRUE, vartype = "var"),
      .groups = "drop"
    )

  # Rename columns - survey_mean returns mean_val and mean_val_var
  names(df) <- c(ags, paste0(var, "_mean"), paste0(var, "_var"))

  # Identify unsampled domains
  NoObs <- which(!AgsContr %in% df[[ags]])
  ags_NoObs <- AgsContr[NoObs]

  # Create rows for unsampled domains (filled with NA for variance)
  if (length(NoObs) > 0) {
    df_addon <- data.frame(
      ags_col = ags_NoObs,
      mean_col = 0,
      var_col = NA_real_
    )
    names(df_addon) <- names(df)
    df <- rbind(df, df_addon) %>% arrange(.data[[ags]])
  }

  return(df)
}

# ==============================================================================
# FUNCTION: prop_fun_with_variance
# ==============================================================================
# Calculate weighted proportions AND variances for categorical variables by domain
# Uses srvyr::survey_prop() for proper variance estimation
#
# Parameters:
#   data: Survey design object (from srvyr::as_survey_design)
#   ags: Domain identifier column name (e.g., "ags5" or "ags5_hvm")
#   groupvar: Categorical variable name to calculate proportions for
#   FilterVar: Error codes to exclude (comma-separated string or "none")
#
# Returns:
#   List with two data frames:
#   - props: Proportion estimates (same format as prop_fun)
#   - vars: Variance estimates (same structure)
# ==============================================================================

prop_fun_with_variance <- function(data, ags, groupvar, FilterVar) {

  # Get all unique domain values
  AgsContr <- data$variables %>%
    select(tidyselect::all_of(ags)) %>%
    as.vector() %>%
    unlist() %>%
    unique() %>%
    sort()

  # Parse filter codes
  FilterVar <- unique(strsplit(as.character(FilterVar), split = ","))

  # Apply filter if specified
  if (FilterVar != "none") {
    data <- data %>%
      filter(!(.data[[groupvar]] %in% as.numeric(unlist(FilterVar))))
  }

  # Calculate proportions and variances by domain and category
  df <- data %>%
    group_by(.data[[ags]], .data[[groupvar]]) %>%
    summarize(
      prop_val = srvyr::survey_prop(vartype = "var"),
      .groups = "drop"
    )

  # survey_prop returns prop_val and prop_val_var columns
  # Pivot both to wide format
  df_props <- df %>%
    select(all_of(ags), all_of(groupvar), prop_val) %>%
    pivot_wider(
      names_from = all_of(groupvar),
      values_from = prop_val,
      values_fill = 0
    ) %>%
    rename_with(
      .fn = function(x) paste0(groupvar, "_prop_", x),
      .cols = 2:last_col()
    )

  df_vars <- df %>%
    select(all_of(ags), all_of(groupvar), prop_val_var) %>%
    pivot_wider(
      names_from = all_of(groupvar),
      values_from = prop_val_var,
      values_fill = NA_real_
    ) %>%
    rename_with(
      .fn = function(x) paste0(groupvar, "_prop_", x, "_var"),
      .cols = 2:last_col()
    )

  # Handle unsampled domains
  NoObs <- which(!AgsContr %in% df_props[[ags]])
  ags_NoObs <- AgsContr[NoObs]

  if (length(NoObs) > 0) {
    # Add rows for unsampled domains
    df_props_addon <- matrix(0, nrow = length(NoObs), ncol = ncol(df_props)) %>%
      as.data.frame() %>%
      setNames(names(df_props))
    df_props_addon[[ags]] <- ags_NoObs
    df_props <- rbind(df_props, df_props_addon) %>% arrange(.data[[ags]])

    df_vars_addon <- matrix(NA_real_, nrow = length(NoObs), ncol = ncol(df_vars)) %>%
      as.data.frame() %>%
      setNames(names(df_vars))
    df_vars_addon[[ags]] <- ags_NoObs
    df_vars <- rbind(df_vars, df_vars_addon) %>% arrange(.data[[ags]])
  }

  # Reorder columns alphabetically (excluding domain ID)
  df_props <- df_props[, c(1, 1 + order(names(df_props)[-1]))]
  df_vars <- df_vars[, c(1, 1 + order(names(df_vars)[-1]))]

  return(list(props = df_props, vars = df_vars))
}

# ==============================================================================
# FUNCTION: aggregator_fun_with_variance
# ==============================================================================
# Master aggregation function that computes BOTH estimates and variances
#
# Parameters:
#   data: Survey design object (from srvyr::as_survey_design)
#   VarCoding: Data frame with VarCode specification
#   ags: Domain identifier column name
#
# Returns:
#   List with two data frames:
#   - estimates: Point estimates (same as aggregator_fun output)
#   - variances: Variance estimates for each covariate
# ==============================================================================

aggregator_fun_with_variance <- function(data, VarCoding, ags) {

  # Initialize outputs with domain IDs only
  df_estimates <- data %>%
    group_by(.data[[ags]]) %>%
    summarise(.groups = "drop")

  df_variances <- df_estimates

  counter <- 0

  # Process each variable according to VarCode
  for (i in VarCoding$VarCode) {
    counter <- counter + 1
    var_name <- VarCoding$Variable[counter]

    if (i == 1) {
      # Calculate mean with variance
      result <- mean_fun_with_variance(
        data,
        ags = ags,
        var = var_name,
        FilterVar = VarCoding[counter, "FilterVar"]
      )

      # Split into estimates and variances
      mean_col <- paste0(var_name, "_mean")
      var_col <- paste0(var_name, "_var")

      df_estimates <- full_join(
        df_estimates,
        result %>% select(all_of(c(ags, mean_col))),
        by = ags
      )

      df_variances <- full_join(
        df_variances,
        result %>% select(all_of(c(ags, var_col))),
        by = ags
      )

    } else if (i == 2) {
      # Calculate proportions with variance
      result <- prop_fun_with_variance(
        data,
        ags = ags,
        groupvar = var_name,
        FilterVar = VarCoding[counter, "FilterVar"]
      )

      df_estimates <- full_join(
        df_estimates,
        result$props,
        by = ags
      )

      df_variances <- full_join(
        df_variances,
        result$vars,
        by = ags
      )
    }
    # VarCode == 0: skip variable

    # Progress indicator
    message("Processed variable ", counter, "/", nrow(VarCoding), ": ", var_name,
            " (with variance)")
  }

  return(list(estimates = df_estimates, variances = df_variances))
}

# ==============================================================================
# End of aggregation functions
# ==============================================================================
