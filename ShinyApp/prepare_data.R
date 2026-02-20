#### Prepare Data for Shiny App ####
# Run this script once from the project root to generate
# ShinyApp/mid_shiny_data.RData
#
# Usage: source("ShinyApp/prepare_data.R")  # from project root

rm(list = ls())
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, sf)

data_path <- "./data"

#### 1. Load FH model and extract components ####

fh_model <- readRDS(file.path(data_path, "output/models/fh_model_final.rds"))

# Extract FH estimates (pattern from R/plotting/plot_fh.r:23-27)
df_fh <- data.frame(
  ags5_hvm = fh_model$ind$Domain,
  FH = fh_model$ind$FH,
  Direct_FH = fh_model$ind$Direct
)

# Extract MSE (pattern from R/plotting/plot_fh.r:30-33)
df_mse <- data.frame(
  ags5_hvm = fh_model$MSE$Domain,
  MSE_FH = fh_model$MSE$FH
)

# Extract gamma (pattern from R/plotting/generate_fh_results_table.r:37-40)
df_gamma <- data.frame(
  ags5_hvm = fh_model$model$gamma$Domain,
  Gamma = fh_model$model$gamma$Gamma
)

# Merge FH components
df_fh <- df_fh %>%
  left_join(df_mse, by = "ags5_hvm") %>%
  left_join(df_gamma, by = "ags5_hvm") %>%
  mutate(
    ags5 = sub("_.*", "", ags5_hvm),
    hvm_label = sub(".*_", "", ags5_hvm),
    CV_FH = sqrt(MSE_FH) / FH,
    CV_FH_pct = CV_FH * 100
  )

rm(fh_model, df_mse, df_gamma)

#### 2. Load direct estimates ####

df_direct <- readRDS(file.path(data_path, "output/direct/df_HTH_perskm_analytical.rds"))

#### 3. Merge into combined comparison dataframe ####
# (pattern from R/plotting/generate_fh_results_table.r:53-60)

df_combined <- df_direct %>%
  select(ags5_hvm, ags5, hvm_label, perskm_hth, var_hth, se_hth) %>%
  left_join(
    df_fh %>% select(ags5_hvm, FH, MSE_FH, CV_FH, CV_FH_pct, Gamma),
    by = "ags5_hvm"
  ) %>%
  rename(
    Direct = perskm_hth,
    Direct_Var = var_hth,
    Direct_SE = se_hth
  ) %>%
  mutate(
    CV_HTH = sqrt(Direct_Var) / Direct,
    CV_HTH_pct = CV_HTH * 100,
    RV = (CV_HTH - CV_FH) / CV_HTH
  )

rm(df_direct, df_fh)

cat("Combined data:", nrow(df_combined), "domains,",
    n_distinct(df_combined$ags5), "districts\n")

#### 4. Load and prepare shapefile ####
# (pattern from R/plotting/plot_fh.r:48-56)

shp_districts <- sf::st_read(
  file.path(data_path, "raw/shapes/vg2500_01-01.gk3.shape/vg2500/VG2500_KRS.shp"),
  quiet = TRUE
) %>%
  mutate(ags5 = ARS) %>%
  st_transform(crs = 4326)

#### 5. Extract district names for dropdown ####

district_lookup <- shp_districts %>%
  st_drop_geometry() %>%
  transmute(
    ags5 = ags5,
    district_name = GEN,
    district_type = BEZ
  ) %>%
  distinct()

# Named vector: "Berlin (11000)" -> "11000"
district_choices <- setNames(
  district_lookup$ags5,
  paste0(district_lookup$district_name, " (", district_lookup$ags5, ")")
)
district_choices <- district_choices[order(names(district_choices))]

#### 6. Pre-merge spatial data with results ####

# Keep only geometry and ags5 from shapefile
shp_slim <- shp_districts %>%
  select(ags5, geometry)

sf_data <- shp_slim %>%
  left_join(df_combined, by = "ags5") %>%
  left_join(district_lookup %>% select(ags5, district_name), by = "ags5")

#### 7. Constants ####

# Mode colors (from R/plotting/plot_comparison.r:80-81)
mode_colors <- c(
  "fuss" = "#E69F00", "fahrrad" = "#56B4E9",
  "miv" = "#009E73", "oepv" = "#CC79A7"
)

# Mode labels
mode_labels <- c(
  "fuss" = "Walking (zu Fu\u00df)",
  "fahrrad" = "Cycling (Fahrrad)",
  "miv" = "Car (MIV)",
  "oepv" = "Public Transit (\u00d6PV)"
)

# Model vs Direct colors (from R/plotting/plot_comparison.r:74-75)
modelcol <- "#276419"   # paletteer_c("grDevices::PiYG", n=10)[9]
directcol <- "#8E0152"  # paletteer_c("grDevices::PiYG", n=10)[2]

#### 8. Save ####

save(
  sf_data,
  df_combined,
  shp_slim,
  district_lookup,
  district_choices,
  mode_colors,
  mode_labels,
  modelcol,
  directcol,
  file = "ShinyApp/mid_shiny_data.RData"
)

cat("Saved ShinyApp/mid_shiny_data.RData\n")
cat("Size:", round(file.size("ShinyApp/mid_shiny_data.RData") / 1024, 0), "KB\n")
