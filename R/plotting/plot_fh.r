#### FH Estimation Visualization - Multimodal Analysis ####
# Created: 2025-12-02, Updated: 2026-01-23
# Purpose: Visualize multimodal FH model estimates (ags5 × hvm)
# Data: fh_model_final.rds (from phase3_cross_validation.r)

rm(list = ls())  # Clear workspace
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggpubr, viridis, colorspace, sf, patchwork)

data_path <- "./data"
manuscript_path <- "./manuscript"  # EPS figures go here for LaTeX compilation

# Load auxiliaries
source(paste0(getwd(), "/R/auxiliaries/00_themes.r"))
source(paste0(getwd(), "/R/auxiliaries/01_plot_functions.r"))

#### Load Data ####

# Load FH model
fh_model <- readRDS(file.path(data_path, "output/models/fh_model_final.rds"))

# Extract FH estimates
df_fh <- data.frame(
  ags5_hvm = fh_model$ind$Domain,
  FH = fh_model$ind$FH,
  Direct = fh_model$ind$Direct
)

# Extract MSE and calculate CV
df_mse <- data.frame(
  ags5_hvm = fh_model$MSE$Domain,
  MSE_FH = fh_model$MSE$FH
)

# Merge and calculate CV
df_fh <- df_fh %>%
  left_join(df_mse, by = "ags5_hvm") %>%
  mutate(
    # Parse ags5 and mode from domain ID
    ags5 = sub("_.*", "", ags5_hvm),
    hvm_label = sub(".*_", "", ags5_hvm),
    # Calculate CV
    CV_FH = sqrt(MSE_FH) / FH,
    CV_pct = CV_FH * 100
  )

# Load shapefile for district boundaries
shp_districts <- sf::st_read(
  file.path(data_path, "raw/shapes/vg2500_01-01.gk3.shape/vg2500/VG2500_KRS.shp"),
  quiet = TRUE
)

# Prepare shapefile: extract ags5 and transform to common CRS
shp_districts <- shp_districts %>%
  mutate(ags5 = ARS) %>%
  st_transform(crs = 4326)  # WGS84 for consistent plotting

#### Data Preparation ####

cat("\n=== FH Model Summary by Mode ===\n")
df_fh %>%
  group_by(hvm_label) %>%
  summarise(
    n_domains = n(),
    mean_FH = mean(FH, na.rm = TRUE),
    median_FH = median(FH, na.rm = TRUE),
    mean_CV_pct = mean(CV_pct, na.rm = TRUE),
    median_CV_pct = median(CV_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()

# Create separate datasets for each mode
df_fuss <- df_fh %>% filter(hvm_label == "fuss")
df_fahrrad <- df_fh %>% filter(hvm_label == "fahrrad")
df_miv <- df_fh %>% filter(hvm_label == "miv")
df_oepv <- df_fh %>% filter(hvm_label == "oepv")

# Merge with spatial data
sf_fuss <- shp_districts %>% left_join(df_fuss, by = "ags5")
sf_fahrrad <- shp_districts %>% left_join(df_fahrrad, by = "ags5")
sf_miv <- shp_districts %>% left_join(df_miv, by = "ags5")
sf_oepv <- shp_districts %>% left_join(df_oepv, by = "ags5")

# Visualization parameters loaded from 00_themes.r:
# textwidth, scal_ch, map_2panel_width/height, map_4panel_width/height,
# legend_barwidth, legend_barheight

#### Fig_FH: FH Estimates by Mode (4 Maps) ####

# Helper function to create mode-specific FH map with horizontal bottom legend
create_fh_map <- function(sf_data, mode_name, tag = "") {
  ggplot(data = sf_data) +
    geom_sf(mapping = aes(fill = FH), color = "black", size = 0.1) +
    scale_fill_viridis(
      option = "H",
      begin = 0,
      end = 1,
      na.value = "grey90",
      name = "km/day",
      guide = guide_colorbar(
        frame.colour = "black",
        ticks.colour = "black",
        label.hjust = 0.5,
        barwidth = legend_barwidth,
        barheight = legend_barheight,
        title.position = "top",
        title.hjust = 0.5
      )
    ) +
    labs(
      x = NULL,
      y = NULL,
      title = paste0(tag, " ", mode_name)
    ) +
    coord_sf(datum = NA) +
    Map_theme(
      legend.position = "bottom"
    )
}

# Create FH maps for each mode (with German MiD abbreviations)
map_fh_fuss <- create_fh_map(sf_fuss, "Walking (zu Fuß)", "(a)")
map_fh_fahrrad <- create_fh_map(sf_fahrrad, "Cycling (Fahrrad)", "(b)")
map_fh_miv <- create_fh_map(sf_miv, "Car (MIV)", "(c)")
map_fh_oepv <- create_fh_map(sf_oepv, "Public Transit (ÖPV)", "(d)")

# Combine into 2×2 grid with spacing between panels
design <- "A#B
###
C#D"
fig_fh_multimodal <- map_fh_fuss + map_fh_fahrrad + map_fh_miv + map_fh_oepv +
  plot_layout(design = design, widths = c(1, 0.05, 1), heights = c(1, 0.05, 1))

# Save Fig_FH - 4-panel map
ggsave(
  filename = file.path(manuscript_path, "FigFHMultimodal.eps"),
  plot = fig_fh_multimodal,
  width = map_4panel_width,
  height = map_4panel_height,
  device = cairo_ps,
  units = "cm"
)

cat("\nFigFHMultimodal (FH estimates by mode) saved!\n")

#### FigFHCV: Coefficient of Variation by Mode (4 Maps) ####

# Helper function for FH CV maps
create_fh_cv_map <- function(sf_data, mode_name, tag = "", show_legend = FALSE) {
  p <- ggplot(data = sf_data) +
    geom_sf(aes(fill = CV_pct), color = "black", size = 0.1) +
    scale_fill_continuous_diverging(
      palette = "Blue-Red 3",
      c1 = 130,
      l1 = 2,
      l2 = 100,
      p1 = 0.6,
      p2 = 1.2,
      limits = c(0, 30),  # FH CVs are much lower than direct estimates
      na.value = "grey90",
      mid = 10,
      name = "CV (%)",
      breaks = c(0, 10, 20, 30),
      labels = c("0%", "10%", "20%", "30%"),
      guide = guide_colorbar(
        frame.colour = "black",
        ticks.colour = "black",
        label.hjust = 0.5,
        barwidth = legend_barwidth,
        barheight = legend_barheight,
        title.position = "top",
        title.hjust = 0.5
      )
    ) +
    labs(
      x = NULL,
      y = NULL,
      title = paste0(tag, " CV: ", mode_name)
    ) +
    coord_sf(datum = NA) +
    Map_theme(
      legend.position = if (show_legend) "bottom" else "none"
    )

  return(p)
}

# Create FH CV maps (only last one shows legend) - with German MiD abbreviations
cv_fh_fuss <- create_fh_cv_map(sf_fuss, "Walking (zu Fuß)", "(a)", show_legend = FALSE)
cv_fh_fahrrad <- create_fh_cv_map(sf_fahrrad, "Cycling (Fahrrad)", "(b)", show_legend = FALSE)
cv_fh_miv <- create_fh_cv_map(sf_miv, "Car (MIV)", "(c)", show_legend = FALSE)
cv_fh_oepv <- create_fh_cv_map(sf_oepv, "Public Transit (ÖPV)", "(d)", show_legend = TRUE)

# Combine with shared legend at bottom and spacing between panels
design <- "A#B
###
C#D"
fig_fh_cv_multimodal <- cv_fh_fuss + cv_fh_fahrrad + cv_fh_miv + cv_fh_oepv +
  plot_layout(design = design, widths = c(1, 0.05, 1), heights = c(1, 0.05, 1), guides = "collect") &
  theme(legend.position = "bottom")

# Save FigFHCV - 4-panel map
ggsave(
  filename = file.path(manuscript_path, "FigFHCVMultimodal.eps"),
  plot = fig_fh_cv_multimodal,
  width = map_4panel_width,
  height = map_4panel_height,
  device = cairo_ps,
  units = "cm"
)

cat("FigFHCVMultimodal (FH CV by mode) saved!\n")

#### FigGammaDensity: Shrinkage Factor Distribution by Mode ####

# Extract gamma (shrinkage factor) from model
df_gamma <- data.frame(
  ags5_hvm = fh_model$model$gamma$Domain,
  Gamma = fh_model$model$gamma$Gamma
) %>%
  mutate(
    ags5 = sub("_.*", "", ags5_hvm),
    hvm_label = sub(".*_", "", ags5_hvm)
  )

# Mode colors (consistent with plot_comparison.r - colorblind-friendly palette)
mode_colors <- c("fuss" = "#E69F00", "fahrrad" = "#56B4E9",
                 "miv" = "#009E73", "oepv" = "#CC79A7")

# Mode labels (English with German abbreviation)
mode_labels <- c("fuss" = "Walking (zu Fuß)",
                 "fahrrad" = "Cycling (Fahrrad)",
                 "miv" = "Car (MIV)",
                 "oepv" = "Public Transit (ÖPV)")

# Create density plot - narrow and full textwidth
fig_gamma_density <- ggplot(df_gamma, aes(x = Gamma, fill = hvm_label, color = hvm_label)) +
  geom_density(alpha = 0.35, linewidth = 0.6) +
  scale_fill_manual(values = mode_colors,
                    labels = mode_labels,
                    name = NULL) +
  scale_color_manual(values = mode_colors,
                     labels = mode_labels,
                     name = NULL) +
  scale_x_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, 0.2),
                     expand = c(0.01, 0)) +
  labs(
    x = "Shrinkage Factor",
    y = "Density"
  ) +
  General_theme() +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.margin = margin(t = -5, b = 0),
    plot.margin = margin(t = 5, r = 10, b = 5, l = 10),
    panel.grid.major.x = element_line(color = "#dbdbd9", linewidth = 0.2)
  ) +
  guides(fill = guide_legend(nrow = 1),
         color = guide_legend(nrow = 1))

# Save figure - narrow height, full textwidth
ggsave(
  filename = file.path(manuscript_path, "FigGammaDensity.eps"),
  plot = fig_gamma_density,
  width = textwidth,
  height = 5,  # Narrow: 5 cm height
  device = cairo_ps,
  units = "cm"
)

cat("FigGammaDensity (shrinkage factor by mode) saved!\n")

#### Summary Statistics ####

cat("\n=== Overall FH Summary ===\n")
cat("Total domains:", nrow(df_fh), "\n")
cat("Districts:", n_distinct(df_fh$ags5), "\n")
cat("Modes:", n_distinct(df_fh$hvm_label), "\n\n")

cat("=== FH Precision Summary (CV > 20%) ===\n")
df_fh %>%
  mutate(low_precision = CV_pct > 20) %>%
  group_by(hvm_label) %>%
  summarise(
    total = n(),
    low_precision_count = sum(low_precision, na.rm = TRUE),
    low_precision_pct = 100 * mean(low_precision, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()

cat("\n=== All FH plots complete! ===\n")
cat("Saved:\n")
cat("  - FigFHMultimodal.eps (4 maps: FH estimates)\n")
cat("  - FigFHCVMultimodal.eps (4 maps: FH CV)\n")
cat("  - FigGammaDensity.eps (density: shrinkage factor by mode)\n")
