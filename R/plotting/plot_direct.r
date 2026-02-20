#### Direct Estimation Visualization - Multimodal Analysis ####
# Created: 2025-12-02
# Purpose: Visualize multimodal direct estimates (ags5 × hvm)
# Data: df_HTH_perskm_analytical.rds (from direct_estimation.r)

rm(list = ls())  # Clear workspace
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggpubr, viridis, colorspace, sf, patchwork)

data_path <- "./data"
manuscript_path <- "./manuscript"  # EPS figures go here for LaTeX compilation
output_dir <- file.path(data_path, "data_cleaning")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Load auxiliaries
source(paste0(getwd(), "/R/auxiliaries/00_themes.r"))
source(paste0(getwd(), "/R/auxiliaries/01_plot_functions.r"))

#### Load Data ####

# Load direct estimates with analytical variance
df_direct <- readRDS(file.path(data_path, "output/direct/df_HTH_perskm_analytical.rds"))

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

# Calculate CV for each domain
df_direct <- df_direct %>%
  mutate(
    cv_hth = se_hth / perskm_hth,
    cv_pct = cv_hth * 100  # CV in percentage
  )

# Summary statistics by mode
cat("\n=== Summary Statistics by Mode ===\n")
df_direct %>%
  group_by(hvm_label) %>%
  summarise(
    n_districts = n(),
    mean_perskm = mean(perskm_hth, na.rm = TRUE),
    median_perskm = median(perskm_hth, na.rm = TRUE),
    mean_cv_pct = mean(cv_pct, na.rm = TRUE),
    median_cv_pct = median(cv_pct, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()

# Create separate datasets for each mode
df_fuss <- df_direct %>% filter(hvm_label == "fuss")
df_fahrrad <- df_direct %>% filter(hvm_label == "fahrrad")
df_miv <- df_direct %>% filter(hvm_label == "miv")
df_oepv <- df_direct %>% filter(hvm_label == "oepv")

# Merge with spatial data
sf_fuss <- shp_districts %>% left_join(df_fuss, by = "ags5")
sf_fahrrad <- shp_districts %>% left_join(df_fahrrad, by = "ags5")
sf_miv <- shp_districts %>% left_join(df_miv, by = "ags5")
sf_oepv <- shp_districts %>% left_join(df_oepv, by = "ags5")

# Visualization parameters loaded from 00_themes.r:
# textwidth, scal_ch, map_2panel_width/height, map_4panel_width/height,
# legend_barwidth, legend_barheight

#### Main Figure: Public Transit Direct Estimates + CV (2-panel) ####
# This is the main figure for the results section - illustrates the problem
# Full 4-mode figures are saved for the appendix

# Helper function to create mode-specific map with horizontal bottom legend
# Legend title positioned on top (underneath the map, above the colorbar)
create_mode_map <- function(sf_data, mode_name, tag = "", fill_var = "perskm_hth",
                            fill_label = "km/day", fill_option = "H") {
  ggplot(data = sf_data) +
    geom_sf(mapping = aes(fill = .data[[fill_var]]), color = "black", size = 0.1) +
    scale_fill_viridis(
      option = fill_option,
      begin = 0,
      end = 1,
      na.value = "grey90",
      name = fill_label,
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

# Helper function for CV maps (diverging colorscale)
# Legend title positioned on top (underneath the map, above the colorbar)
create_cv_map_single <- function(sf_data, mode_name, tag = "") {
  ggplot(data = sf_data) +
    geom_sf(aes(fill = cv_pct), color = "black", size = 0.1) +
    scale_fill_continuous_diverging(
      palette = "Blue-Red 3",
      c1 = 130,
      l1 = 2,
      l2 = 100,
      p1 = 0.6,
      p2 = 1.2,
      limits = c(0, 100),
      na.value = "grey90",
      mid = 20,
      name = "CV (%)",
      breaks = c(0, 20, 40, 60, 80, 100),
      labels = c("0%", "20%", "40%", "60%", "80%", "100%"),
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

# Create main figure: Public Transit only (estimates + CV side by side)
map_oepv_estimate <- create_mode_map(sf_oepv, "Direct Estimate", "(a)")
map_oepv_cv <- create_cv_map_single(sf_oepv, "Coefficient of Variation", "(b)")

fig_direct_illustrative <- map_oepv_estimate | map_oepv_cv

# Save main figure - two maps side by side
ggsave(
  filename = file.path(manuscript_path, "FigDirect.eps"),
  plot = fig_direct_illustrative,
  width = map_2panel_width,
  height = map_2panel_height,
  device = cairo_ps,
  units = "cm"
)

cat("\nFigDirect (Public transit: estimate + CV) saved!\n")

#### Appendix Figures: Full 4-Mode Maps ####

# Create maps for each mode (with German MiD abbreviations)
map_fuss <- create_mode_map(sf_fuss, "Walking (zu Fuß)", "(a)")
map_fahrrad <- create_mode_map(sf_fahrrad, "Cycling (Fahrrad)", "(b)")
map_miv <- create_mode_map(sf_miv, "Car (MIV)", "(c)")
map_oepv_full <- create_mode_map(sf_oepv, "Public Transit (ÖPV)", "(d)")

# Combine into 2×2 grid with spacing between panels
design <- "A#B
###
C#D"
fig1_multimodal <- map_fuss + map_fahrrad + map_miv + map_oepv_full +
  plot_layout(design = design, widths = c(1, 0.05, 1), heights = c(1, 0.05, 1))

# Save Fig1 (Appendix) - 4-panel map
ggsave(
  filename = file.path(manuscript_path, "FigAppDirect.eps"),
  plot = fig1_multimodal,
  width = map_4panel_width,
  height = map_4panel_height,
  device = cairo_ps,
  units = "cm"
)

cat("FigAppDirect (Appendix: 4-mode direct estimates) saved!\n")

#### Appendix: Coefficient of Variation by Mode (4 Maps) ####

# Helper function for CV maps (4-panel version with shared legend)
create_cv_map <- function(sf_data, mode_name, tag = "", show_legend = FALSE) {
  p <- ggplot(data = sf_data) +
    geom_sf(aes(fill = cv_pct), color = "black", size = 0.1) +
    scale_fill_continuous_diverging(
      palette = "Blue-Red 3",
      c1 = 130,
      l1 = 2,
      l2 = 100,
      p1 = 0.6,
      p2 = 1.2,
      limits = c(0, 100),
      na.value = "grey90",
      mid = 20,
      name = "CV (%)",
      breaks = c(0, 20, 40, 60, 80, 100),
      labels = c("0%", "20%", "40%", "60%", "80%", "100%"),
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

# Create CV maps (only last one shows legend) - with German MiD abbreviations
cv_fuss <- create_cv_map(sf_fuss, "Walking (zu Fuß)", "(a)", show_legend = FALSE)
cv_fahrrad <- create_cv_map(sf_fahrrad, "Cycling (Fahrrad)", "(b)", show_legend = FALSE)
cv_miv <- create_cv_map(sf_miv, "Car (MIV)", "(c)", show_legend = FALSE)
cv_oepv <- create_cv_map(sf_oepv, "Public Transit (ÖPV)", "(d)", show_legend = TRUE)

# Combine with shared legend at bottom and spacing between panels
design <- "A#B
###
C#D"
fig2_cv_multimodal <- cv_fuss + cv_fahrrad + cv_miv + cv_oepv +
  plot_layout(design = design, widths = c(1, 0.05, 1), heights = c(1, 0.05, 1), guides = "collect") &
  theme(legend.position = "bottom")

# Save (Appendix) - 4-panel map
ggsave(
  filename = file.path(manuscript_path, "FigAppDirectCV.eps"),
  plot = fig2_cv_multimodal,
  width = map_4panel_width,
  height = map_4panel_height,
  device = cairo_ps,
  units = "cm"
)

cat("FigAppDirectCV (Appendix: 4-mode CV maps) saved!\n")

#### Fig3: Distribution of Estimates by Mode (Violin Plot) ####

fig3_violin <- ggplot(df_direct, aes(x = hvm_label, y = perskm_hth, fill = hvm_label)) +
  geom_violin(alpha = 0.7, draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_jitter(alpha = 0.1, width = 0.2, size = 0.5) +
  scale_fill_viridis_d(option = "D", begin = 0.2, end = 0.8) +
  labs(
    x = "Transport Mode",
    y = "Person-km per day (HTH Direct Estimate)",
    title = "Distribution of Multimodal Direct Estimates",
    fill = "Mode"
  ) +
  General_theme() +
  theme(
    legend.position = "none"
  )

ggsave(
  filename = file.path(manuscript_path, "Fig3_multimodal_violin.eps"),
  plot = fig3_violin,
  width = textwidth * 0.7,
  height = textwidth * 0.6,
  device = cairo_ps,
  units = "cm"
)

cat("Fig3 (Violin plot) saved!\n")

#### Fig4: CV Distribution by Mode (Box Plot) ####

fig4_cv_box <- ggplot(df_direct, aes(x = hvm_label, y = cv_pct, fill = hvm_label)) +
  geom_boxplot(alpha = 0.7, outlier.alpha = 0.3) +
  scale_fill_viridis_d(option = "D", begin = 0.2, end = 0.8) +
  geom_hline(yintercept = 20, linetype = "dashed", color = "red", linewidth = 0.5) +
  annotate("text", x = 0.6, y = 22, label = "20% threshold", color = "red", size = 3) +
  labs(
    x = "Transport Mode",
    y = "Coefficient of Variation (%)",
    title = "Precision of Direct Estimates by Mode",
    fill = "Mode"
  ) +
  General_theme() +
  theme(
    legend.position = "none"
  )

ggsave(
  filename = file.path(manuscript_path, "Fig4_multimodal_cv_box.eps"),
  plot = fig4_cv_box,
  width = textwidth * 0.7,
  height = textwidth * 0.6,
  device = cairo_ps,
  units = "cm"
)

cat("Fig4 (CV boxplot) saved!\n")

#### Summary Statistics ####

cat("\n=== Overall Summary ===\n")
cat("Total domains:", nrow(df_direct), "\n")
cat("Districts:", n_distinct(df_direct$ags5), "\n")
cat("Modes:", n_distinct(df_direct$hvm_label), "\n\n")

cat("=== Precision Summary (CV > 20%) ===\n")
df_direct %>%
  mutate(low_precision = cv_pct > 20) %>%
  group_by(hvm_label) %>%
  summarise(
    total = n(),
    low_precision_count = sum(low_precision),
    low_precision_pct = 100 * mean(low_precision),
    .groups = "drop"
  ) %>%
  print()

cat("\n=== All plots complete! ===\n")
cat("Saved:\n")
cat("Main figures:\n")
cat("  - FigDirect.eps (Public transit: estimate + CV side by side)\n")
cat("Appendix figures:\n")
cat("  - FigAppDirect.eps (4 maps: direct estimates by mode)\n")
cat("  - FigAppDirectCV.eps (4 maps: CV by mode)\n")
cat("  - Fig3_multimodal_violin.eps (violin plot)\n")
cat("  - Fig4_multimodal_cv_box.eps (CV boxplot)\n")

#### Variance Comparison: Analytical vs Bootstrap Methods ####

cat("\n=== Loading Bootstrap Variance Estimates ===\n")

# Load bootstrap variance estimates (emdi direct objects)
emdi_mid_calib <- readRDS(file.path(data_path, "output/direct/df_HTH_perskm_emdi_mid_calib.rds"))
emdi_census_calib <- readRDS(file.path(data_path, "output/direct/df_HTH_perskm_emdi_census_calib.rds"))
emdi_naive <- readRDS(file.path(data_path, "output/direct/df_HTH_perskm_emdi_naive.rds"))

# Extract point estimates and MSE from emdi objects
df_mid_calib <- data.frame(
  Domain = emdi_mid_calib$ind$Domain,
  Mean_mid_calib = emdi_mid_calib$ind$Mean,
  MSE_mid_calib = emdi_mid_calib$MSE$Mean
) %>%
  mutate(
    se_mid_calib = sqrt(MSE_mid_calib),
    cv_mid_calib = (se_mid_calib / Mean_mid_calib) * 100
  ) %>%
  separate(Domain, into = c("ags5", "hvm_label"), sep = "_", remove = FALSE)

df_census_calib <- data.frame(
  Domain = emdi_census_calib$ind$Domain,
  Mean_census_calib = emdi_census_calib$ind$Mean,
  MSE_census_calib = emdi_census_calib$MSE$Mean
) %>%
  mutate(
    se_census_calib = sqrt(MSE_census_calib),
    cv_census_calib = (se_census_calib / Mean_census_calib) * 100
  ) %>%
  separate(Domain, into = c("ags5", "hvm_label"), sep = "_", remove = FALSE)

df_naive <- data.frame(
  Domain = emdi_naive$ind$Domain,
  Mean_naive = emdi_naive$ind$Mean,
  MSE_naive = emdi_naive$MSE$Mean
) %>%
  mutate(
    se_naive = sqrt(MSE_naive),
    cv_naive = (se_naive / Mean_naive) * 100
  ) %>%
  separate(Domain, into = c("ags5", "hvm_label"), sep = "_", remove = FALSE)

# Merge all four variance estimates
df_var_comp <- df_direct %>%
  select(ags5, hvm_label, perskm_hth, se_analytical = se_hth, cv_analytical = cv_pct) %>%
  left_join(
    df_mid_calib %>% select(ags5, hvm_label, se_mid_calib, cv_mid_calib),
    by = c("ags5", "hvm_label")
  ) %>%
  left_join(
    df_census_calib %>% select(ags5, hvm_label, se_census_calib, cv_census_calib),
    by = c("ags5", "hvm_label")
  ) %>%
  left_join(
    df_naive %>% select(ags5, hvm_label, se_naive, cv_naive),
    by = c("ags5", "hvm_label")
  )

cat("Merged variance estimates for", nrow(df_var_comp), "domains\n")

#### FigAppVarianceScatter: 3-Panel Scatter Comparison (Appendix) ####
# Panel (a): Taylor vs MiD-calibrated bootstrap
# Panel (b): Census-calibrated vs MiD-calibrated bootstrap
# Panel (c): Naive vs Census-calibrated bootstrap

# Fit regression models and extract parameters
lm_taylor_mid <- lm(se_mid_calib ~ se_analytical, data = df_var_comp)
lm_census_mid <- lm(se_census_calib ~ se_mid_calib, data = df_var_comp)
lm_naive_census <- lm(se_naive ~ se_census_calib, data = df_var_comp)

# Extract coefficients and R²
coef_taylor_mid <- coef(lm_taylor_mid)
r2_taylor_mid <- summary(lm_taylor_mid)$r.squared

coef_census_mid <- coef(lm_census_mid)
r2_census_mid <- summary(lm_census_mid)$r.squared

coef_naive_census <- coef(lm_naive_census)
r2_naive_census <- summary(lm_naive_census)$r.squared

# Create annotation labels
label_taylor_mid <- sprintf("y = %.3f + %.3fx\nR² = %.4f",
                            coef_taylor_mid[1], coef_taylor_mid[2], r2_taylor_mid)
label_census_mid <- sprintf("y = %.3f + %.3fx\nR² = %.4f",
                            coef_census_mid[1], coef_census_mid[2], r2_census_mid)
label_naive_census <- sprintf("y = %.3f + %.3fx\nR² = %.4f",
                              coef_naive_census[1], coef_naive_census[2], r2_naive_census)

cat("\n=== Regression Parameters ===\n")
cat("Taylor vs MiD-calib:", label_taylor_mid, "\n")
cat("Census vs MiD-calib:", label_census_mid, "\n")
cat("Naive vs Census-calib:", label_naive_census, "\n")

# Get axis ranges for annotation positioning
max_se <- max(c(df_var_comp$se_analytical, df_var_comp$se_mid_calib,
                df_var_comp$se_census_calib, df_var_comp$se_naive), na.rm = TRUE)

# Panel (a): Taylor (analytical) vs MiD-calibrated bootstrap
p_taylor_vs_mid <- ggplot(df_var_comp, aes(x = se_analytical, y = se_mid_calib)) +
  geom_point(alpha = 0.4, size = 1.5, color = "#2166AC") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "magenta", linewidth = 0.8) +
  annotate("text", x = max_se * 0.05, y = max_se * 0.92,
           label = label_taylor_mid, hjust = 0, vjust = 1, size = 3, fontface = "italic") +
  labs(
    title = "(a)",
    x = "Taylor Linearization SE",
    y = "MiD-Calibrated Bootstrap SE"
  ) +
  General_theme() +
  coord_fixed(ratio = 1)

# Panel (b): Census-calibrated vs MiD-calibrated bootstrap
p_census_vs_mid <- ggplot(df_var_comp, aes(x = se_mid_calib, y = se_census_calib)) +
  geom_point(alpha = 0.4, size = 1.5, color = "#4DAF4A") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "magenta", linewidth = 0.8) +
  annotate("text", x = max_se * 0.05, y = max_se * 0.92,
           label = label_census_mid, hjust = 0, vjust = 1, size = 3, fontface = "italic") +
  labs(
    title = "(b)",
    x = "MiD-Calibrated Bootstrap SE",
    y = "Census-Calibrated Bootstrap SE"
  ) +
  General_theme() +
  coord_fixed(ratio = 1)

# Panel (c): Naive vs Census-calibrated bootstrap
p_naive_vs_census <- ggplot(df_var_comp, aes(x = se_census_calib, y = se_naive)) +
  geom_point(alpha = 0.4, size = 1.5, color = "#984EA3") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40", linewidth = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "magenta", linewidth = 0.8) +
  annotate("text", x = max_se * 0.05, y = max_se * 0.92,
           label = label_naive_census, hjust = 0, vjust = 1, size = 3, fontface = "italic") +
  labs(
    title = "(c)",
    x = "Census-Calibrated Bootstrap SE",
    y = "Naive Bootstrap SE"
  ) +
  General_theme() +
  coord_fixed(ratio = 1)

# Combine into 3-panel figure (2 on top, 1 on bottom left, all same size)
# Use design matrix: A B on top row, C # on bottom row (# = empty)
design <- "AB
##
C#"
fig_variance_scatter <- p_taylor_vs_mid + p_census_vs_mid + p_naive_vs_census +
  plot_layout(design = design, heights = c(1, 0.08, 1))

ggsave(
  filename = file.path(manuscript_path, "FigAppVarianceScatter.eps"),
  plot = fig_variance_scatter,
  width = textwidth,
  height = textwidth * 1.15,  # Taller to accommodate spacer row between panels
  device = cairo_ps,
  units = "cm"
)

cat("FigAppVarianceScatter (3-panel variance comparison) saved!\n")

#### Summary Statistics: Variance Comparison ####

cat("\n=== Variance Estimation Method Comparison ===\n")

# Summary by method (pooled across modes)
cat("\nPooled summary (all modes):\n")
summary_pooled <- df_var_comp %>%
  summarise(
    n = n(),
    mean_se_taylor = mean(se_analytical, na.rm = TRUE),
    mean_se_mid_calib = mean(se_mid_calib, na.rm = TRUE),
    mean_se_census_calib = mean(se_census_calib, na.rm = TRUE),
    mean_se_naive = mean(se_naive, na.rm = TRUE),
    median_se_taylor = median(se_analytical, na.rm = TRUE),
    median_se_mid_calib = median(se_mid_calib, na.rm = TRUE),
    median_se_census_calib = median(se_census_calib, na.rm = TRUE),
    median_se_naive = median(se_naive, na.rm = TRUE)
  )
print(summary_pooled)

# Correlations between methods
cat("\n=== Correlations between Methods ===\n")
cor_matrix <- df_var_comp %>%
  select(se_analytical, se_mid_calib, se_census_calib, se_naive) %>%
  cor(use = "complete.obs")
print(round(cor_matrix, 4))

# Ratios (how much larger naive is compared to calibrated)
cat("\n=== Variance Ratios ===\n")
df_var_comp %>%
  summarise(
    ratio_naive_to_census = mean(se_naive / se_census_calib, na.rm = TRUE),
    ratio_naive_to_mid = mean(se_naive / se_mid_calib, na.rm = TRUE),
    ratio_census_to_mid = mean(se_census_calib / se_mid_calib, na.rm = TRUE),
    ratio_taylor_to_mid = mean(se_analytical / se_mid_calib, na.rm = TRUE)
  ) %>%
  print()

cat("\n=== Variance comparison complete! ===\n")
cat("Saved: FigAppVarianceScatter.eps (3-panel: Taylor vs MiD-calib, Census vs MiD-calib, Naive vs Census-calib)\n")


