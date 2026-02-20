#### District FH vs HTH Comparison Visualization ####
# ==============================================================================
# Purpose: Compare district-level FH estimates with direct HTH estimates
# Generates scatter plots, CV comparisons, and efficiency gain visualizations
# ==============================================================================

rm(list = ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggpubr, colorspace, paletteer, sf, patchwork)

data_path <- "./data"
manuscript_path <- "./manuscript"  # EPS figures go here for LaTeX compilation

# Load auxiliaries
source(paste0(getwd(),"/R/auxiliaries/00_themes.r"))
source(paste0(getwd(),"/R/auxiliaries/01_plot_functions.r"))

#### Load Saved Model Data ####

# Load FH model (final validated model from Phase 3 CV)
fh_model <- readRDS(file = paste0(data_path, "/output/models/fh_model_final.rds"))

# Load direct estimates (same as plot_direct.r)
df_direct <- readRDS(file.path(data_path, "output/direct/df_HTH_perskm_analytical.rds"))

# Mode-specific comparisons
# Note: FH model estimates at domain level (ags5 × hvm)
# Both FH and direct estimates are mode-specific

# Extract FH estimates by domain
df_fh <- data.frame(
  ags5_hvm = fh_model$ind$Domain,
  FH_estimate = fh_model$ind$FH,
  FH_Direct = fh_model$ind$Direct  # Direct estimate used in model
)

# Extract mode-specific MSE from FH model
df_mse <- data.frame(
  ags5_hvm = fh_model$MSE$Domain,
  MSE_FH = fh_model$MSE$FH
)

# Build comparison dataset from direct estimates
df_mode_comparison <- df_direct %>%
  select(ags5_hvm, ags5, hvm_label, perskm_hth, var_hth, se_hth) %>%
  # Join FH estimates
  left_join(df_fh %>% select(ags5_hvm, FH_estimate), by = "ags5_hvm") %>%
  # Join MSE
  left_join(df_mse, by = "ags5_hvm") %>%
  # Rename for clarity
  dplyr::rename(HTH = perskm_hth, HTH_Var = var_hth, HTH_SE = se_hth) %>%
  mutate(
    # Calculate CV (coefficient of variation)
    CV_HTH = sqrt(HTH_Var) / HTH,
    CV_FH = sqrt(MSE_FH) / FH_estimate,
    # Calculate efficiency gain based on CV: (CV_HTH - CV_FH) / CV_HTH
    # Positive values = FH improves precision (reduces CV)
    Eff_gain = (CV_HTH - CV_FH) / CV_HTH,
    Eff_gain_rescale = Eff_gain
  )

#### Visualization Parameters ####
# Core parameters loaded from 00_themes.r:
# textwidth, scal_ch, map_2panel_width/height, map_4panel_width/height,
# legend_barwidth, legend_barheight

# Script-specific parameters
pointshape <- 21
pointsize <- 2
windowsFonts(Times=windowsFont("Times"))
pointalpha <- 0.4

modelcol <- paletteer_c("grDevices::PiYG", n = 10)[9]
directcol <- paletteer_c("grDevices::PiYG", n = 10)[2]

#### Plot FH vs HTH Line Plot (sorted by HTH, by mode) ####

# Define mode colors
mode_colors <- c("fuss" = "#E69F00", "fahrrad" = "#56B4E9",
                 "miv" = "#009E73", "oepv" = "#CC79A7")

# Prepare data for line plot: create rank within each mode based on HTH
df_lineplot <- df_mode_comparison %>%
  group_by(hvm_label) %>%
  arrange(HTH) %>%
  mutate(rank = row_number()) %>%
  ungroup() %>%
  select(rank, hvm_label, HTH, FH_estimate) %>%
  pivot_longer(cols = c("HTH", "FH_estimate"),
               names_to = "Method",
               values_to = "Estimate") %>%
  mutate(
    Method = recode(Method,
                    "HTH" = "Direct (HTH)",
                    "FH_estimate" = "FH Model"),
    # Create combined factor for color mapping (mode × method)
    mode_method = paste(hvm_label, Method, sep = "_")
  )

# Create color palette: full color for Direct, faint (50% alpha) for FH
# We'll use geom_line with alpha aesthetic for each method
Line_FH_HTH <- ggplot(df_lineplot,
                      aes(x = rank, y = Estimate, group = Method)) +
  # FH Model lines (faint, drawn first so they're underneath)
  geom_line(data = df_lineplot %>% filter(Method == "FH Model"),
            aes(color = hvm_label),
            linewidth = 0.8,
            alpha = 0.5) +
  # Smoothed FH Model lines (black, LOESS smoothing)
  geom_smooth(data = df_lineplot %>% filter(Method == "FH Model"),
              aes(y = Estimate),
              method = "loess",
              span = 0.3,
              se = FALSE,
              color = "black",
              linewidth = 1.2,
              linetype = "solid") +
  # Direct HTH lines (full color, drawn on top)
  geom_line(data = df_lineplot %>% filter(Method == "Direct (HTH)"),
            aes(color = hvm_label),
            linewidth = 0.6,
            alpha = 1) +
  facet_wrap(~ hvm_label, ncol = 2, scales = "free_y",
             labeller = labeller(hvm_label = c("fuss" = "Walking (zu Fuß)",
                                                "fahrrad" = "Cycling (Fahrrad)",
                                                "miv" = "Car (MIV)",
                                                "oepv" = "Public Transit (ÖPV)"))) +
  scale_color_manual(values = mode_colors,
                     guide = "none") +  # Hide legend since facets show modes
  xlab("Districts (sorted by increasing HTH estimate)") +
  ylab("Person km/day") +
  ggtitle("(a) FH Model Smoothing by Mode") +
  General_theme() +
  # Add custom legend for line types
  annotate("text", x = Inf, y = Inf,
           label = "Colored: Direct (HTH)\nFaint: FH Model\nBlack: FH Trend",
           hjust = 1.05, vjust = 1.1, size = 3, color = "gray30")

#### Plot FH vs HTH Scatter (by mode) ####

Scatter_FH_HTH <- ggplot(data = df_mode_comparison,
                           aes(y = FH_estimate, x = HTH, color = hvm_label)) +
  geom_point(shape = pointshape,
             size = pointsize,
             alpha = pointalpha) +
  geom_abline(slope = 1, intercept = 0, col = "black", lwd = 0.5, linetype = "dashed") +
  scale_color_manual(values = mode_colors,
                     name = "Mode",
                     labels = c("Walking (zu Fuß)", "Cycling (Fahrrad)", "Car (MIV)", "Public Transit (ÖPV)")) +
  xlab("Direct (HJ) Estimate [km/day/district]") +
  ylab("FH Estimate [km/day/district]") +
  ggtitle("(a) FH vs. HJ by Mode") +
  General_theme() +
  theme(legend.position = "none")  # Legend will be shared at bottom

#### Plot CV Boxplot (by mode) ####

df_boxplot <- df_mode_comparison %>%
  select(ags5_hvm, hvm_label, CV_FH, CV_HTH) %>%
  pivot_longer(cols = c("CV_FH", "CV_HTH"),
               names_to = "Method",
               values_to = "CV") %>%
  mutate(Method = recode(Method, "CV_FH" = "FH", "CV_HTH" = "HJ"))

Box_FH_HTH <- ggplot(df_boxplot,  aes(x = hvm_label, y = CV, fill = Method)) +
  geom_boxplot(outlier.alpha = pointalpha,
               outlier.shape = pointshape,
               position = position_dodge(width = 0.8)) +
  geom_hline(yintercept = 0.20,
             color = "#6A0DAD",
             linewidth = 0.5,
             linetype = "dashed") +
  labs(title = "(b) CV by Mode: FH vs. HJ",
       x = "Mode",
       y = "Coefficient of Variation"
       ) +
  scale_fill_manual(values = c("FH" = modelcol, "HJ" = directcol),
                    name = "Method") +
  scale_x_discrete(labels = c("fuss" = "Walking (zu Fuß)",
                              "fahrrad" = "Cycling (Fahrrad)",
                              "miv" = "Car (MIV)",
                              "oepv" = "Public Transit (ÖPV)")) +
  General_theme() +
  theme(legend.position = "bottom")

#### Save Comparison Plots Separately ####

# Add mode legend to scatter plot
Scatter_FH_HTH_with_legend <- Scatter_FH_HTH +
  theme(legend.position = "bottom")

# Re-create scatter with facets by mode
# Create separate plots per mode with coord_fixed, then combine with patchwork
# This ensures 45° bisectrix in each panel

# Compute shared CV limits across all modes for consistent fill scale
cv_range <- range(df_mode_comparison$CV_HTH, na.rm = TRUE)

# Helper function to create single mode scatter plot with regression annotation
create_mode_scatter <- function(data, mode, mode_label, color, tag = "",
                                cv_limits = NULL) {
  df_mode <- data %>% filter(hvm_label == mode)

  # Calculate square axis limits (same range for x and y)
  axis_min <- min(c(df_mode$HTH, df_mode$FH_estimate), na.rm = TRUE)
  axis_max <- max(c(df_mode$HTH, df_mode$FH_estimate), na.rm = TRUE)

  # Fit linear model to get regression coefficients
  lm_fit <- lm(FH_estimate ~ HTH, data = df_mode)
  beta0 <- coef(lm_fit)[1]
  beta1 <- coef(lm_fit)[2]
  r2 <- summary(lm_fit)$r.squared

  # Compute alpha: low CV = high alpha (0.9), high CV = low alpha (0.15)
  df_mode <- df_mode %>%
    mutate(cv_alpha = scales::rescale(pmin(CV_HTH, 1), to = c(0.9, 0.15)))

  ggplot(data = df_mode, aes(y = FH_estimate, x = HTH, fill = CV_HTH,
                              alpha = cv_alpha)) +
    geom_point(shape = pointshape, size = pointsize, stroke = 0.3, color = color) +
    scale_alpha_identity() +
    geom_smooth(method = "lm", se = FALSE, linewidth = 0.8, color = color) +
    geom_abline(slope = 1, intercept = 0, col = "black", lwd = 0.5, linetype = "dashed") +
    # Two separate annotations for proper parsing (parse doesn't handle newlines)
    annotate("text",
             x = axis_min + (axis_max - axis_min) * 0.05,
             y = axis_max - (axis_max - axis_min) * 0.02,
             label = paste0("beta[1] == '", sprintf("%.2f", beta1), "'"),
             hjust = 0, vjust = 1, size = 3, parse = TRUE) +
    annotate("text",
             x = axis_min + (axis_max - axis_min) * 0.05,
             y = axis_max - (axis_max - axis_min) * 0.12,
             label = sprintf("R^2 == %.2f", r2),
             hjust = 0, vjust = 1, size = 3, parse = TRUE) +
    # CV fill: dark = low CV (reliable), light = high CV (unreliable)
    scale_fill_gradient(low = color, high = "white",
                        limits = c(0, 1),
                        oob = scales::squish,
                        breaks = c(0, 0.5, 1),
                        labels = c("0", "0.5", "1"),
                        name = NULL,
                        guide = guide_colorbar(
                          barwidth = legend_barwidth,
                          barheight = legend_barheight,
                          frame.colour = "black",
                          ticks.colour = "black",
                          label.hjust = 0.5
                        )) +
    scale_x_continuous(limits = c(axis_min, axis_max)) +
    scale_y_continuous(limits = c(axis_min, axis_max)) +
    coord_fixed(ratio = 1) +
    labs(title = paste0(tag, " ", mode_label), x = NULL, y = NULL) +
    General_theme() +
    theme(legend.position = "bottom",
          legend.margin = margin(0, 0, 0, 0),
          legend.box.margin = margin(-5, 0, -3, 0))
}

# Create individual scatter plots (shared CV limits for consistent fill scale)
scatter_fuss <- create_mode_scatter(df_mode_comparison, "fuss", "Walking (zu Fuß)",
                                     mode_colors["fuss"], "(a)", cv_limits = cv_range)
scatter_fahrrad <- create_mode_scatter(df_mode_comparison, "fahrrad", "Cycling (Fahrrad)",
                                        mode_colors["fahrrad"], "(b)", cv_limits = cv_range)
scatter_miv <- create_mode_scatter(df_mode_comparison, "miv", "Car (MIV)",
                                    mode_colors["miv"], "(c)", cv_limits = cv_range)
scatter_oepv <- create_mode_scatter(df_mode_comparison, "oepv", "Public Transit (ÖPV)",
                                     mode_colors["oepv"], "(d)", cv_limits = cv_range)

# Combine with patchwork — each panel keeps its own legend (different color ramps)
Scatter_FH_HTH_final <- (scatter_fuss | scatter_fahrrad) / (scatter_miv | scatter_oepv) +
  plot_annotation(
    caption = "Direct (HJ) Estimate [km/day/district] (x-axis) vs FH Estimate [km/day/district] (y-axis)"
  ) &
  theme(plot.caption = element_text(hjust = 0.5, size = 9))

# Save scatter plot separately
# Height ~= width for 2x2 grid of square plots (coord_fixed) to fill textwidth
ggsave(filename = file.path(manuscript_path, "FigScatter.eps"),
       plot = Scatter_FH_HTH_final,
       width = textwidth,
       height = textwidth * 1.3,  # Taller to give colorbars + pane titles room
       device = cairo_ps,
       units = "cm")

cat("Scatter plot (FH vs HJ) saved!\n")

# Save boxplot separately
ggsave(filename = file.path(manuscript_path, "FigBoxplot.eps"),
       plot = Box_FH_HTH,
       width = textwidth,
       height = textwidth * 0.7,
       device = cairo_ps,
       units = "cm")

cat("Boxplot (CV by mode) saved!\n")

# Save line plot separately
ggsave(filename = file.path(manuscript_path, "FigComparison_Lines.eps"),
       plot = Line_FH_HTH,
       width = textwidth,
       height = textwidth*0.65,
       device = cairo_ps,
       units = "cm")

cat("Line plot (FH vs HTH by mode, sorted) saved!\n")

#### Plot Efficiency Gain Maps (by Mode) ####

# Load German district shapefile
sf_districts <- sf::st_read(
  paste0(data_path, "/raw/shapes/vg2500_01-01.gk3.shape/vg2500/VG2500_KRS.shp"),
  quiet = TRUE
) %>%
  mutate(ags5 = substr(ARS, 1, 5)) %>%
  select(ags5, geometry)

# Create separate datasets for each mode
df_fuss_eff <- df_mode_comparison %>% filter(hvm_label == "fuss")
df_fahrrad_eff <- df_mode_comparison %>% filter(hvm_label == "fahrrad")
df_miv_eff <- df_mode_comparison %>% filter(hvm_label == "miv")
df_oepv_eff <- df_mode_comparison %>% filter(hvm_label == "oepv")

# Merge with spatial data
sf_fuss_eff <- sf_districts %>% left_join(df_fuss_eff, by = "ags5")
sf_fahrrad_eff <- sf_districts %>% left_join(df_fahrrad_eff, by = "ags5")
sf_miv_eff <- sf_districts %>% left_join(df_miv_eff, by = "ags5")
sf_oepv_eff <- sf_districts %>% left_join(df_oepv_eff, by = "ags5")

# Calculate shared breaks and labels across all modes
all_eff_gains <- df_mode_comparison$Eff_gain_rescale
temp <- summary(all_eff_gains, na.rm = TRUE)

my_breaks <- c(
  temp["Min."],
  0,
  temp["Median"],
  temp["Max."]
)

my_limits <- c(
  temp["Min."],
  temp["Max."]
)

my_labels <- c(
  temp["Min."],
  0,
  temp["Median"],
  temp["Max."]
) %>% round(2)

# Helper function to create efficiency gain map
create_eff_map <- function(sf_data, mode_name, tag = "", show_legend = FALSE) {
  p <- ggplot(data = sf_data) +
    geom_sf(aes(fill = Eff_gain_rescale), color = "black", size = 0.1) +
    colorspace::scale_fill_continuous_divergingx(
      palette = "PiYG",
      na.value = "grey90",
      labels = my_labels,
      limits = my_limits,
      breaks = my_breaks,
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
      title = paste0(tag, " Efficiency Gain: ", mode_name),
      fill = ""
    ) +
    coord_sf(datum = NA) +
    Map_theme(
      legend.position = if (show_legend) "bottom" else "none"
    )

  return(p)
}

# Create efficiency maps (only last one shows legend) - with German MiD abbreviations
eff_fuss <- create_eff_map(sf_fuss_eff, "Walking (zu Fuß)", "(a)", show_legend = FALSE)
eff_fahrrad <- create_eff_map(sf_fahrrad_eff, "Cycling (Fahrrad)", "(b)", show_legend = FALSE)
eff_miv <- create_eff_map(sf_miv_eff, "Car (MIV)", "(c)", show_legend = FALSE)
eff_oepv <- create_eff_map(sf_oepv_eff, "Public Transit (ÖPV)", "(d)", show_legend = TRUE)

# Combine with shared legend at bottom and spacing between panels
design <- "A#B
###
C#D"
fig_eff_multimodal <- eff_fuss + eff_fahrrad + eff_miv + eff_oepv +
  patchwork::plot_layout(design = design, widths = c(1, 0.05, 1), heights = c(1, 0.05, 1), guides = "collect") &
  theme(legend.position = "bottom")

#### Save Efficiency Maps ####

ggsave(
  filename = file.path(manuscript_path, "FigEfficiency.eps"),
  plot = fig_eff_multimodal,
  width = map_4panel_width,
  height = map_4panel_height,
  device = cairo_ps,
  units = "cm"
)

cat("Efficiency gain maps (4 modes) saved!\n")
cat("All comparison plots complete!\n")
