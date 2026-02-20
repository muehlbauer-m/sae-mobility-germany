#### Scatter Plot with Gamma/CV-based Color Fill ####
# ==============================================================================
# Purpose: Create scatter plot for public transit where point color
#          reflects compression (via gamma or CV)
#          Panel (a): Color by gamma (low gamma = dark = more compression)
#          Panel (b): Color by CV (high CV = dark = more compression)
# ==============================================================================

rm(list = ls())

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggpubr, colorspace, paletteer, patchwork)

data_path <- "./data"
manuscript_path <- "./manuscript"
reply_path <- "./reply"

# Load auxiliaries
source(paste0(getwd(),"/R/auxiliaries/00_themes.r"))

#### Visualization Parameters ####
# Match manuscript scatter plot style (plot_comparison.r)
pointshape <- 21
pointsize <- 2
pointalpha <- 0.8

#### Load Data ####

# Load FH model
fh_model <- readRDS(file = paste0(data_path, "/output/models/fh_model_final.rds"))

# Load direct estimates (includes se_hth)
df_direct <- readRDS(file.path(data_path, "output/direct/df_HTH_perskm_analytical.rds"))

# Extract FH estimates
df_fh <- data.frame(
  ags5_hvm = fh_model$ind$Domain,
  FH_estimate = fh_model$ind$FH
)

# Extract gamma (shrinkage factor)
df_gamma <- data.frame(
  ags5_hvm = fh_model$model$gamma$Domain,
  Gamma = fh_model$model$gamma$Gamma
)

# Build comparison dataset
df_comparison <- df_direct %>%
  select(ags5_hvm, ags5, hvm_label, perskm_hth, se_hth) %>%
  left_join(df_fh, by = "ags5_hvm") %>%
  left_join(df_gamma, by = "ags5_hvm")

# Filter to public transit only and calculate CV
df_oepv <- df_comparison %>%
  filter(hvm_label == "oepv") %>%
  filter(!is.na(Gamma)) %>%
  mutate(cv_hth = se_hth / perskm_hth)

#### Create Scatter Plots ####

# Calculate axis limits for square plot
axis_min <- min(c(df_oepv$perskm_hth, df_oepv$FH_estimate), na.rm = TRUE)
axis_max <- max(c(df_oepv$perskm_hth, df_oepv$FH_estimate), na.rm = TRUE)

# Fit linear model for annotation
lm_fit <- lm(FH_estimate ~ perskm_hth, data = df_oepv)
beta1 <- coef(lm_fit)[2]
r2 <- summary(lm_fit)$r.squared

# Get CV range for scaling
cv_min <- min(df_oepv$cv_hth, na.rm = TRUE)
cv_max <- max(df_oepv$cv_hth, na.rm = TRUE)

#### Panel (a): Fill color by Gamma ####
# Color mapping: low gamma (strong compression) = light, high gamma (less compression) = dark/red
fig_scatter_gamma <- ggplot(data = df_oepv,
                            aes(x = perskm_hth, y = FH_estimate, fill = 1 - Gamma)) +
  geom_point(shape = pointshape, size = pointsize, color = "black", stroke = 0.3, alpha = pointalpha) +
  geom_abline(slope = 1, intercept = 0, col = "black", lwd = 0.5, linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8, color = "#CC79A7") +
  # Regression annotations (top-left, matching manuscript style)
  annotate("text",
           x = axis_min + (axis_max - axis_min) * 0.05,
           y = axis_max - (axis_max - axis_min) * 0.02,
           label = sprintf("beta[1] == %.2f", beta1),
           hjust = 0, vjust = 1, size = 3, parse = TRUE) +
  annotate("text",
           x = axis_min + (axis_max - axis_min) * 0.05,
           y = axis_max - (axis_max - axis_min) * 0.12,
           label = sprintf("R^2 == %.2f", r2),
           hjust = 0, vjust = 1, size = 3, parse = TRUE) +
  scale_fill_continuous_sequential(
    palette = "PuRd",
    rev = FALSE,
    limits = c(0, 1),
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    labels = c("1", "0.75", "0.5", "0.25", "0"),
    name = expression(gamma[d]),
    guide = guide_colorbar(
      barwidth = legend_barwidth,
      barheight = legend_barheight,
      title.position = "top",
      title.hjust = 0.5,
      frame.colour = "black",
      ticks.colour = "black",
      label.hjust = 0.5
    )
  ) +
  scale_x_continuous(limits = c(axis_min, axis_max)) +
  scale_y_continuous(limits = c(axis_min, axis_max)) +
  coord_fixed(ratio = 1) +
  labs(
    title = expression("(a) Color by " * gamma[d]),
    x = "Direct (HJ) Estimate [km/day]",
    y = "FH Estimate [km/day]"
  ) +
  General_theme() +
  theme(legend.position = "none")

#### Panel (b): Fill color by CV ####
# Color mapping: high CV (strong compression) = light, low CV (less compression) = dark/red
# Scale CV to [0,1] range
df_oepv <- df_oepv %>%
  mutate(cv_scaled = (cv_hth - cv_min) / (cv_max - cv_min))

fig_scatter_cv <- ggplot(data = df_oepv,
                         aes(x = perskm_hth, y = FH_estimate, fill = cv_scaled)) +
  geom_point(shape = pointshape, size = pointsize, color = "black", stroke = 0.3, alpha = pointalpha) +
  geom_abline(slope = 1, intercept = 0, col = "black", lwd = 0.5, linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8, color = "#CC79A7") +
  # Regression annotations (top-left, matching manuscript style)
  annotate("text",
           x = axis_min + (axis_max - axis_min) * 0.05,
           y = axis_max - (axis_max - axis_min) * 0.02,
           label = sprintf("beta[1] == %.2f", beta1),
           hjust = 0, vjust = 1, size = 3, parse = TRUE) +
  annotate("text",
           x = axis_min + (axis_max - axis_min) * 0.05,
           y = axis_max - (axis_max - axis_min) * 0.12,
           label = sprintf("R^2 == %.2f", r2),
           hjust = 0, vjust = 1, size = 3, parse = TRUE) +
  scale_fill_continuous_sequential(
    palette = "PuRd",
    rev = FALSE,
    limits = c(0, 1),
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    labels = round(c(cv_min, cv_min + 0.25*(cv_max-cv_min), cv_min + 0.5*(cv_max-cv_min),
                     cv_min + 0.75*(cv_max-cv_min), cv_max), 2),
    name = expression(CV[d]),
    guide = guide_colorbar(
      barwidth = legend_barwidth,
      barheight = legend_barheight,
      title.position = "top",
      title.hjust = 0.5,
      frame.colour = "black",
      ticks.colour = "black",
      label.hjust = 0.5
    )
  ) +
  scale_x_continuous(limits = c(axis_min, axis_max)) +
  scale_y_continuous(limits = c(axis_min, axis_max)) +
  coord_fixed(ratio = 1) +
  labs(
    title = expression("(b) Color by " * CV[d]),
    x = "Direct (HJ) Estimate [km/day]",
    y = "FH Estimate [km/day]"
  ) +
  General_theme() +
  theme(legend.position = "none")

#### Combine Panels with Shared Legend at Bottom ####

# Extract legends from individual plots
fig_scatter_gamma_legend <- fig_scatter_gamma +
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

fig_scatter_cv_legend <- fig_scatter_cv +
  theme(legend.position = "bottom",
        legend.direction = "horizontal")

# Combine plots side by side with legends at bottom using patchwork
fig_combined <- (fig_scatter_gamma_legend | fig_scatter_cv_legend) +
  plot_layout(guides = "keep") &  # Keep individual legends
  theme(legend.position = "bottom")

# Display plot
print(fig_combined)

# Save combined plot (textwidth for 2 panels side by side, square aspect for coord_fixed)
ggsave(
  filename = file.path(reply_path, "FigScatterGammaAlpha_OEPV.eps"),
  plot = fig_combined,
  width = textwidth,
  height = textwidth,  # Slightly taller than half to accommodate legend
  device = cairo_ps,
  units = "cm"
)

cat("Scatter plot with gamma/CV-based color saved to manuscript/FigScatterGammaAlpha_OEPV.eps\n")

# Print summary statistics
cat("\nPublic Transit Gamma Summary:\n")
print(summary(df_oepv$Gamma))

cat("\nPublic Transit CV Summary:\n")
print(summary(df_oepv$cv_hth))

cat("\nCorrelation between perskm_hth and Gamma:\n")
cat(sprintf("r = %.3f\n", cor(df_oepv$perskm_hth, df_oepv$Gamma, use = "complete.obs")))

cat("\nCorrelation between perskm_hth and CV:\n")
cat(sprintf("r = %.3f\n", cor(df_oepv$perskm_hth, df_oepv$cv_hth, use = "complete.obs")))

cat("\nCorrelation between Gamma and 1/CV:\n")
cat(sprintf("r = %.3f\n", cor(df_oepv$Gamma, 1/df_oepv$cv_hth, use = "complete.obs")))
