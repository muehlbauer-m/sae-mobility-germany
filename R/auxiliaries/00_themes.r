################################################################################
##### Visualization Parameters (consistent across all plotting scripts)
################################################################################

# Text width for LaTeX manuscript (cm)
textwidth <- 15.99773

# Character scaling factor
scal_ch <- 0.8

# 2-panel maps (1x2 horizontal)
map_2panel_width <- textwidth
map_2panel_height <- textwidth * 0.75

# 4-panel maps (2x2 grid) - slightly less than 2x height to reduce whitespace
map_4panel_width <- textwidth
map_4panel_height <- map_2panel_height * 1.9

# Legend parameters
legend_barwidth <- unit(5, "cm")
legend_barheight <- unit(0.4, "cm")

################################################################################
##### General Theme
################################################################################
General_theme <- function(scal_ch = 1,
                          default_font_color = "black",
                          default_background_color = "white",...) {
  theme_minimal() +
    theme(
      text = element_text(color = default_font_color),
      # add a subtle grid
      panel.grid.major = element_line(color = "#dbdbd9", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      # background colors
      plot.background = element_rect(fill = default_background_color,
                                     color = NA),
      panel.background = element_rect(fill = default_background_color,
                                      color = NA),
      legend.background = element_rect(fill = default_background_color,
                                       color = NA),
      # borders and margins
      plot.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0),
      panel.border = element_blank(),
      #panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
      # titles
      legend.title = element_text(size = 9.5*scal_ch),
      legend.text = element_text(size = 9*scal_ch, hjust = 0,
                                 color = default_font_color),
      plot.title = element_text(size = 11*scal_ch, hjust = 0.5,
                                color = default_font_color),
      plot.subtitle = element_text(size = 11*scal_ch, hjust = 0.5,
                                   color = default_font_color,
                                   margin = ggplot2::margin(b = -0.1,
                                                   t = -0.1,
                                                   l = 2,
                                                   unit = "cm"),
                                   debug = F),
      axis.title.x.bottom =element_text(size = 11*scal_ch,
                                color = default_font_color),
      axis.title.y.left =element_text(size = 11*scal_ch,
                                color = default_font_color),
      plot.tag = element_text(size = 11*scal_ch),
      # captions
      plot.caption = element_text(size = 7,
                                  hjust = .5,
                                  margin = ggplot2::margin(t = 0.2,
                                                  b = 0,
                                                  unit = "cm"),
                                  color = "#939184"),
      ...
    )
}


################################################################################
##### Map Theme
################################################################################

Map_theme <- function(scal_ch = 1,
                      default_font_color = "black",
                      default_background_color = "white",...) {
  theme_minimal() +
    theme(
      text = element_text(color = default_font_color),
      # remove all axes
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      # add a subtle grid
      panel.grid.major = element_line(color = "#dbdbd9", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      # background colors
      plot.background = element_rect(fill = default_background_color,
                                     color = NA),
      panel.background = element_rect(fill = default_background_color,
                                      color = NA),
      legend.background = element_rect(fill = default_background_color,
                                       color = NA),
      # borders and margins
      plot.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0),
      panel.border = element_blank(),
      #panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
      # titles
      legend.title = element_text(size = 9.5*scal_ch),
      legend.text = element_text(size = 9*scal_ch, hjust = 0,
                                 color = default_font_color),
      plot.title = element_text(size = 11*scal_ch, hjust = 0.5,
                                color = default_font_color),
      plot.subtitle = element_text(size = 11*scal_ch, hjust = 0.5,
                                   color = default_font_color,
                                   margin = ggplot2::margin(b = -0.1,
                                                   t = -0.1,
                                                   l = 2,
                                                   unit = "cm"),
                                   debug = F),
      plot.tag = element_text(size = 11*scal_ch),
      # captions
      plot.caption = element_text(size = 7,
                                  hjust = .5,
                                  margin = ggplot2::margin(t = 0.2,
                                                  b = 0,
                                                  unit = "cm"),
                                  color = "#939184"),
      ...
    )
}

################################################################################
##### Old General Theme
################################################################################

General_theme_old <- function(scal_ch, default_font_family = "Times",
                          default_font_color = "black",
                          default_background_color = "white",...) {
  theme_minimal() +
    theme(
      text = element_text(family = default_font_family,
                          color = default_font_color),
      # remove all axes
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      # add a subtle grid
      panel.grid.major = element_line(color = "#dbdbd9", linewidth = 0.2),
      panel.grid.minor = element_blank(),
      # background colors
      plot.background = element_rect(fill = default_background_color,
                                     color = NA),
      panel.background = element_rect(fill = default_background_color,
                                      color = NA),
      legend.background = element_rect(fill = default_background_color,
                                       color = NA),
      # borders and margins
      plot.margin = unit(c(.5, .5, .2, .5), "cm"),
      panel.border = element_blank(),
      #panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
      # titles
      legend.title = element_text(size = 11*scal_ch),
      legend.text = element_text(size = 11*scal_ch, hjust = 0,
                                 color = default_font_color),
      plot.title = element_text(size = 13*scal_ch, hjust = 0.5,
                                color = default_font_color),
      plot.subtitle = element_text(size = 11*scal_ch, hjust = 0.5,
                                   color = default_font_color,
                                   margin = margin(b = -0.1,
                                                   t = -0.1,
                                                   l = 2,
                                                   unit = "cm"),
                                   debug = F),
      plot.tag = element_text(size = 11*scal_ch),
      # captions
      plot.caption = element_text(size = 7,
                                  hjust = .5,
                                  margin = margin(t = 0.2,
                                                  b = 0,
                                                  unit = "cm"),
                                  color = "#939184"),
      ...
    )
}
