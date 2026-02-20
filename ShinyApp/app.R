#### MiD Mobility Shiny Application ####
# District-level mobility estimates for Germany
# Displays FH small area estimates across 4 transport modes

if (!require("pacman")) install.packages("pacman")
pacman::p_load(shiny, bslib, ggplot2, sf, tidyverse, cowplot, viridis, colorspace,
               ggiraph, patchwork, DT)

#### Load pre-computed data ####
load("mid_shiny_data.RData")

#### Load reference table data ####
df_varset   <- read.csv("data/tbl_final_varset.csv", stringsAsFactors = FALSE)
df_destatis <- read.csv("data/tbl_admin_stats_tables.csv", stringsAsFactors = FALSE)
df_inkar    <- read.csv("data/tbl_inkar_variables.csv", stringsAsFactors = FALSE)

#### Theme colors (warm grey-yellow) ####
bg_color <- "#2b2b2b"
panel_color <- "#363636"
text_color <- "#FFCC33"
grid_color <- "#3d3d3d"
accent_color <- "#FF9900"

#### Themes (dark) ####

Map_theme <- function(scal_ch = 1, default_font_family = "sans", ...) {
  theme_minimal() +
    theme(
      text = element_text(family = default_font_family, color = text_color),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      panel.grid.major = element_line(color = grid_color, linewidth = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = bg_color, color = NA),
      panel.background = element_rect(fill = bg_color, color = NA),
      legend.background = element_rect(fill = bg_color, color = NA),
      plot.margin = unit(c(.5, .5, .2, .5), "cm"),
      panel.border = element_blank(),
      legend.title = element_text(size = 8 * scal_ch, color = text_color),
      legend.text = element_text(size = 7 * scal_ch, hjust = 0, color = text_color),
      plot.title = element_text(size = 13 * scal_ch, hjust = 0.5, color = text_color),
      plot.subtitle = element_text(size = 11 * scal_ch, hjust = 0.5, color = text_color),
      plot.tag = element_text(size = 11 * scal_ch, color = text_color),
      plot.caption = element_text(size = 7, hjust = .5, color = "#7a7a8a"),
      ...
    )
}

#### Build tooltip column ####
sf_data <- sf_data %>%
  mutate(
    tooltip = paste0(
      district_name, " (", ags5, ")\n",
      "FH: ", round(FH, 2), " km/day (CV: ", round(CV_FH_pct, 1), "%)\n",
      "Direct: ", round(Direct, 2), " km/day (CV: ", round(CV_HTH_pct, 1), "%)"
    )
  )

#### Helper: create a single choropleth map ####

create_map <- function(sf_data, fill_var, metric, mode_name,
                       highlight_ags5 = NULL, tag = "") {

  p <- ggplot(data = sf_data) +
    geom_sf_interactive(
      aes(fill = .data[[fill_var]], tooltip = tooltip, data_id = ags5),
      color = grid_color, size = 0.1
    )

  # Color scale depends on metric
  if (metric %in% c("FH", "Direct")) {
    p <- p + scale_fill_viridis(
      option = "H", na.value = grid_color, name = "km/day",
      guide = guide_colorbar(
        frame.colour = NA, ticks.colour = NA,
        barwidth = unit(4, "cm"), barheight = unit(0.4, "cm"),
        title.position = "top", title.hjust = 0.5
      )
    )
  } else if (metric %in% c("CV_FH_pct", "CV_HTH_pct")) {
    p <- p + scale_fill_continuous_diverging(
      palette = "Blue-Red 3", c1 = 130, l1 = 2, l2 = 100,
      p1 = 0.6, p2 = 1.2,
      limits = c(0, 120), mid = 20, na.value = grid_color,
      name = "CV (%)",
      breaks = c(0, 20, 40, 60, 80, 100, 120),
      labels = paste0(c(0, 20, 40, 60, 80, 100, 120), "%"),
      guide = guide_colorbar(
        frame.colour = NA, ticks.colour = NA,
        barwidth = unit(4, "cm"), barheight = unit(0.4, "cm"),
        title.position = "top", title.hjust = 0.5
      )
    )
  } else if (metric == "RV") {
    p <- p + scale_fill_continuous_divergingx(
      palette = "PiYG", na.value = grid_color, name = "RV",
      guide = guide_colorbar(
        frame.colour = NA, ticks.colour = NA,
        barwidth = unit(4, "cm"), barheight = unit(0.4, "cm"),
        title.position = "top", title.hjust = 0.5
      )
    )
  }

  # Highlight selected district
  if (!is.null(highlight_ags5)) {
    highlight_sf <- shp_slim %>% filter(ags5 == highlight_ags5)
    if (nrow(highlight_sf) > 0) {
      p <- p + geom_sf(data = highlight_sf, fill = NA, color = "red", linewidth = 1.5)
    }
  }

  p <- p +
    labs(x = NULL, y = NULL, title = mode_name, tag = tag) +
    coord_sf(datum = NA) +
    Map_theme(legend.position = "bottom")

  return(p)
}

#### UI ####

ui <- fluidPage(

  theme = bs_theme(
    bg = bg_color, fg = text_color,
    primary = accent_color, secondary = panel_color,
    base_font = font_google("Inter"),
    "input-bg" = panel_color,
    "input-color" = text_color,
    "input-border-color" = "#4a4a4a",
    "table-color" = text_color,
    "table-bg" = panel_color,
    "table-striped-bg" = "#303030",
    "table-hover-bg" = "#4a4032",
    "card-bg" = panel_color
  ),
  tags$style(HTML(paste0(
    "svg { background: ", bg_color, " !important; }",
    ".girafe_container_std { background: ", bg_color, " !important; border: none !important; padding: 0 !important; margin: 0 !important; }",
    "div[id$='_container'] { background: ", bg_color, " !important; }",
    ".girafe_container_std div { background: ", bg_color, " !important; }",
    ".shiny-html-output { background: ", bg_color, " !important; }",
    ".girafe_container_std canvas { background: ", bg_color, " !important; }",
    # DT dark theme overrides
    ".dataTables_wrapper { color: ", text_color, " !important; }",
    ".dataTables_wrapper .dataTables_filter input,",
    ".dataTables_wrapper .dataTables_length select {",
    "  background-color: ", panel_color, " !important;",
    "  color: ", text_color, " !important;",
    "  border: 1px solid #4a4a4a !important; }",
    ".dataTables_wrapper .dataTables_info,",
    ".dataTables_wrapper .dataTables_paginate { color: ", text_color, " !important; }",
    ".dataTables_wrapper .dataTables_paginate .paginate_button { color: ", text_color, " !important; }",
    ".dataTables_wrapper .dataTables_paginate .paginate_button.current {",
    "  color: ", bg_color, " !important; background: ", accent_color, " !important; }",
    "table.dataTable { color: #E0E0E0 !important; }",
    "table.dataTable thead th { color: ", text_color, " !important; border-bottom: 1px solid #4a4a4a !important; }",
    "table.dataTable tbody tr { background-color: ", panel_color, " !important; }",
    "table.dataTable tbody tr:hover { background-color: #4a4032 !important; }",
    "table.dataTable tbody tr.odd { background-color: #303030 !important; }",
    # DT column filter inputs
    "table.dataTable thead .dt-column-order { color: ", text_color, " !important; }",
    "table.dataTable thead input, table.dataTable thead select {",
    "  background-color: ", panel_color, " !important;",
    "  color: ", text_color, " !important;",
    "  border: 1px solid #4a4a4a !important; }"
  ))),
  titlePanel("District-Level Mobility Estimates for Germany"),
  navset_tab(
    nav_panel("Results",
      fluidRow(
        column(
          width = 3,
          selectInput(
            "mode", "Transport Mode",
            choices = setNames(names(mode_labels), mode_labels),
            selected = "fuss"
          ),
          selectInput(
            "metric", "Map Pair",
            choices = c(
              "FH / CV" = "FH_CV",
              "FH / Efficiency Gain" = "FH_RV",
              "Direct / CV" = "Direct_CV"
            ),
            selected = "FH_CV"
          ),
          selectInput(
            "district", "District",
            choices = district_choices,
            selected = district_choices[1],
            selectize = TRUE
          ),
          hr(),
          h4("Selected District"),
          tableOutput("summary_table")
        ),
        column(
          width = 9, align = "center",
          uiOutput("map_girafe_ui")
        )
      )
    ),
    nav_panel("Final Variables",
      div(style = "padding: 20px;",
        h4("Final Variable Set", style = paste0("color:", text_color, ";")),
        p("All variables in the winning baseline model, grouped by type: main effects, polynomial terms, and mode \u00d7 covariate interactions.",
          style = "color: #E0E0E0;"),
        DT::dataTableOutput("tbl_varset")
      )
    ),
    nav_panel("Destatis Census",
      div(style = "padding: 20px;",
        h4("Destatis Census Tables", style = paste0("color:", text_color, ";")),
        p("Census tables acquired from GENESIS-Online and Regionaldatenbank Deutschland as candidate auxiliary variables. All tables refer to 2017 at district level.",
          style = "color: #E0E0E0;"),
        DT::dataTableOutput("tbl_destatis")
      )
    ),
    nav_panel("INKAR Indicators",
      div(style = "padding: 20px;",
        h4("INKAR Regional Indicators", style = paste0("color:", text_color, ";")),
        p("All INKAR indicators used in the analysis, organized by thematic category with German names and English translations.",
          style = "color: #E0E0E0;"),
        DT::dataTableOutput("tbl_inkar")
      )
    )
  )
)

#### Server ####

server <- function(input, output, session) {

  # Reactive: filtered sf data for map
  filtered_sf <- reactive({
    sf_data %>% filter(hvm_label == input$mode)
  })

  # Reactive: data for selected district
  district_data <- reactive({
    df_combined %>% filter(ags5 == input$district)
  })

  # Girafe output container
  output$map_girafe_ui <- renderUI({
    girafeOutput("map_girafe", height = "650px", width = "100%")
  })

  # Parse metric pair into left/right fill variables and scale types
  metric_pair <- reactive({
    switch(input$metric,
      "FH_CV"     = list(left = "FH",     left_scale = "FH",     left_title = "FH Estimate",
                         right = "CV_FH_pct", right_scale = "CV_FH_pct", right_title = "CV (FH)"),
      "FH_RV"     = list(left = "FH",     left_scale = "FH",     left_title = "FH Estimate",
                         right = "RV",        right_scale = "RV",        right_title = "Efficiency Gain"),
      "Direct_CV" = list(left = "Direct", left_scale = "Direct", left_title = "Direct Estimate",
                         right = "CV_HTH_pct", right_scale = "CV_HTH_pct", right_title = "CV (Direct)")
    )
  })

  # Output: interactive choropleth map (always 2 maps side by side)
  output$map_girafe <- renderGirafe({
    req(input$metric, input$mode, input$district)
    mp <- metric_pair()

    mode_sf <- filtered_sf()
    map_left <- create_map(mode_sf, mp$left, mp$left_scale,
                           mp$left_title,
                           highlight_ags5 = input$district, tag = "(a)")
    map_right <- create_map(mode_sf, mp$right, mp$right_scale,
                            mp$right_title,
                            highlight_ags5 = input$district, tag = "(b)")
    combined <- (map_left + map_right) &
      theme(plot.background = element_rect(fill = bg_color, color = NA))
    combined <- combined + plot_annotation(
      theme = theme_void() + theme(
        plot.background = element_rect(fill = bg_color, color = NA),
        plot.margin = margin(0, 0, 0, 0)
      )
    )

    girafe(
      ggobj = combined,
      bg = bg_color,
      options = list(
        opts_tooltip(css = paste0("background:", panel_color, ";color:", text_color, ";padding:8px;border-radius:4px;border:1px solid ", accent_color, ";font-size:13px;")),
        opts_hover(css = paste0("fill:", accent_color, ";stroke:", text_color, ";stroke-width:1.5;")),
        opts_sizing(rescale = TRUE, width = 1),
        opts_toolbar(saveaspng = FALSE)
      )
    )
  })

  # Output: summary table for selected district
  output$summary_table <- renderTable({
    d <- district_data()
    if (nrow(d) == 0) return(NULL)

    d %>%
      mutate(Mode = mode_labels[hvm_label]) %>%
      transmute(
        Mode,
        `FH (km/day)` = round(FH, 2),
        `Direct (km/day)` = round(Direct, 2),
        `CV FH (%)` = round(CV_FH_pct, 1),
        `CV Direct (%)` = round(CV_HTH_pct, 1),
        `Eff. Gain` = round(RV, 2)
      )
  }, striped = TRUE, hover = TRUE, spacing = "s")

  # Output: reference tables
  output$tbl_varset <- DT::renderDataTable({
    DT::datatable(
      df_varset,
      colnames = c("Variable" = "raw_name", "Type" = "var_type",
                    "Source" = "source", "Base Variable" = "base_var",
                    "Description" = "description"),
      options = list(pageLength = 25, scrollX = TRUE),
      filter = "top", rownames = FALSE, style = "bootstrap5"
    )
  })

  output$tbl_destatis <- DT::renderDataTable({
    DT::datatable(
      df_destatis,
      colnames = c("Table Code" = "table_code", "Category" = "category",
                    "Description (DE)" = "description_de",
                    "Description (EN)" = "description_en"),
      options = list(pageLength = 40, scrollX = TRUE),
      filter = "top", rownames = FALSE, style = "bootstrap5"
    )
  })

  output$tbl_inkar <- DT::renderDataTable({
    DT::datatable(
      df_inkar,
      colnames = c("#" = "row_num", "German Name" = "german_name",
                    "English Name" = "english_name", "Theme" = "theme_name",
                    "Sub-Theme" = "theme_sub", "Year" = "year_used"),
      options = list(pageLength = 25, scrollX = TRUE),
      filter = "top", rownames = FALSE, style = "bootstrap5"
    )
  })

}

#### Run App ####
shinyApp(ui = ui, server = server)
