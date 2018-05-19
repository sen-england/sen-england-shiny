# ==== Global components ====
controls_global <- p(
  checkboxGroupInput(
    inputId = "global_phase", label = "Education phase",
    choices = cand_phases, selected = cand_phases,
    width = "100%"),
  checkboxGroupInput(
    inputId = "global_sen_type", label = "SEN type",
    choices = cand_sen_type, selected = cand_sen_type,
    width = "100%"))

# ==== Primary components ====
panel_primary_output <- box(
  plotlyOutput("primary_plot"))
controls_primary <- box(
  title = "Settings", collapsible = TRUE, solidHeader = TRUE,
  status = "warning",
  checkboxGroupInput(
    inputId = "primary_years", label = "Select Years",
    choices = cand_years,
    selected = cand_years,
    width = "100%"))
panel_primary <- fluidPage(panel_primary_output,
                           controls_primary)

# ==== tseries components ===
panel_tseries_output <- box(
  plotlyOutput("tseries_plot"))
controls_tseries <- box(
  title = "Settings", collapsible = TRUE, solidHeader = TRUE,
  status = "warning",
  selectInput(
    inputId = glue("tseries_type"), label = "Choose type",
    choices = cand_types,
    selected = "Academisation"),
  checkboxGroupInput(
    inputId = "tseries_years", label = "Select Years",
    choices = cand_years, selected = cand_years,
    width = "100%"))
panel_tseries <- fluidPage(panel_tseries_output,
                           controls_tseries)

# ==== maps components ====
maps_controls <- function(prefix = "maps_a", name = "A",
                          selected_type = "Academisation",
                          selected_region = "E12000009",
                          selected_year = 2015L) {
  box(title = "Settings", width = 12, collapsible = TRUE,
      solidHeader = TRUE,
      column(4,
             actionButton(
               inputId = glue("{prefix}_render"),
               label = glue("Render map"),
               icon = icon("map")),
             selectInput(
               inputId = glue("{prefix}_type"),
               label = "Choose map type",
               choices = cand_types,
               selected = selected_type)),
      column(4,
             selectInput(
               inputId = glue("{prefix}_region"),
               label = "Choose region(s)",
               choices = cand_region,
               selected = selected_region,
               multiple = TRUE),
             checkboxInput(
               inputId = glue("{prefix}_whole_country"),
               label = "Show all England (slow)",
               value = FALSE)),
      column(4,
             selectInput(
               inputId = glue("{prefix}_year"),
               label = "Select year to show",
               choices = cand_years,
               selected = selected_year)))
}

maps_ui <- function(prefix = "maps_a", name = "A",
                    selected_type = "Academisation",
                    box_status = "primary") {
  box(title = glue("Map {name}"), solidHeader = TRUE,
      status = box_status,
      verticalLayout(
        maps_controls(prefix, name, selected_type = selected_type),
        leafletOutput(prefix, height = params$maps_gen$height)))
}

panel_maps <- fluidPage(uiOutput("maps_a_ui"),
                        uiOutput("maps_b_ui"))
