# ==== Global components ====
controls_global <- p(
  checkboxGroupInput(
    inputId = "global_phase", label = "Education phase",
    choices = cand_phases,
    selected = cand_phases,
    width = "100%"),
  checkboxGroupInput(
    inputId = "global_sen_type", label = "SEN type",
    choices = cand_sen_type,
    selected = cand_sen_type,
    width = "100%"))

# ==== Primary components ====
panel_primary_output <- fluidRow(
  plotlyOutput("primary_plot"))
controls_primary <- p(
  checkboxGroupInput(
    inputId = "primary_years", label = "Select Years",
    choices = cand_years,
    selected = 2011L:2015L,
    width = "100%"))

# ==== tseries components ===
panel_tseries_output <- fluidRow(
  plotlyOutput("tseries_plot"))
controls_tseries <- p(
  selectInput(
    inputId = glue("tseries_type"), label = "Choose type",
    choices = cand_types,
    selected = "Academisation"),
  checkboxGroupInput(
    inputId = "tseries_years", label = "Select Years",
    choices = cand_years,
    selected = 2011L:2015L,
    width = "100%"))

# ==== maps components ====
maps_input <- function(prefix = "maps_a", name = "A",
                       selected_type = "Academisation",
                       selected_region = "E12000009",
                       selected_year = 2015L) {
  box(title = "Settings", width = 12, collapsible = TRUE,
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
               label = "Show whole England (slow)",
               value = FALSE)),
      column(4,
             selectInput(
               inputId = glue("{prefix}_year"),
               label = "Select year to show",
               choices = cand_years,
               selected = selected_year)))
}

panel_maps <- fluidRow(
  box(title = "Map A", solidHeader = TRUE,
      status = "primary",
      verticalLayout(
        maps_input("maps_a", "A",
                   selected_type = "Academisation"),
        leafletOutput("maps_a",
                      height = params$maps_gen$height))),
  box(title = "Map B", solidHeader = TRUE,
      status = "warning",
      verticalLayout(
        maps_input("maps_b", "B",
                   selected_type = "SEN"),
        leafletOutput("maps_b",
                      height = params$maps_gen$height))))
