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

panel_primary_output <- fluidRow(
  plotlyOutput("primary_plot"))
controls_primary <- p(
  checkboxGroupInput(
    inputId = "primary_years", label = "Select Years",
    choices = cand_years,
    selected = 2011L:2015L,
    width = "100%"))

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

widget_maps_output_a_region <- leafletOutput("maps_a_region",
                                             height = params$maps_gen$height)
widget_maps_output_b_region <- leafletOutput("maps_b_region",
                                             height = params$maps_gen$height)
panel_maps_region <- fluidRow(
  box(verticalLayout(
    titlePanel("Map A"),
    widget_maps_output_a_region)),
  box(verticalLayout(
    titlePanel("Map B"),
    widget_maps_output_b_region)))
maps_input_region <- function(prefix = "maps_a_region", name = "A",
                              selected_year = 2015L, selected_type = "Academisation",
                              region = "E12000009") {
  p(
    h4(glue("Map {name}:")),
    selectInput(
      inputId = glue("{prefix}_type"), label = "Choose map type",
      choices = cand_types,
      selected = selected_type),
    selectInput(
      inputId = glue("{prefix}_region"), label = "Choose region(s)",
      choices = cand_region,
      selected = region,
      multiple = TRUE),
    selectInput(
      inputId = glue("{prefix}_year"), label = "Select year to show",
      choices = cand_years,
      selected = selected_year),
    actionButton(glue("{prefix}_render"), glue("Render map {name}"),
                 icon = icon("map")),
    width = 12)
}
controls_maps_region <- p(
  br(),
  maps_input_region("maps_a_region", "A", selected_type = "Academisation"),
  br(),
  maps_input_region("maps_b_region", "B", selected_type = "SEN"))

widget_maps_output_a <- leafletOutput("maps_a",
                                      height = params$maps_gen$height)
widget_maps_output_b <- leafletOutput("maps_b",
                                      height = params$maps_gen$height)
panel_maps <- fluidRow(
  box(verticalLayout(
    titlePanel("Map A"),
    widget_maps_output_a)),
  box(verticalLayout(
    titlePanel("Map B"),
    widget_maps_output_b)))
maps_input <- function(prefix = "maps_a", name = "A",
                       selected_year = 2015L, selected_type = "Academisation") {
  p(
    h4(glue("Map {name}:")),
    selectInput(
      inputId = glue("{prefix}_type"), label = "Choose map type",
      choices = cand_types,
      selected = selected_type),
    selectInput(
      inputId = glue("{prefix}_year"), label = "Select year to show",
      choices = cand_years,
      selected = selected_year),
    actionButton(glue("{prefix}_render"), glue("Render map {name}"),
                 icon = icon("map")),
    width = 12)
}
controls_maps <- p(
  br(),
  maps_input("maps_a", "A", selected_type = "Academisation"),
  br(),
  maps_input("maps_b", "B", selected_type = "SEN"))
