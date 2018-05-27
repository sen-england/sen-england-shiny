# ==== Global components ====
docs_global <- div()
docs_info <- div(
  h6("Project website: ", a(href = "", "Github Pages")),
  h6("Developed: ", a(href = "https://github.com/YiLiu6240", "Dr Yi Liu")),
  h6("Data sources: ",
     a(href = "", "SEND"),
     a(href = "", "UK data service")))
controls_global <- p(
  checkboxGroupInput(
    inputId = "global_phase", label = "Education phase",
    choices = cand_phases, selected = cand_phases,
    width = "100%"),
  checkboxGroupInput(
    inputId = "global_type_academy", label = "Academies and other schools",
    choices = cand_type_academy, selected = cand_type_academy,
    width = "100%"),
  checkboxGroupInput(
    inputId = "global_type_sen", label = "SEN type",
    choices = cand_type_sen, selected = cand_type_sen,
    width = "100%"))
controls_global_box <- box(
  title = "Settings", collapsible = TRUE, width = 12,
  solidHeader = FALSE, background = "black",
  checkboxGroupInput(
    inputId = "global_phase", label = "Education phase",
    choices = cand_phases, selected = cand_phases,
    width = "100%"),
  checkboxGroupInput(
    inputId = "global_type_academy", label = "Academies and other schools",
    choices = cand_type_academy, selected = cand_type_academy,
    width = "100%"),
  checkboxGroupInput(
    inputId = "global_type_sen", label = "SEN type",
    choices = cand_type_sen, selected = cand_type_sen,
    width = "100%"))

# ==== Primary components ====
docs_primary <- div(
  h4("Primary Dashboard:"),
  fmt_html(
    "Mauris ac felis vel velit tristique imperdiet.",
    "Nunc porta vulputate tellus.  ",
    "Sed bibendum.  ",
    "Nam a sapien.  "))
panel_primary_output <- box(
  plotlyOutput("primary_plot"))
controls_primary <- box(
  title = "Settings", collapsible = TRUE, solidHeader = TRUE,
  status = "warning",
  checkboxGroupInput(
    inputId = "primary_years", label = "Select Years",
    choices = cand_years, selected = cand_years,
    width = "100%"))
panel_primary <- fluidPage(panel_primary_output,
                           controls_primary)

# ==== tseries components ===
docs_tseries <- div(
  h4("Time series trends:"),
  fmt_html(
    "Cum sociis natoque penatibus et",
    "magnis dis parturient montes, nascetur ridiculus mus."))
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
docs_maps <- div(
  h4("Compare maps:"),
  fmt_html(
    "Press the \"Render map\" button to (re-)generate a map"))
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
