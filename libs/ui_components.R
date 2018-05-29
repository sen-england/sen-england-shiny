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
    inputId = "global_type_schools", label = "Type of schools",
    choices = cand_type_schools, selected = cand_type_schools,
    width = "100%"),
  checkboxGroupInput(
    inputId = "global_type_sen", label = "Type of special educational needs",
    choices = cand_type_sen, selected = cand_type_sen,
    width = "100%"))

# ==== Primary components ====
docs_primary <- div(
  h4("Primary Dashboard:"),
  fmt_html("User instructions goes here"),
  fmt_html(
    "Mauris ac felis vel velit tristique imperdiet.",
    "Nunc porta vulputate tellus.  ",
    "Sed bibendum.  ",
    "Nam a sapien.  "))
panel_primary <- fluidRow(
  column(width = 8,
         box(
           width = NULL,
           column(width = 6,
                  valueBoxOutput("primary_box_total_pupils",
                                 width = NULL),
                  valueBoxOutput("primary_box_total_sen",
                                 width = NULL),
                  valueBoxOutput("primary_box_pct_sen",
                                 width = NULL)) ,
           column(width = 6,
                  valueBoxOutput("primary_box_total_schools",
                                 width = NULL),
                  valueBoxOutput("primary_box_ca",
                                 width = NULL),
                  valueBoxOutput("primary_box_sa",
                                 width = NULL))),
         tabBox(
           title = "Composition of schools",
           tabPanel(
             title = "Relative percentages",
             plotlyOutput("primary_composition_pct")),
           tabPanel(
             title = "Absolute numbers",
             plotlyOutput("primary_composition_n")),
           width = NULL)),
  column(width = 4,
         box(plotlyOutput("primary_academ"),
             width = NULL),
         box(plotlyOutput("primary_sen"),
             width = NULL))
)

# ==== tseries components ===
docs_tseries <- div(
  h4("Time series trends:"),
  fmt_html("User instructions goes here"),
  fmt_html(
    "Cum sociis natoque penatibus et",
    "magnis dis parturient montes, nascetur ridiculus mus."))
controls_tseries <- box(
  title = "Settings", collapsible = TRUE, solidHeader = TRUE,
  status = "warning", width = 2,
  checkboxInput(
    inputId = "tseries_facetted", label = "Factted by school types",
    value = TRUE),
  checkboxInput(
    inputId = "tseries_scales_free", label = "Flexible scales",
    value = FALSE),
  checkboxGroupInput(
    inputId = "tseries_regions", label = "Choose region(s)",
    choices = cand_region,
    selected = cand_region),
  sliderInput(
    inputId = "tseries_years", label = "Select Years",
    round = TRUE, step = 1L,
    min = min(cand_years), max = max(cand_years),
    value = c(min(cand_years), max(cand_years))))
panel_tseries <- fluidRow(
  box(plotlyOutput("tseries_a"),
      plotlyOutput("tseries_b"),
      width = 10),
  controls_tseries)

# ==== maps components ====
docs_maps <- div(
  h4("Regional breakdowns:"),
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
