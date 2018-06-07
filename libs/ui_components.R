# ==== Global components ====
docs_global <- div()
docs_info <- div(
  h6("Project website: ", a(href = "https://sen-england.github.io",
                            "Github Pages")),
  h6("Developed: ", a(href = "https://github.com/YiLiu6240",
                      "Dr Yi Liu")),
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
    width = "100%"),
  checkboxInput(
    inputId = "global_maps_dual", label = "Render a second map",
    value = FALSE))

# ==== Primary components ====
docs_primary <- div(
  h4(params$tabs$primary),
  fmt_html(
    "General overview regarding the academisation of English schools",
    "and the inclusion of pupils of special educational needs."))
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
  h4(params$tabs$tseries),
  fmt_html(
    "Trends of academistion and educational inclusiveness.",
    "Adjust the settings to your preference."))
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
    inputId = "tseries_regions",
    label = "Choose region(s)",
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
  h4(params$tabs$maps),
  fmt_html(
    "Press the \"Render map\" button to (re-)generate a map"))
maps_controls <- function(prefix = "maps_a", name = "A",
                          selected_type = "Academisation",
                          selected_region = "E12000009",
                          selected_year = 2015L,
                          dual_map = TRUE,
                          whole_country = FALSE,
                          width = 12) {
  widget_button <- actionButton(
    inputId = glue("{prefix}_render"),
    label = glue("Render map"),
    icon = icon("map"))
  widget_type <- selectInput(
    inputId = glue("{prefix}_type"),
    label = "Choose map type",
    choices = cand_types,
    selected = selected_type)
  widget_region <- selectInput(
    inputId = glue("{prefix}_region"),
    label = "Choose region",
    choices = cand_region,
    selected = selected_region,
    multiple = params$maps_gen$`multi-region`)
  widget_whole_country <- checkboxInput(
    inputId = glue("{prefix}_whole_country"),
    label = "Show all England (slow)",
    value = FALSE)
  widget_year <- selectInput(
    inputId = glue("{prefix}_year"),
    label = "Select year to show",
    choices = cand_years,
    selected = selected_year)
  layout_wide <- box(
    title = "Settings", width = width, collapsible = TRUE,
    solidHeader = TRUE,
    column(4, widget_button, widget_type),
    column(4, widget_region,
           if (whole_country) {
             widget_whole_country
           } else {
             NULL
           }),
    column(4, widget_year))
  layout_long <- box(
    title = "Settings", width = width, collapsible = FALSE,
    solidHeader = TRUE,
    status = "warning",
    widget_button, widget_type, widget_region,
    if (whole_country) {
      widget_whole_country
    } else {
      NULL
    },
    widget_year
  )

  if (dual_map) {
    layout_wide
  } else {
    layout_long
  }
}

maps_ui_single <- function(prefix = "maps_a", name = "A",
                           selected_type = "Academisation",
                           box_status = "primary",
                           dual_map = TRUE) {
  if (dual_map) {
    box(title = glue("Map {name}"), solidHeader = TRUE,
        status = box_status,
        if (dual_map)
          verticalLayout(
            maps_controls(prefix, name,
                          selected_type = selected_type,
                          dual_map = TRUE,
                          whole_country = params$maps_gen$`render-england`),
            leafletOutput(prefix, height = params$maps_gen$height)))
  } else {
    fluidRow(
      column(
        width = 8,
        leafletOutput(prefix, height = params$maps_gen$height)),
      column(
        width = 4,
        maps_controls(prefix, name,
                      selected_type = selected_type,
                      dual_map = FALSE,
                      whole_country = params$maps_gen$`render-england`,
                      width = NULL)))
  }
}

maps_ui <- function(dual_map = TRUE) {
  if (dual_map) {
    fluidPage(uiOutput("maps_a_ui"),
              uiOutput("maps_b_ui"))
  } else {
    fluidPage(uiOutput("maps_a_ui"))
  }
}

panel_maps <- uiOutput("maps")
