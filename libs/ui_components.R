# ==== Global components ====
docs_global <- div()
docs_info <- div(
  h6("Project website: ", a(href = "https://sen-england.github.io",
                            "Github Pages")),
  h6("Usage tutorials: ", a(href = params$docs$site,
                            "Documentation")),
  h6("Developed: ", a(href = "https://github.com/YiLiu6240",
                      "Dr Yi Liu")),
  h6("Data sources: ",
     format_markdown("

- [DfE Statistics: SEND](https://www.gov.uk/government/collections/statistics-special-educational-needs-sen);
- [UK Data Service: Boundary Datasets](https://borders.ukdataservice.ac.uk/easy_download.html)
")))
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
    inputId = "global_type_sen", label = "Type of SEN",
    choices = cand_type_sen, selected = cand_type_sen,
    width = "100%"),
  checkboxInput(
    inputId = "global_maps_dual", label = "Render a second map",
    value = FALSE))

# ==== Primary components ====
docs_primary <- box(
  title = tagList(icon("tachometer"), params$tabs$primary),
  collapsible = TRUE, solidHeader = TRUE, collapsed = FALSE,
  status = "success", width = NULL,
  format_markdown(glue("
General overview regarding the academisation of English schools
and the inclusion of pupils of special educational needs.

Change options in `Global settings` in the sidebar to update the charts.

For tutorials please see the [documentation]({params$docs$site}).

Press the ` - ` button in the upper right hand to collapse this widget.
")))
panel_primary <- fluidRow(
  column(width = 8,
         box(
           title = "Statistics as of year 2017",
           width = NULL, collapsible = TRUE,
           solidHeader = TRUE, status = "primary",
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
           title = "Composition",
           tabPanel(
             title = "schools: percentages",
             plotly::plotlyOutput("primary_composition_schools_pct")),
           tabPanel(
             title = "schools: numbers",
             plotly::plotlyOutput("primary_composition_schools_n")),
           tabPanel(
             title = "SEN: percentages",
             plotly::plotlyOutput("primary_composition_sen_pct")),
           tabPanel(
             title = "SEN: numbers",
             plotly::plotlyOutput("primary_composition_sen_n")),
           width = NULL)),
  column(width = 4,
         docs_primary,
         box(width = NULL, collapsible = TRUE,
             solidHeader = TRUE, status = "primary",
             title = "Percentage of academised schools",
             plotly::plotlyOutput("primary_academ")),
         box(width = NULL, collapsible = TRUE,
             solidHeader = TRUE, status = "primary",
             title = "Percentage of pupils with SEN",
             plotly::plotlyOutput("primary_sen")))
)

# ==== tseries components ====
docs_tseries <- box(
  title = tagList(icon("line-chart"), params$tabs$tseries),
  collapsible = TRUE, solidHeader = TRUE, collapsed = FALSE,
  status = "success", width = NULL,
  format_markdown("
Trends of academistion and educational inclusiveness.

Adjust the settings to your preference.
"))
controls_tseries <- box(
  title = "Settings", collapsible = FALSE, solidHeader = TRUE,
  status = "warning", width = NULL,
  checkboxInput(
    inputId = "tseries_facetted", label = "Factted by school types",
    value = TRUE),
  checkboxInput(
    inputId = "tseries_scales_free", label = "Flexible scales",
    value = FALSE),
  radioButtons(
    inputId = "tseries_geo_level",
    label = "Choose geographical level",
    choices = c("England" = "whole_country",
                "Region level" = "region",
                "Local Authority level" = "LA",
                "Parliamentary Constituency level" = "parlcon"),
    selected = "whole_country"),
  selectInput(
    inputId = "tseries_region",
    label = "Choose region",
    choices = cand_region,
    selected = "E12000009",
    multiple = FALSE),
  selectInput(
    inputId = "tseries_la",
    label = "Choose local authority",
    choices = cand_la,
    # Devon
    selected = "E10000008",
    multiple = FALSE),
  selectInput(
    inputId = "tseries_parlcon",
    label = "Choose parliamentary constituency",
    choices = cand_parlcon,
    # East Devon
    selected = "E14000678",
    multiple = FALSE),
  sliderInput(
    inputId = "tseries_years", label = "Select Years",
    round = TRUE, step = 1L,
    min = min(cand_years), max = max(cand_years),
    value = c(min(cand_years), max(cand_years))))
panel_tseries <- fluidRow(
  column(width = 8,
         box(plotly::plotlyOutput("tseries_a"),
             plotly::plotlyOutput("tseries_b"),
             width = NULL)),
  column(width = 4,
         docs_tseries,
         controls_tseries))

# ==== maps components ====
docs_maps <- box(
  title = tagList(icon("map"), params$tabs$maps),
  collapsible = TRUE, solidHeader = TRUE, collapsed = FALSE,
  status = "success", width = NULL,
  format_markdown("
Press the `Render map` button to (re-)generate a map.

Option `Optimal scales` will select scales best suited for the individual map,
otherwise common scales will be set to better track changes between maps.

By default the only one region is displayed (override with `Show all England`).
"))
maps_controls <- function(prefix = "maps_a", name = "A",
                          selected_type = "Academisation",
                          selected_region = "E12000009",
                          selected_year = 2017L,
                          dual_map = TRUE,
                          width = 12) {
  # Widgets
  widget_button <- actionButton(
    inputId = glue("{prefix}_render"),
    label = glue("Render map"),
    icon = icon("map"))
  widget_type <- radioButtons(
    inputId = glue("{prefix}_type"),
    label = "Choose map type",
    choices = cand_types,
    selected = selected_type)
  widget_year <- selectInput(
    inputId = glue("{prefix}_year"),
    label = "Select year to show",
    choices = cand_years,
    selected = selected_year)
  widget_breaks <- checkboxInput(
    inputId = glue("{prefix}_auto_breaks"),
    label = "Optimal scales",
    value = FALSE)
  widget_geo_level <- radioButtons(
    inputId = glue("{prefix}_geo_level"),
    label = "Choose geographical level",
    choices = c("Local authority level" = "LA",
                "Parliamentary constituency level" = "ParlCon"),
    selected = "LA")

  # Layouts
  layout_wide <- box(
    title = "Settings", width = width, collapsible = TRUE,
    solidHeader = TRUE,
    column(4, widget_button, widget_type),
    column(4, widget_geo_level),
    column(4, widget_year, widget_breaks))
  layout_long <- box(
    title = "Settings", width = width, collapsible = FALSE,
    solidHeader = TRUE,
    status = "warning",
    widget_button, widget_type, widget_geo_level,
    widget_year, widget_breaks
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
                          dual_map = TRUE),
            leaflet::leafletOutput(prefix, height = params$maps_gen$height)))
  } else {
    fluidRow(
      column(
        width = 8,
        leaflet::leafletOutput(prefix, height = params$maps_gen$height)),
      column(
        width = 4,
        docs_maps,
        maps_controls(prefix, name,
                      selected_type = selected_type,
                      dual_map = FALSE,
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
