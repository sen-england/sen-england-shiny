library("tidyverse")
library("glue")
library("DBI")
library("shiny")
library("shinythemes")
library("shinydashboard")
library("leaflet")
library("plotly")
library("rgdal")
library("tmap")
options(stringsAsFactors = FALSE)

source("libs/common.R", local = TRUE)
source("libs/render.R", local = TRUE)
source("libs/render_value_box.R", local = TRUE)

# ==== Assets ====
message(glue("{Sys.time()}, start loading assets"))
data_conf <- config::get("data")
send_db_conf <- data_conf$send_db
params <- config::get("params")

# Load the datasets
send_db_conn <- dbConnect(
  RSQLite::SQLite(),
  # odbc::odbc(),
  # driver = send_db_conf$driver,
  dbname = send_db_conf$db)
df_send_lazy <- send_db_conn %>% tbl(send_db_conf$tbl) %>%
  select(one_of(send_db_conf$vars))
# Shapefiles, LA level and region level
england_la <- data_conf$england_la %>% readOGR(verbose = TRUE)
england_region <- data_conf$england_region %>% readOGR(verbose = TRUE)

# Candidates
cand_years <- df_send_lazy %>% pull(Year) %>% unique()
cand_types <- c("Academisation", "SEN")
cand_phases <- df_send_lazy %>% pull(Phase) %>% unique() %>%
  set_names(str_to_title(.))
cand_type_sen <- c("SEN_Support", "Statement_EHC_Plan") %>%
  set_names(str_replace_all(., "_", " "))
cand_type_schools <- df_send_lazy %>% pull(TypeGeneral) %>% unique() %>%
  fct_relevel("others", after = Inf) %>%
  sort() %>% as.character() %>%
  set_names(str_to_title(.))
cand_la_tbl <- df_send_lazy %>%
  select(RegionCode, LACode) %>% distinct() %>% collect() %>%
  left_join(england_region@data %>%
              select(code, name) %>%
              rename(RegionName = name),
            by = c("RegionCode" = "code")) %>%
  left_join(england_la@data %>%
              select(code, name) %>%
              rename(LAName = name),
            by = c("LACode" = "code"))
cand_region_tbl <- cand_la_tbl %>%
  select(RegionCode, RegionName) %>% distinct()
cand_region <- cand_region_tbl %>% select(RegionName, RegionCode) %>%
  deframe()

message(glue("{Sys.time()}, finished loading assets"))

# ==== UIs ====
source("libs/ui_components.R", local = TRUE)
source("libs/ui_dashboard.R", local = TRUE)

# ==== Server ====

server <- function(input, output) {
  # ---- global params ----
  df_send <- reactive({
    req(input$global_phase, input$global_type_sen)
    df_send_lazy %>%
      filter(Phase %in% input$global_phase) %>%
      filter(TypeGeneral %in% input$global_type_schools)
  })

  # ---- primary components ----
  output$primary_academ <- renderPlotly(
    ggplotly(render_primary_academ(
      df_send = df_send(), palette = params$academ$palette)))
  output$primary_sen <- renderPlotly(
    ggplotly(render_primary_sen(
      df_send = df_send(), sen_type = input$global_type_sen,
      palette = params$sen$palette)))
  output$primary_composition_n <- renderPlotly(
    ggplotly(render_primary_composition(
      df_send = df_send(), pct = FALSE,
      palette = params$academ$palette)))
  output$primary_composition_pct <- renderPlotly(
    ggplotly(render_primary_composition(
      df_send = df_send(), pct = TRUE,
      palette = params$academ$palette)))
  output$primary_box_total_pupils <- renderValueBox(
    render_box_total_pupils(
      df_send(), input$global_type_sen))
  output$primary_box_total_sen <- renderValueBox(
    render_box_total_sen(
      df_send(), input$global_type_sen))
  output$primary_box_pct_sen <- renderValueBox(
    render_box_pct_sen(
      df_send(), input$global_type_sen))

  output$primary_box_total_schools <- renderValueBox(
    render_box_total_schools(df_send()))
  output$primary_box_ca <- renderValueBox(
    render_box_ca(df_send()))
  output$primary_box_sa <- renderValueBox(
    render_box_sa(df_send()))

  # ---- tseries components ----
  spawn_tseries <- function(prefix = "tseries_a", type = "Academisation") {
    ggplotly(render_tseries(
      # years = input[[glue("{prefix}_years")]],
      years = input$tseries_years,
      df_send = df_send(),
      type = type,
      sen_type = input$global_type_sen,
      scales_free = input$tseries_scales_free,
      facetted = input$tseries_facetted,
      regions = input$tseries_regions))
  }
  output$tseries_a <- renderPlotly(spawn_tseries("tseries_a",
                                                 "Academisation"))
  output$tseries_b <- renderPlotly(spawn_tseries("tseries_b",
                                                 "SEN"))

  # ---- maps components ----
  get_status_color <- function(type) {
    if (type == "Academisation") {
      "warning"
    } else if (type == "SEN") {
      "success"
    } else {
      "primary"
    }
  }
  get_params_maps <- function(prefix) {
    list(year = input[[glue("{prefix}_year")]],
         type = input[[glue("{prefix}_type")]],
         region = input[[glue("{prefix}_region")]],
         whole_country = if (params$maps_gen$`render-england`) {
           input[[glue("{prefix}_whole_country")]]
         } else {
           FALSE
         },
         shape = england_la,
         df_send = df_send(),
         sen_type = input$global_type_sen)
  }
  spawn_maps <- function(prefix) {
    eventReactive(input[[glue("{prefix}_render")]], {
      if (input[[glue("{prefix}_render")]] > 0) {
        do.call(render_map, get_params_maps(prefix))
      } else {
        leaflet() %>% addTiles()
      }
    }, ignoreNULL = FALSE)
  }

  maps_a_ui_status <- reactiveVal("primary")
  maps_b_ui_status <- reactiveVal("primary")
  output$maps_a <- renderLeaflet(spawn_maps("maps_a")())
  output$maps_b <- renderLeaflet(spawn_maps("maps_b")())
  output$maps_a_ui <- renderUI(maps_ui("maps_a", "A", "Academisation",
                                       maps_a_ui_status()))
  output$maps_b_ui <- renderUI(maps_ui("maps_b", "B", "SEN",
                                       maps_b_ui_status()))

}

shinyApp(ui = dsb_ui, server = server)
