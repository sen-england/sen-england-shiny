message(sprintf("%s, start loading packages", Sys.time()))
library("tidyverse")
library("magrittr")
library("glue")
library("here")
library("shiny")
library("shinydashboard")
options(stringsAsFactors = FALSE)

source("libs/common.R", local = TRUE)
source("libs/render_value_box.R", local = TRUE)
source("libs/render_primary.R", local = TRUE)
source("libs/render_tseries.R", local = TRUE)
source("libs/render_maps.R", local = TRUE)

# ==== Assets ====
message(glue("{Sys.time()}, start loading assets"))
data_conf <- config::get("data")
send_db_conf <- data_conf$send_db
preproc_conf <- data_conf$preprocess
params <- config::get("params")

# Load the datasets
send_db_conn <- DBI::dbConnect(
  RSQLite::SQLite(),
  dbname = here(send_db_conf$db))
preproc_db_conn <- DBI::dbConnect(
  RSQLite::SQLite(),
  dbname = here(preproc_conf$db))
df_main_table <- send_db_conn %>%
  tbl(send_db_conf$tbl) %>%
  select(one_of(send_db_conf$vars))
df_preproc_stats_sen <- preproc_db_conn %>%
  tbl(preproc_conf$stats_sen)
df_preproc_stats_schools <- preproc_db_conn %>%
  tbl(preproc_conf$stats_schools)

# Variables and candidates
dsb_id_primary <- "primary"
dsb_id_tseries <- "tseries"
dsb_id_maps <- "maps"
cand_years <- 2011L:2017L
cand_types <- c("% academised schools" = "Academisation",
                "% pupils with SEN" = "SEN")
cand_phases <- c("Primary schools" = "primary",
                 "Secondary schools" = "secondary",
                 "Others (e.g. nursery, 16 Plus, etc)" = "others")
cand_type_sen <- c("SEN_Support", "Statement_EHC_Plan") %>%
  set_names(str_replace_all(., "_", " "))
cand_type_schools <- c("Mainstream School" = "mainstream school",
                       "Pupil Referral Unit" = "pupil referral unit",
                       "Special School" = "special school",
                       "Others (e.g. independent school)" = "others")
cand_la_tbl <- read_csv(here("data/region-info/region-info.csv"),
                        col_types = c("cccc"))
cand_la <- cand_la_tbl %>%
  select(LAName, LACode) %>% distinct() %>% deframe()
cand_region <- cand_la_tbl %>%
  select(RegionName, RegionCode) %>% distinct() %>% deframe()
cand_parlcon <- read_csv(here("data/region-info/parlcon-info.csv"),
                         col_types = c("cc")) %>% deframe()

message(glue("{Sys.time()}, finished loading assets"))

# ==== UIs ====
source("libs/ui_components.R", local = TRUE)
source("libs/ui_dashboard.R", local = TRUE)

# ==== Server ====

server <- function(input, output) {

  # ---- Deferred loading of assets ----
  england_la <- reactive({
    data_conf$england_la %>% rgdal::readOGR(verbose = FALSE)
  })
  england_parlcon <- reactive({
    data_conf$england_parlcon %>% rgdal::readOGR(verbose = FALSE)
  })

  # ---- global params ----
  df_main <- reactive({
    req(input$global_phase, input$global_type_sen)
    df_main_table %>%
      filter(Phase %in% input$global_phase) %>%
      filter(TypeGeneral %in% input$global_type_schools)
  })
  df_preproc_stats_sen <- reactive({
    req(input$global_phase, input$global_type_sen)
    preproc_db_conn %>%
      tbl(preproc_conf$stats_sen) %>%
      filter(Phase %in% input$global_phase) %>%
      filter(TypeGeneral %in% input$global_type_schools)
  })
  df_preproc_stats_schools <- reactive({
    req(input$global_phase, input$global_type_sen)
    preproc_db_conn %>%
      tbl(preproc_conf$stats_schools) %>%
      filter(Phase %in% input$global_phase) %>%
      filter(TypeGeneral %in% input$global_type_schools)
  })
  df_preproc_composition_schools <- reactive({
    req(input$global_phase, input$global_type_sen)
    preproc_db_conn %>%
      tbl(preproc_conf$composition_schools) %>%
      filter(Phase %in% input$global_phase) %>%
      filter(TypeGeneral %in% input$global_type_schools)
  })
  df_preproc_composition_sen <- reactive({
    req(input$global_phase, input$global_type_sen)
    preproc_db_conn %>%
      tbl(preproc_conf$composition_sen) %>%
      filter(Phase %in% input$global_phase) %>%
      filter(TypeGeneral %in% input$global_type_schools)
  })

  # ---- primary components ----
  # stats
  # pupils
  output$primary_box_total_pupils <- renderValueBox(
    render_box_total_pupils(
      df = df_preproc_stats_sen()))
  output$primary_box_total_sen <- renderValueBox(
    render_box_sen(
      df = df_preproc_stats_sen(),
      sen_type = input$global_type_sen,
      value_type = "total_number"))
  output$primary_box_pct_sen <- renderValueBox(
    render_box_sen(
      df = df_preproc_stats_sen(),
      sen_type = input$global_type_sen,
      value_type = "percent"))
  # schools
  output$primary_box_total_schools <- renderValueBox(
    render_box_total_schools(
      df = df_preproc_stats_schools()))
  output$primary_box_ca <- renderValueBox(
    render_box_by_route(df = df_preproc_stats_schools(),
                        route = "converter academy"))
  output$primary_box_sa <- renderValueBox(
    render_box_by_route(df = df_preproc_stats_schools(),
                        route = "sponsored academy"))
  # ts plots
  output$primary_academ <- plotly::renderPlotly({
    df_main() %>%
      render_primary_academ(palette = params$academ$palette)
  })
  output$primary_sen <- plotly::renderPlotly({
    df_main() %>%
      render_primary_sen(sen_type = input$global_type_sen,
                               palette = params$sen$palette)
  })
  # bar plots
  output$primary_composition_schools_n <- plotly::renderPlotly({
    df_preproc_composition_schools() %>%
      render_primary_composition_schools(
        pct = FALSE, palette = params$academ$palette)
  })
  output$primary_composition_schools_pct <- plotly::renderPlotly({
    df_preproc_composition_schools() %>%
      render_primary_composition_schools(
        pct = TRUE, palette = params$academ$palette)
  })
  output$primary_composition_sen_n <- plotly::renderPlotly({
    df_preproc_composition_sen() %>%
      render_primary_composition_sen(
        pct = FALSE, palette = params$sen$palette)
  })
  output$primary_composition_sen_pct <- plotly::renderPlotly({
    df_preproc_composition_sen() %>%
      render_primary_composition_sen(
        pct = TRUE, palette = params$sen$palette)
  })

  # ---- tseries components ----
  spawn_tseries <- function(prefix = "tseries_a", type = "Academisation") {
    plotly::ggplotly(render_tseries(
      # years = input[[glue("{prefix}_years")]],
      years = input$tseries_years,
      df = df_main(),
      type = type,
      sen_type = input$global_type_sen,
      scales_free = input$tseries_scales_free,
      facetted = input$tseries_facetted,
      geo_level = input$tseries_geo_level,
      region = input$tseries_region,
      LA = input$tseries_la,
      parlcon = input$tseries_parlcon))
  }
  output$tseries_a <- plotly::renderPlotly(spawn_tseries("tseries_a",
                                                 "Academisation"))
  output$tseries_b <- plotly::renderPlotly(spawn_tseries("tseries_b",
                                                 "SEN"))

  # ---- maps components ----
  get_status_color <- function(type) {
    if (type == "Academisation") {
      "warning"
    } else if (type == "SEN") {
      "success"
    } else {
      "warning"
    }
  }
  get_params_maps <- function(prefix) {
    whole_country <- input[[glue("{prefix}_whole_country")]]
    whole_country_enabled <- !is.null(whole_country) && whole_country == TRUE
    list(year = input[[glue("{prefix}_year")]],
         shape = if(input[[glue("{prefix}_geo_level")]] == "LA") {
           england_la()
         } else {
           england_parlcon()
         },
         df = df_main(),
         type = input[[glue("{prefix}_type")]],
         geo_level = input[[glue("{prefix}_geo_level")]],
         sen_type = input$global_type_sen,
         auto_breaks = input[[glue("{prefix}_auto_breaks")]])
  }
  spawn_maps <- function(prefix) {
    eventReactive(input[[glue("{prefix}_render")]], {
      if (input[[glue("{prefix}_render")]] > 0) {
        withProgress(
          do.call(render_map, get_params_maps(prefix)),
          message = "Rendering maps...")
      } else {
        leaflet::leaflet() %>%
          leaflet::addTiles() %>%
          # Q-Step Building
          leaflet::setView(lng = params$maps_gen$default_lng,
                           lat = params$maps_gen$default_lat,
                           zoom = params$maps_gen$default_zoom)
      }
    },
    ignoreNULL = FALSE)
  }

  maps_a_ui_status <- reactiveVal("primary")
  maps_b_ui_status <- reactiveVal("primary")
  output$maps_a <- leaflet::renderLeaflet(spawn_maps("maps_a")())
  output$maps_b <- leaflet::renderLeaflet(spawn_maps("maps_b")())
  output$maps_a_ui <- renderUI(maps_ui_single(
    "maps_a", "A", "Academisation",
    box_status = maps_a_ui_status(),
    dual_map = input$global_maps_dual))
  output$maps_b_ui <- renderUI(maps_ui_single(
    "maps_b", "B", "SEN",
    box_status = maps_b_ui_status(),
    dual_map = input$global_maps_dual))
  output$maps <- renderUI(maps_ui(input$global_maps_dual))

}

shinyApp(ui = dsb_ui, server = server)
