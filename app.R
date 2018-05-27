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
      filter(Phase %in% input$global_phase)
  })

  # ---- primary components ----
  df_primary <- reactive({
    req(input$primary_years)
    df_send() %>% filter(Year %in% input$primary_years) %>%
      count(Year) %>% collect()
  })
  output$primary_plot <- renderPlotly({
    p <- df_primary() %>%
      ggplot(aes(y = Year, x = n)) +
      geom_line() +
      geom_point()
    ggplotly(p)
  })

  # ---- tseries components ----
  output$tseries_plot <- renderPlotly({
    ggplotly(render_tseries(
      year = input$tseries_years,
      df_send = df_send(),
      type = input$tseries_type,
      sen_type = input$global_type_sen))
  })

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
         whole_country = input[[glue("{prefix}_whole_country")]],
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
