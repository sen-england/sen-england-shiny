# Namespace: `dsb_`
# NOTE: font-awesome icons are version 4

# ==== Individual components ====
dsb_header <- dashboardHeader(title = "SEN England")
dsb_id_primary <- "primary"
dsb_id_tseries <- "tseries"
dsb_id_maps <- "maps"

# ==== Sidebar ====

dsb_sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    h3(icon("list-alt", lib = "glyphicon"), "Topics"),
    menuItem(
      "Primary dashboard", tabName = dsb_id_primary,
      icon = icon("tachometer")),
    menuItem(
      "Time series trends", tabName = dsb_id_tseries,
      icon = icon("line-chart")),
    menuItem(
      "Breakdown by region", tabName = dsb_id_maps,
      icon = icon("map")),
    menuItem(
      "Global settings", icon = icon("cog", lib = "glyphicon"),
      startExpanded = TRUE,
      controls_global),
    br(),
    docs_global,
    conditionalPanel(
      glue("input.tabs == '{dsb_id_primary}'"),
      docs_primary),
    conditionalPanel(
      glue("input.tabs == '{dsb_id_tseries}'"),
      docs_tseries),
    conditionalPanel(
      glue("input.tabs == '{dsb_id_maps}'"),
      docs_maps),
    br(),
    docs_info))

# ==== Main tabs ====

dsb_main <- dashboardBody(
  tabItems(tabItem(tabName = dsb_id_primary,
                   panel_primary),
           tabItem(tabName = dsb_id_tseries,
                   panel_tseries),
           tabItem(tabName = dsb_id_maps,
                   panel_maps)))

# ==== Aggregate ====

dsb_ui <- dashboardPage(dsb_header,
                        dsb_sidebar,
                        dsb_main,
                        skin = "black")
