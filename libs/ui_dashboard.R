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
    dsb_sidebar_tseries <- menuItem(
      "Time series trends", tabName = dsb_id_tseries,
      icon = icon("line-chart")),
    menuItem(
      "Compare maps", tabName = dsb_id_maps,
      icon = icon("map")),
    h3(icon("cog", lib = "glyphicon"), "Global settings"),
    controls_global,
    h3(icon("cogs"), "Topic settings"),
    conditionalPanel(
      glue("input.tabs == '{dsb_id_primary}'"),
      controls_primary),
    conditionalPanel(
      glue("input.tabs == '{dsb_id_tseries}'"),
      controls_tseries)))

# ==== Main tabs ====

dsb_main <- dashboardBody(
  tabItems(tabItem(tabName = dsb_id_primary,
                   panel_primary_output),
           tabItem(tabName = dsb_id_tseries,
                   panel_tseries_output),
           tabItem(tabName = dsb_id_maps,
                   panel_maps)))

# ==== Aggregate ====

dsb_ui <- dashboardPage(dsb_header,
                        dsb_sidebar,
                        dsb_main,
                        skin = "black")
