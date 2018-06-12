# Namespace: `dsb_`
# NOTE: font-awesome icons are version 4

# ==== Individual components ====
dashboardHeaderHack <- function (..., title = NULL, longtitle = NULL,
                                 disable = FALSE, .list = NULL) {
  items <- c(list(...), .list)
  lapply(items, shinydashboard:::tagAssert, type = "li", class = "dropdown")
  tags$header(
    class = "main-header",
    style = if (disable) "display: none;",
    span(class = "logo", title),
    tags$nav(class = "navbar navbar-static-top",
             role = "navigation",
             span(shiny::icon("bars"),
                  style = "display:none;"),
             a(href = "#", class = "sidebar-toggle",
               `data-toggle` = "offcanvas", role = "button",
               span(class = "sr-only", "Toggle navigation")),
             div(class = "longtitle", longtitle),
             div(class = "navbar-custom-menu",
                 tags$ul(class = "nav navbar-nav",
                         items))))
}
dsb_header <- dashboardHeaderHack(
  title = "SEN & academisation",
  longtitle = "Inclusion and the academisation of English schools")
dsb_id_primary <- "primary"
dsb_id_tseries <- "tseries"
dsb_id_maps <- "maps"

# ==== Sidebar ====

dsb_sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    h3(icon("list-alt", lib = "glyphicon"), "Topics"),
    menuItem(
      params$tabs$primary, tabName = dsb_id_primary,
      icon = icon("tachometer")),
    menuItem(
      params$tabs$tseries, tabName = dsb_id_tseries,
      icon = icon("line-chart")),
    menuItem(
      params$tabs$maps, tabName = dsb_id_maps,
      icon = icon("map")),
    menuItem(
      params$tabs$global, icon = icon("cog", lib = "glyphicon"),
      startExpanded = TRUE,
      controls_global),
    docs_global,
    docs_info))

# ==== Main tabs ====

dsb_main <- dashboardBody(
  tags$head(tags$link(rel = "stylesheet",
                      type = "text/css", href = "custom.css")),
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
