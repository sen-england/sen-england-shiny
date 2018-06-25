summarise_map_df <- function(df_send, shape, year, type, sen_type) {
  df <- df_send %>%
    filter(Year == year)

  if (type == "Academisation") {
    df <- df %>% select(Year, LACode, IsAcademy) %>%
      collect() %>%
      summarise_academ(by = "LACode", multiplier = FALSE)
  } else {
    df <- df %>%
      select(Year, LACode,
             SEN_Support, Statement_EHC_Plan, TotalPupils) %>%
      collect() %>%
      summarise_sen(sen_type, by = "LACode", multiplier = FALSE)
  }

  shape@data <- shape@data %>%
    left_join(df, by = c("code" = "LACode"))
  shape
}

render_map_academ <- function(year, shape, auto_breaks = FALSE) {
  message(glue("{Sys.time()}, start rendering `map_academ`"))
  map_academ_plot <- tmap::tm_shape(shape[!is.na(shape$Academies), ]) +
    tmap::tm_polygons("Academies", id = "name",
                      palette = params$maps_academ$palette,
                      breaks = if (auto_breaks) {
                        NULL
                      } else {
                        params$maps_academ$breaks
                      }) +
    tmap::tm_legend(legend.format = list(fun = prop_to_pct))
  message(glue("{Sys.time()}, finished rendering `map_academ`"))
  withProgress(
    tmap::tmap_leaflet(map_academ_plot, mode = "view", show = TRUE),
    message = "Rendering maps...")
}

render_map_sen <- function(year, shape, sen_type, auto_breaks = FALSE) {

  message(glue("{Sys.time()}, start rendering `map_sen`"))
  map_sen_plot <- tmap::tm_shape(shape[!is.na(shape$SEN), ]) +
    tmap::tm_polygons("SEN", id = "name",
                      palette = params$maps_sen$palette,
                      breaks = if (auto_breaks) {
                        NULL
                      } else {
                        params$maps_sen$breaks
                      }) +
    tmap::tm_legend(legend.format = list(fun = prop_to_pct))
  message(glue("{Sys.time()}, finished rendering `map_sen`"))
  withProgress(
    tmap::tmap_leaflet(map_sen_plot, mode = "view", show = TRUE),
    message = "Rendering maps...")
}

render_map <- function(year, shape, df_send,
                       type = c("Academisation", "SEN"),
                       region_type = c("LA", "ParlCon"),
                       sen_type = c("SEN_Support",
                                    "Statement_EHC_Plan"),
                       auto_breaks = FALSE) {
  type <- match.arg(type)
  region_type <- match.arg(region_type)

  shape <- withProgress(
    summarise_map_df(df_send, shape, year, type, sen_type),
    message = "Collecting data...")

  if (type == "Academisation") {
    render_map_academ(year, shape, auto_breaks)
  } else {
    render_map_sen(year, shape, sen_type, auto_breaks)
  }
}
