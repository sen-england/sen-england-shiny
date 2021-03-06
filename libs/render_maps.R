summarise_map_df <- function(df, shape, year, type, geo_level, sen_type) {
  df <- df %>%
    filter(Year == year)
  if (geo_level == "LA") {
    by_var = "LACode"
  } else {
    by_var = "ParlConCode"
  }

  if (type == "Academisation") {
    df <- df %>% select(Year, one_of(by_var), IsAcademy) %>%
      collect() %>%
      summarise_academ(by = by_var, multiplier = FALSE)
  } else {
    df <- df %>%
      select(Year, one_of(by_var),
             SEN_Support, Statement_EHC_Plan, TotalPupils) %>%
      collect() %>%
      summarise_sen(sen_type, by = by_var, multiplier = FALSE)
  }

  shape@data <- shape@data %>% left_join(df, by = c("code" = by_var))
  shape
}

render_map_academ <- function(year, shape, auto_breaks = FALSE) {
  message(glue("{Sys.time()}, start rendering `map_academ`"))
  map_academ_plot <- tmap::tm_shape(shape) +
    tmap::tm_polygons("Academies", id = "name",
                      palette = params$maps_academ$palette,
                      breaks = if (auto_breaks) {
                        NULL
                      } else {
                        params$maps_academ$breaks
                      }) +
    tmap::tm_legend(legend.format = list(fun = prop_to_pct))
  message(glue("{Sys.time()}, finished rendering `map_academ`"))

  tmap::tmap_leaflet(map_academ_plot, mode = "view", show = TRUE)
}

render_map_sen <- function(year, shape, sen_type, auto_breaks = FALSE) {

  message(glue("{Sys.time()}, start rendering `map_sen`"))
  map_sen_plot <- tmap::tm_shape(shape) +
    tmap::tm_polygons("SEN", id = "name",
                      palette = params$maps_sen$palette,
                      breaks = if (auto_breaks) {
                        NULL
                      } else {
                        params$maps_sen$breaks
                      }) +
    tmap::tm_legend(legend.format = list(fun = prop_to_pct))
  message(glue("{Sys.time()}, finished rendering `map_sen`"))

  tmap::tmap_leaflet(map_sen_plot, mode = "view", show = TRUE)
}

render_map_academ_async <- function(year, shape, auto_breaks = FALSE) {
  future({
    tmap::tm_shape(shape) +
      tmap::tm_polygons("Academies", id = "name",
                        palette = params$maps_academ$palette,
                        breaks = if (auto_breaks) {
                          NULL
                        } else {
                          params$maps_academ$breaks
                        }) +
        tmap::tm_legend(legend.format = list(fun = prop_to_pct))
  }) %...>%
    tmap::tmap_leaflet(mode = "view", show = TRUE)
}

render_map_sen_async <- function(year, shape, sen_type, auto_breaks = FALSE) {
  future({
    tmap::tm_shape(shape) +
      tmap::tm_polygons("SEN", id = "name",
                        palette = params$maps_sen$palette,
                        breaks = if (auto_breaks) {
                          NULL
                        } else {
                          params$maps_sen$breaks
                        }) +
        tmap::tm_legend(legend.format = list(fun = prop_to_pct))
  }) %...>%
    tmap::tmap_leaflet(mode = "view", show = TRUE)
}

render_map <- function(year, shape, df,
                       type = c("Academisation", "SEN"),
                       geo_level = c("LA", "ParlCon"),
                       sen_type = c("SEN_Support",
                                    "Statement_EHC_Plan"),
                       auto_breaks = FALSE) {
  type <- match.arg(type)
  geo_level <- match.arg(geo_level)

  shape <- summarise_map_df(df, shape, year, type, geo_level, sen_type)

  if (type == "Academisation") {
    render_map_academ(year, shape, auto_breaks)
  } else {
    render_map_sen(year, shape, sen_type, auto_breaks)
  }
}
