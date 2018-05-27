# ==== renderer by type ====
render_map_academ <- function(year, shape, df_send, region, whole_country) {
  message(glue("{Sys.time()}, start rendering `map_academ`"))
  shape@data <- shape@data %>%
    left_join(
      df_send %>%
        filter(Year == year) %>%
        (function(df)
          if (!whole_country) {
            df %>% filter(RegionCode %in% region)
          } else {
            df
          }) %>%
        select(Year, LACode, IsAcademy) %>%
        collect() %>%
        summarise_academ(by = "LACode", multiplier = FALSE),
      by = c("code" = "LACode"))
  map_academ_plot <- tm_shape(shape[!is.na(shape$Academies), ]) +
    tm_polygons("Academies", id = "name",
                palette = params$maps_academ$palette,
                breaks = params$maps_academ$breaks) +
    tm_legend(legend.format = list(fun = prop_to_pct))
  message(glue("{Sys.time()}, finished rendering `map_academ`"))
  withProgress(
    tmap_leaflet(map_academ_plot, mode = "view", show = TRUE),
    message = "Rendering maps... Please wait")

}

render_map_sen <- function(year, shape, df_send, sen_type, region, whole_country) {

  message(glue("{Sys.time()}, start rendering `map_sen`"))
  shape@data <- shape@data %>%
    left_join(
      df_send %>%
        filter(Year == year) %>%
        (function(df)
          if (!whole_country) {
            df %>% filter(RegionCode %in% region)
          } else {
            df
          }) %>%
        select(Year, LACode,
               SEN_Support, Statement_EHC_Plan, TotalPupils) %>%
        collect() %>%
        summarise_sen(sen_type, by = "LACode", multiplier = FALSE),
      by = c("code" = "LACode"))
  map_sen_plot <- tm_shape(shape[!is.na(shape$SEN), ]) +
    tm_polygons("SEN", id = "name",
                palette = params$maps_sen$palette,
                breaks = params$maps_sen$breaks) +
    tm_legend(legend.format = list(fun = prop_to_pct))
  message(glue("{Sys.time()}, finished rendering `map_sen`"))
  withProgress(
    tmap_leaflet(map_sen_plot, mode = "view", show = TRUE),
    message = "Rendering maps... Please wait")
}

render_tseries_sen <- function(years, df_send, sen_type, regions) {
  df_send %>% filter(between(Year, years[1], years[2])) %>%
    filter(RegionCode %in% regions) %>%
    collect() %>%
    summarise_sen_tseries(sen_type = sen_type, by = "Year", multiplier = TRUE) %>%
    ggplot(aes(x = Year, y = SEN,
               group = TypeSEN, color = TypeSEN)) +
    geom_line() + geom_point() +
    labs(title = "Percentage of inclusion of pupils with SEN")
}

render_tseries_academ <- function(years, df_send, regions) {
  df_send %>% filter(between(Year, years[1], years[2])) %>%
    filter(RegionCode %in% regions) %>%
    collect() %>%
    summarise_academ_tseries(by = c("Year"), multiplier = TRUE) %>%
    ggplot(aes(x = Year, y = Academies,
               group = TypeAcademy, color = TypeAcademy)) +
    geom_line() + geom_point() +
    labs(title = "Percentage of academisation")
}

# ==== main renderer ====

render_map <- function(year, shape, df_send,
                       type = c("Academisation", "SEN"),
                       sen_type = c("SEN_Support",
                                    "Statement_EHC_Plan"),
                       region = c("E12000007", "E12000003", "E12000009",
                                  "E12000006", "E12000005", "E12000002",
                                  "E12000008", "E12000001", "E12000004"),
                       whole_country = FALSE) {
  type <- match.arg(type)
  if (type == "Academisation") {
    render_map_academ(year, shape, df_send, region, whole_country)
  } else {
    render_map_sen(year, shape, df_send, sen_type, region, whole_country)
  }
}

render_tseries <- function(years, df_send,
                           type = c("Academisation", "SEN"),
                           sen_type = c("SEN_Support", "Statement_EHC_Plan"),
                           regions = c("E12000007", "E12000003", "E12000009",
                                       "E12000006", "E12000005", "E12000002",
                                       "E12000008", "E12000001", "E12000004")) {
  type <- match.arg(type)
  if (type == "Academisation") {
    render_tseries_academ(years, df_send, regions = regions)
  } else {
    render_tseries_sen(years, df_send, sen_type = sen_type, regions = regions)
  }
}
