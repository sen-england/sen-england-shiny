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

render_tseries_sen <- function(years, df_send, sen_type, regions,
                               scales_free, facetted,
                               palette = "Set1") {
  p <- df_send %>% filter(between(Year, years[1], years[2])) %>%
    filter(RegionCode %in% regions) %>%
    collect() %>%
    summarise_sen_tseries(
      sen_type = sen_type,
      by = if (facetted) c("Year", "TypeGeneral") else c("Year"),
      multiplier = TRUE) %>%
    ggplot(aes(x = Year, y = SEN,
               group = TypeSEN, color = TypeSEN)) +
    geom_line() + geom_point() +
    scale_color_brewer(palette = palette) +
    labs(title = "Percentage of inclusion of pupils with SEN")
  if (facetted)
    p <- p +
      facet_wrap(~ TypeGeneral,
                 scales = ifelse(scales_free, "free_y", "fixed"))
  p
}

render_tseries_academ <- function(years, df_send, regions,
                                  scales_free, facetted,
                                  palette = "Set2") {
  p <- df_send %>% filter(between(Year, years[1], years[2])) %>%
    filter(RegionCode %in% regions) %>%
    collect() %>%
    summarise_academ_tseries(
      by = if (facetted) c("Year", "TypeGeneral") else c("Year"),
      multiplier = TRUE) %>%
    ggplot(aes(x = Year, y = Academies,
               group = TypeAcademy, color = TypeAcademy)) +
    geom_line() + geom_point() +
    scale_color_brewer(palette = palette) +
    labs(title = "Percentage of academisation")
  if (facetted)
    p <- p +
      facet_wrap(~ TypeGeneral,
                 scales = ifelse(scales_free, "free_y", "fixed"))
  p
}

render_primary_academ <- function(df_send,
                                  palette = "Set2") {
  df_send %>%
    select(Year, IsAcademy, TypeAcademy) %>%
    collect() %>%
    summarise_academ_tseries(
      by = "Year", TRUE) %>%
    ggplot(aes(x = Year, y = Academies,
               group = TypeAcademy, color = TypeAcademy)) +
    facet_wrap(~ TypeAcademy, scales = "fixed", ncol = 1) +
    geom_line() +
    scale_color_brewer(palette = palette) +
    theme(legend.position = "none",
          axis.title.y = element_blank(),
          axis.title.x = element_blank()) +
    labs(title = "Percentage of academised schools")
}

render_primary_sen <- function(df_send, sen_type,
                               palette = "Set1") {
  df_send %>%
    select(Year, TotalPupils,
           SEN_Support, Statement_EHC_Plan) %>%
    collect() %>%
    summarise_sen_tseries(
      sen_type = sen_type,
      by = "Year", TRUE) %>%
    ggplot(aes(x = Year, y = SEN,
               group = TypeSEN, color = TypeSEN)) +
    facet_wrap(~ TypeSEN, scales = "fixed", ncol = 1) +
    geom_line() +
    scale_color_brewer(palette = palette) +
    theme(legend.position = "none",
          axis.title.y = element_blank(),
          axis.title.x = element_blank()) +
    labs(title = "Percentage of pupils with SEN")
}

render_primary_composition <- function(df_send, pct = FALSE,
                                       palette = "Set2") {
  df <- df_send %>% filter(Year == 2017L) %>% count(TypeAcademy, TypeGeneral) %>%
    collect() %>% ungroup()
  if (pct) {
    df <- df %>% group_by(TypeGeneral) %>%
      mutate(n_group = sum(n, na.rm = TRUE)) %>%
      ungroup() %>%
      group_by(TypeGeneral, TypeAcademy) %>%
      summarise(Percentage = n / n_group * 100) %>%
      ungroup()
  }
  df <- df %>%
    mutate_at(
      vars(TypeAcademy), fct_relevel,
      "maintained school",
      "converter academy", "sponsored academy",
      "free school") %>%
    mutate_at(
      vars(TypeGeneral), fct_relevel,
      "mainstream school", "special school", "pupil referral unit") %>%
    mutate_at(vars(TypeAcademy, TypeGeneral), fct_drop) %>%
    # Use with coord_flip
    mutate_at(vars(TypeGeneral), fct_rev) %>%
    mutate(
      Group = if_else(TypeGeneral == "mainstream school",
                      "Mainstream", "Non-mainstream"))
  if (pct) {
    p <- df %>% ggplot(aes(x = TypeGeneral, y = Percentage))
  } else {
    p <- df %>% ggplot(aes(x = TypeGeneral, y = n))
  }
  p <- p +
    facet_wrap(~ Group, scales = "free", nrow = 1) +
    geom_col(aes(fill = TypeAcademy)) +
    coord_flip() +
    theme(
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.y = element_text(angle = 270)) +
    scale_fill_brewer(palette = palette) +
    labs(title = "Composition of schools in England, academic year 2016/2017")
  p
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
                           scales_free = FALSE,
                           facetted = FALSE,
                           sen_type = c("SEN_Support", "Statement_EHC_Plan"),
                           regions = c("E12000007", "E12000003", "E12000009",
                                       "E12000006", "E12000005", "E12000002",
                                       "E12000008", "E12000001", "E12000004")) {
  type <- match.arg(type)
  if (type == "Academisation") {
    render_tseries_academ(years, df_send,
                          regions = regions, scales_free = scales_free,
                          facetted = facetted)
  } else {
    render_tseries_sen(years, df_send,
                       sen_type = sen_type, regions = regions,
                       scales_free = scales_free,
                       facetted = facetted)
  }
}
