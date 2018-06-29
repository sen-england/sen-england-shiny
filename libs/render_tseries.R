render_tseries_sen <- function(years, df_send, sen_type,
                               geo_level, LA, region,
                               scales_free, facetted,
                               palette = "Set1") {
  p <- df_send %>%
    summarise_sen(
      sen_type = sen_type,
      by = if (facetted) c("Year", "TypeGeneral") else c("Year"),
      multiplier = TRUE,
      by_sen_type = TRUE) %>%
    ggplot(aes(x = Year, y = SEN,
               group = TypeSEN, color = TypeSEN)) +
    geom_line() + geom_point() +
    scale_color_brewer(palette = palette) +
    labs(title = "Percentage of pupils with SEN")
  if (facetted)
    p <- p +
      facet_wrap(~ TypeGeneral,
                 scales = ifelse(scales_free, "free_y", "fixed"))
  p
}

render_tseries_academ <- function(years, df_send,
                                  geo_level, LA, region,
                                  scales_free, facetted,
                                  palette = "Set2") {
  p <- df_send %>%
    summarise_academ(
      by = if (facetted) c("Year", "TypeGeneral") else c("Year"),
      multiplier = TRUE,
      by_academisation_route = TRUE) %>%
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


render_tseries <- function(years, df_send,
                           region, LA, parlcon,
                           geo_level = "whole_country",
                           type = c("Academisation", "SEN"),
                           scales_free = FALSE,
                           facetted = FALSE,
                           sen_type = c("SEN_Support", "Statement_EHC_Plan")) {
  type <- match.arg(type)
  df_send <- df_send %>% filter(between(Year, years[1], years[2])) %>%
    (function(df) {
      if (geo_level == "whole_country") {
        df
      } else if (geo_level == "region") {
        df %>% filter(RegionCode %in% region)
      } else if (geo_level == "LA") {
        df %>% filter(LACode %in% LA)
      } else if (geo_level == "parlcon") {
        df %>% filter(ParlConCode %in% parlcon)
      }
    }) %>%
    collect()
  if (type == "Academisation") {
    render_tseries_academ(
      years, df_send,
      geo_level = geo_level, LA = LA, region = region,
      scales_free = scales_free, facetted = facetted)
  } else {
    render_tseries_sen(
      years, df_send,
      sen_type = sen_type,
      geo_level = geo_level, LA = LA, region = region,
      scales_free = scales_free, facetted = facetted)
  }
}
