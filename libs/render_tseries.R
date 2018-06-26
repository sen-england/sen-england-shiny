render_tseries_sen <- function(years, df_send, sen_type,
                               geo_level, LA, regions,
                               scales_free, facetted,
                               palette = "Set1") {
  p <- df_send %>%
    summarise_sen_tseries(
      sen_type = sen_type,
      by = if (facetted) c("Year", "TypeGeneral") else c("Year"),
      multiplier = TRUE) %>%
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
                                  geo_level, LA, regions,
                                  scales_free, facetted,
                                  palette = "Set2") {
  p <- df_send %>%
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


render_tseries <- function(years, df_send,
                           LA,
                           geo_level = "whole_country",
                           type = c("Academisation", "SEN"),
                           scales_free = FALSE,
                           facetted = FALSE,
                           sen_type = c("SEN_Support", "Statement_EHC_Plan"),
                           regions = c("E12000007", "E12000003", "E12000009",
                                       "E12000006", "E12000005", "E12000002",
                                       "E12000008", "E12000001", "E12000004")) {
  type <- match.arg(type)
  df_send <- df_send %>% filter(between(Year, years[1], years[2])) %>%
    (function(df) {
      if (geo_level == "whole_country") {
        df
      } else if (geo_level == "region") {
        df %>% filter(RegionCode %in% regions)
      } else if (geo_level == "la") {
        df %>% filter(LACode %in% LA)
      }
    }) %>%
    collect()
  if (type == "Academisation") {
    render_tseries_academ(
      years, df_send,
      geo_level = geo_level, LA = LA, regions = regions,
      scales_free = scales_free, facetted = facetted)
  } else {
    render_tseries_sen(
      years, df_send,
      sen_type = sen_type,
      geo_level = geo_level, LA = LA, regions = regions,
      scales_free = scales_free, facetted = facetted)
  }
}
