render_primary_academ <- function(df, palette = "Set2") {
  df %>%
    select(Year, IsAcademy, TypeAcademy) %>%
    collect() %>%
    summarise_academ(
      by = "Year", multiplier = TRUE,
      by_academisation_route = TRUE) %>%
    mutate_at(vars(Year), as.factor) %>% {
      ggplot(., aes(x = Year, y = Academies,
                    group = TypeAcademy, color = TypeAcademy)) +
        facet_wrap(~ TypeAcademy, scales = "fixed", ncol = 1) +
        geom_line() + geom_point() +
        scale_color_brewer(palette = palette) +
        theme(legend.position = "none",
              axis.title.y = element_blank(),
              axis.title.x = element_blank())
    } %>%
    plotly::ggplotly()
}

render_primary_sen <- function(df, sen_type, palette = "Set1") {
  df %>%
    select(Year, TotalPupils,
           SEN_Support, Statement_EHC_Plan) %>%
    collect() %>%
    summarise_sen(
      sen_type = sen_type, by = "Year",
      multiplier = TRUE, by_sen_type = TRUE) %>%
    mutate_at(vars(Year), as.factor) %>% {
      ggplot(., aes(x = Year, y = SEN,
                    group = TypeSEN, color = TypeSEN)) +
        facet_wrap(~ TypeSEN, scales = "fixed", ncol = 1) +
        geom_line() + geom_point() +
        scale_color_brewer(palette = palette) +
        theme(legend.position = "none",
              axis.title.y = element_blank(),
              axis.title.x = element_blank())
    } %>%
    plotly::ggplotly()
}

render_primary_composition_schools <- function(df, pct = FALSE,
                                               palette = "Set2") {
  df_schools <- df %>% group_by(TypeAcademy, TypeGeneral) %>%
    summarise_at(vars(n), sum, na.rm = TRUE) %>% ungroup() %>% collect()
  if (pct) {
    df_schools <- df_schools %>% group_by(TypeGeneral) %>%
      mutate(n_group = sum(n, na.rm = TRUE)) %>%
      ungroup() %>%
      group_by(TypeGeneral, TypeAcademy) %>%
      summarise(Percentage = n / n_group * 100) %>%
      ungroup()
  }
  df_schools <- df_schools %>%
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
    p <- df_schools %>% ggplot(aes(x = TypeGeneral, y = Percentage))
  } else {
    p <- df_schools %>% ggplot(aes(x = TypeGeneral, y = n))
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
    labs(title = "Composition of schools, academic year 2016/2017")
  plotly::ggplotly(p)
}

render_primary_composition_sen <- function(df, pct = FALSE,
                                           palette = "Set1") {
  df_sen <- df %>%
    group_by(TypeGeneral, TypeAcademy) %>%
    summarise_at(vars(NumPupils), sum, na.rm = TRUE) %>% ungroup() %>% collect()

  if (pct) {
    df_sen <- df_sen %>% group_by(TypeGeneral) %>%
      mutate(n_group = sum(NumPupils, na.rm = TRUE)) %>%
      ungroup() %>%
      group_by(TypeGeneral, TypeAcademy) %>%
      summarise(Percentage = NumPupils / n_group * 100) %>%
      ungroup()
  }
  df_sen <- df_sen %>%
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
    p <- df_sen %>% ggplot(aes(x = TypeGeneral, y = Percentage))
  } else {
    p <- df_sen %>% ggplot(aes(x = TypeGeneral, y = NumPupils))
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
    labs(title = "Composition of SEN pupils, academic year 2016/2017")
  plotly::ggplotly(p)
}
