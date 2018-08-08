render_box_total_pupils <- function(df) {
  value <- df %>%
    filter(TypeSEN == "AllSEN") %>%
    summarise_at(vars(TotalPupils), sum, na.rm = TRUE) %>%
    collect() %>%
    pull(TotalPupils) %>% format(big.mark = ",")

  valueBox(
    value = value,
    "Total number of pupils, 2017",
    icon = icon("users"),
    color = "blue")
}

render_box_sen <- function(df, sen_type,
                           value_type = c("total_number",
                                          "percent")) {
  value_type <- match.arg(value_type)
  # sen_type can be "SEN_Support", "Statement_EHC_Plan", and both
  sen_type_in_df <- ifelse(identical(sen_type, c("SEN_Support",
                                                 "Statement_EHC_Plan")),
                           "AllSEN", sen_type)
  value <- df %>% filter(TypeSEN == sen_type_in_df) %>%
    select(TotalPupils, TotalNumberSEN) %>%
    summarise_at(vars(TotalPupils, TotalNumberSEN), sum, na.rm = TRUE) %>%
    collect() %>%
    (function(df) {
      if (value_type == "total_number") {
        res <- df %>%
          pull(TotalNumberSEN) %>% format(big.mark = ",")
      } else {
        res <- df %>%
          mutate(pct = TotalNumberSEN / TotalPupils * 100) %>%
          pull(pct) %>% sprintf("%.1f%%", .)
      }
      res
    })

  value_type_str <- if_else(value_type == "total_number",
                            "Total number", "Percentage")
  valueBox(
    value = value,
    glue("{value_type_str} of pupils with SEN, 2017"),
    icon = icon("users"),
    color = "green")
}

render_box_total_schools <- function(df) {
  value <- df %>%
    summarise_at(vars(TotalNumberSchools), sum, na.rm = TRUE) %>%
    collect() %>% pull(TotalNumberSchools) %>%
    format(big.mark = ",")

  valueBox(
    value = value,
    "Total number of schools, 2017",
    icon = icon("building"),
    color = "red")
}

render_box_by_route <- function(df,
                                route = c("converter academy",
                                          "sponsored academy")) {
  route <- match.arg(route)
  label <- if_else(route == "converter academy",
                   "converter academies",
                   "sponsored academies")

  value <- df %>%
    summarise_at(vars(TotalNumberSchools,
                      TotalNumberCA,
                      TotalNumberSA), sum, na.rm = TRUE) %>%
    collect() %>% (function(df) {
    if (route == "converter academy") {
      df %>% mutate(num = TotalNumberCA)
    } else {
      df %>% mutate(num = TotalNumberSA)
    }
  }) %>%
    mutate(pct = num / TotalNumberSchools * 100,
           label = sprintf("%s (%.1f%%)",
                           format(num, big.mark = ","),
                           pct)) %>%
    pull(label)

  valueBox(
    value = value,
    glue("Total number of {label}, 2017"),
    icon = icon("building"),
    color = "yellow")
}
