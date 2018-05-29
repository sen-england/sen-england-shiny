render_box_total_pupils <- function(df_send, sen_type) {
  value <- df_send %>%
    filter(Year == 2017) %>%
    pull(TotalPupils) %>% sum(na.rm = TRUE) %>%
    format(big.mark = ",")

  valueBox(
    value = value,
    "Total number of pupils, 2017",
    icon = icon("users"),
    color = "blue")
}

render_box_total_sen <- function(df_send, sen_type) {
  value <- df_send %>%
    filter(Year == 2017) %>%
    select(SEN_Support, Statement_EHC_Plan) %>%
    collect() %>%
    (function(df){
      if (identical(sen_type,
                    c("SEN_Support", "Statement_EHC_Plan"))) {
        df %>% gather(Type, Value,
                      SEN_Support,
                      Statement_EHC_Plan) %>%
          pull(Value)
      } else if (sen_type == "Statement_EHC_Plan") {
        df %>% pull(Statement_EHC_Plan)
      } else {
        df %>% pull(SEN_Support)
      }
    }) %>%
    sum(na.rm = TRUE) %>%
    format(big.mark = ",")

  valueBox(
    value = value,
    "Total number of pupils with SEN, 2017",
    icon = icon("users"),
    color = "green")
}

render_box_pct_sen <- function(df_send, sen_type) {
  value <- df_send %>%
    filter(Year == 2017) %>%
    select(SEN_Support, Statement_EHC_Plan, TotalPupils) %>%
    collect() %>%
    (function(df){
      if (identical(sen_type,
                    c("SEN_Support", "Statement_EHC_Plan"))) {
        df %>% gather(Type, Value,
                      SEN_Support,
                      Statement_EHC_Plan) %>%
          summarise(pct = sum(Value, na.rm = TRUE) /
                      sum(TotalPupils, na.rm = TRUE) * 100)
      } else if (sen_type == "Statement_EHC_Plan") {
        df %>%
          summarise(pct = sum(Statement_EHC_Plan, na.rm = TRUE) /
                      sum(TotalPupils, na.rm = TRUE) * 100)
      } else {
        df %>%
          summarise(pct = sum(SEN_Support, na.rm = TRUE) /
                      sum(TotalPupils, na.rm = TRUE) * 100)
      }
    }) %>% pull(pct) %>%
    sprintf("%.1f%%", .)

  valueBox(
    value = value,
    "Percentage of pupils with SEN, 2017",
    icon = icon("users"),
    color = "green")
}

render_box_total_schools <- function(df_send) {
  value <- df_send %>%
    filter(Year == 2017) %>%
    summarise(n = n()) %>% pull(n) %>%
    format(big.mark = ",")

  valueBox(
    value = value,
    "Total number of schools, 2017",
    icon = icon("building"),
    color = "red")
}

render_box_ca <- function(df_send) {
  value <- df_send %>%
    filter(Year == 2017) %>%
    select(TypeAcademy) %>% collect() %>%
    summarise(
      n = sum(TypeAcademy == "converter academy"),
      pct = n / n() * 100) %>%
    mutate(value = sprintf("%s (%.1f%%)",
                           format(n, big.mark = ","),
                           pct)) %>%
    pull(value)

  valueBox(
    value = value,
    "Total number of converter academies, 2017",
    icon = icon("building"),
    color = "yellow")
}

render_box_sa <- function(df_send) {
  value <- df_send %>%
    filter(Year == 2017) %>%
    select(TypeAcademy) %>% collect() %>%
    summarise(
      n = sum(TypeAcademy == "sponsored academy"),
      pct = n / n() * 100) %>%
    mutate(value = sprintf("%s (%.1f%%)",
                           format(n, big.mark = ","),
                           pct)) %>%
    pull(value)

  valueBox(
    value = value,
    "Total number of sponsored academies, 2017",
    icon = icon("building"),
    color = "yellow")
}
