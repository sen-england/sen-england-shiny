prop_to_pct <- function(x)
  paste0(sprintf("%2.0f", x * 100), " %")

format_markdown <- function(...) {

  html <- markdown::markdownToHTML(text = paste(..., sep = " "), fragment.only = TRUE)
  Encoding(html) <- "UTF-8"
  HTML(html)
}

summarise_academ <- function(df, by = "Year", multiplier = TRUE,
                             by_academisation_route = FALSE) {
  #' Params:
  #' - `by`, vec str: variable(s) to be grouped by;
  #' - `multiplier`, lgl: if `TRUE`, multiply the return value by 100;
  #' - `by_academisation_route`, lgl: if `TRUE`, group by
  #'   `Sponsored Academy` and `Converter Academy` (and discard other types).
  #'
  #' Returns:
  #' A df with column `Academies` as the number of academies

  if (!by_academisation_route) {
    res <- df %>% group_by(!!!rlang::syms(by)) %>%
      summarise(Academies = sum(IsAcademy, na.rm = TRUE) / n()) %>%
      ungroup()
  } else {
    res <- df %>% group_by(!!!rlang::syms(by)) %>%
      summarise(
        `Sponsored Academy` = sum(IsAcademy & TypeAcademy == "sponsored academy",
                                  na.rm = TRUE)/ n(),
        `Converter Academy` = sum(IsAcademy & TypeAcademy == "converter academy",
                                  na.rm = TRUE) / n()) %>%
      gather(TypeAcademy, Academies,
             `Sponsored Academy`, `Converter Academy`) %>%
      ungroup()
  }
  if (multiplier) {
    res <- res %>% mutate(Academies = Academies * 100)
  }

  res
}

summarise_sen <- function(df,
                          sen_type = c("SEN_Support", "Statement_EHC_Plan"),
                          by = "LACode", multiplier = TRUE,
                          by_sen_type = FALSE) {
  #' Params:
  #' - `by`, vec str: variable(s) to be grouped by;
  #' - `multiplier`, lgl: if `TRUE`, multiply the return value by 100;
  #' - `by_sen_type`, lgl: if `TRUE`, group by
  #'   `SEN Support` and `Statement EHC Plan` (and discard other types).
  #'
  #' Returns:
  #' A df with column `SEN` as the number of SEN pupils

  if (!by_sen_type) {
    if (identical(sen_type, c("SEN_Support", "Statement_EHC_Plan"))) {
      res <- df %>% group_by(!!!rlang::syms(by)) %>%
        summarise(SEN =
                    (sum(SEN_Support, na.rm = TRUE) +
                       sum(Statement_EHC_Plan, na.rm = TRUE))
                  / sum(TotalPupils, na.rm = TRUE)) %>%
        ungroup()
    } else if (sen_type == c("Statement_EHC_Plan")) {
      res <- df %>% group_by(!!!rlang::syms(by)) %>%
        summarise(SEN = sum(Statement_EHC_Plan, na.rm = TRUE)
                  / sum(TotalPupils, na.rm = TRUE)) %>%
        ungroup()
    } else {
      res <- df %>% group_by(!!!rlang::syms(by)) %>%
        summarise(SEN = sum(SEN_Support, na.rm = TRUE)
                  / sum(TotalPupils, na.rm = TRUE)) %>%
        ungroup()
    }
  } else {
    if (identical(sen_type, c("SEN_Support", "Statement_EHC_Plan"))) {
      res <- df %>% group_by(!!!rlang::syms(by)) %>%
        summarise(
          `SEN Support` = sum(SEN_Support, na.rm = TRUE)
          / sum(TotalPupils, na.rm = TRUE),
          `Statement EHC Plan` = sum(Statement_EHC_Plan, na.rm = TRUE)
          / sum(TotalPupils, na.rm = TRUE)) %>%
        gather(TypeSEN, SEN, `SEN Support`, `Statement EHC Plan`) %>%
        ungroup()
    } else if (sen_type == c("Statement_EHC_Plan")) {
      res <- df %>% group_by(!!!rlang::syms(by)) %>%
        summarise(`Statement EHC Plan` = sum(Statement_EHC_Plan, na.rm = TRUE)
                  / sum(TotalPupils, na.rm = TRUE)) %>%
        gather(TypeSEN, SEN, `Statement EHC Plan`) %>%
        ungroup()
    } else {
      res <- df %>% group_by(!!!rlang::syms(by)) %>%
        summarise(`SEN Support` = sum(SEN_Support, na.rm = TRUE)
                  / sum(TotalPupils, na.rm = TRUE)) %>%
        gather(TypeSEN, SEN, `SEN Support`) %>%
        ungroup()
    }
  }

  if (multiplier) {
    res <- res %>% mutate(SEN = SEN * 100)
  }

  res
}

fmt_html <- function(..., width = 40) {
  paste(..., sep = " ") %>% str_wrap(width = width) %>%
    str_replace_all("\\n", "<br/>") %>% HTML()
}
