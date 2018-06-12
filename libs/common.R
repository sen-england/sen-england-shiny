prop_to_pct <- function(x)
  paste0(sprintf("%2.0f", x * 100), " %")

summarise_academ_tseries <- function(df, by = "Year", multiplier = TRUE) {
  res <- df %>% group_by(!!!rlang::syms(by)) %>%
    summarise(
      `Sponsored Academy` = sum(IsAcademy & TypeAcademy == "sponsored academy",
                                na.rm = TRUE)/ n(),
      `Converter Academy` = sum(IsAcademy & TypeAcademy == "converter academy",
                                na.rm = TRUE) / n()) %>%
    gather(TypeAcademy, Academies,
           `Sponsored Academy`, `Converter Academy`) %>%
    ungroup()
  if (multiplier) {
    res <- res %>% mutate(Academies = Academies * 100)
  }
  return(res)
}

summarise_sen_tseries <- function(df,
                                  sen_type = c("SEN_Support",
                                               "Statement_EHC_Plan"),
                                  by = "LACode", multiplier = TRUE) {
  if (sen_type == c("SEN_Support", "Statement_EHC_Plan")) {
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
  if (multiplier) {
    res <- res %>% mutate(SEN = SEN * 100)
  }
  return(res)
}

summarise_academ <- function(df, by = "Year", multiplier = TRUE) {
  res <- df %>% group_by(!!!rlang::syms(by)) %>%
    summarise(Academies = sum(IsAcademy, na.rm = TRUE) / n()) %>%
    ungroup()
  if (multiplier) {
    res <- res %>% mutate(Academies = Academies * 100)
  }
  return(res)
}

summarise_sen <- function(df, sen_type = c("SEN_Support", "Statement_EHC_Plan"),
                          by = "LACode", multiplier = TRUE) {
  if (sen_type == c("SEN_Support", "Statement_EHC_Plan")) {
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
  if (multiplier) {
    res <- res %>% mutate(SEN = SEN * 100)
  }
  return(res)
}

fmt_html <- function(..., width = 40) {
  paste(..., sep = " ") %>% str_wrap(width = width) %>%
    str_replace_all("\\n", "<br/>") %>% HTML()
}
