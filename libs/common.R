prop_to_pct <- function(x)
  paste0(sprintf("%2.0f", x * 100), " %")

summarise_academ <- function(df, by = "Year", multiplier = FALSE) {
  res <- df %>% group_by(!!rlang::sym(by)) %>%
    summarise(Academies = sum(IsAcademy, na.rm = TRUE) / n()) %>%
    ungroup()
  if (multiplier) {
    res <- res %>% mutate(Academies = Academies * 100)
  }
  return(res)
}

summarise_sen <- function(df, sen_type = c("SEN_Support", "Statement_EHC_Plan"),
                          by = "LACode", multiplier = FALSE) {
  if (sen_type == c("SEN_Support", "Statement_EHC_Plan")) {
    res <- df %>% group_by(!!rlang::sym(by)) %>%
      summarise(SEN =
                  (sum(SEN_Support, na.rm = TRUE) +
                     sum(Statement_EHC_Plan, na.rm = TRUE))
                / sum(TotalPupils, na.rm = TRUE)) %>%
      ungroup()
  } else if (sen_type == c("Statement_EHC_Plan")) {
    res <- df %>% group_by(!!rlang::sym(by)) %>%
      summarise(SEN = sum(Statement_EHC_Plan, na.rm = TRUE)
                / sum(TotalPupils, na.rm = TRUE)) %>%
      ungroup()
  } else {
    res <- df %>% group_by(!!rlang::sym(by)) %>%
      summarise(SEN = sum(SEN_Support, na.rm = TRUE)
                / sum(TotalPupils, na.rm = TRUE)) %>%
      ungroup()
  }
  if (multiplier) {
    res <- res %>% mutate(SEN = SEN * 100)
  }
  return(res)
}
