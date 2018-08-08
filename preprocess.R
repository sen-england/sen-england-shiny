# Prepares various auxiliary preprocessed datasets from the main dataset
library("dplyr")
library("dbplyr")
library("purrr")
library("readr")
library("tidyr")
library("tibble")
library("stringr")
library("glue")
options(stringsAsFactors = FALSE)

preprocess_stats_sen <- function(df_main, data_conf) {
  df_main %>%
    filter(Year == data_conf$send_db$periods$last) %>%
    select(TypeGeneral, Phase,
           TotalPupils, SEN_Support, Statement_EHC_Plan) %>%
    collect() %>%
    group_by(TypeGeneral, Phase) %>%
    mutate(AllSEN = SEN_Support + Statement_EHC_Plan) %>%
    gather("TypeSEN", "TotalNumberSEN",
           SEN_Support, Statement_EHC_Plan, AllSEN) %>%
    group_by(TypeSEN, add = TRUE) %>%
    # NOTE: Percentage of SEN should be calculated in-place
    summarise_at(vars(TotalPupils, TotalNumberSEN), sum) %>%
    ungroup()
}

preprocess_stats_schools <- function(df_main, data_conf) {
  df_main %>%
    filter(Year == data_conf$send_db$periods$last) %>%
    count(TypeGeneral, Phase, TypeAcademy) %>%
    collect() %>%
    group_by(TypeGeneral, Phase) %>% nest() %>%
    mutate(data = map(
      data,
      function(df) {
        tibble(TotalNumberSchools = sum(df$n),
               TotalNumberCA = df %>%
                 filter(TypeAcademy == "converter academy") %>%
                 pull(n) %>% sum(),
               TotalNumberSA = df %>%
                 filter(TypeAcademy == "sponsored academy") %>%
                 pull(n) %>% sum())
      })) %>% unnest()
}

preprocess_composition_schools <- function(df_main, data_conf) {
  df_main %>%
    filter(Year == data_conf$send_db$periods$last) %>%
    count(TypeGeneral, Phase, TypeAcademy) %>%
    collect()
}

preprocess_dashboard_ts_sen <- function(df_main) {
}

main <- function() {
  # Params
  data_conf <- config::get("data")
  send_db_conf <- data_conf$send_db
  params <- config::get("params")

  # Main data set
  send_db_conn <- DBI::dbConnect(
    RSQLite::SQLite(),
    dbname = send_db_conf$db)
  df_main <- send_db_conn %>%
    tbl(send_db_conf$tbl) %>%
    select(one_of(send_db_conf$vars))

  # Preprocess
  preproc_conf <- data_conf$preprocess
  conn <- DBI::dbConnect(RSQLite::SQLite(),
                         dbname = preproc_conf$db)
  # stats_SEN
  cat(glue("Preprocessing `{preproc_conf$stats_sen}`"), "\n")
  df_main %>% preprocess_stats_sen(data_conf = data_conf) %T>%
    glimpse() %>%
    DBI::dbWriteTable(conn = conn, name = preproc_conf$stats_sen,
                      value = ., overwrite = TRUE)
  # stats_schools
  cat(glue("Preprocessing `{preproc_conf$stats_schools}`"), "\n")
  df_main %>% preprocess_stats_schools(data_conf = data_conf) %T>%
    glimpse() %>%
    DBI::dbWriteTable(conn = conn, name = preproc_conf$stats_schools,
                      value = ., overwrite = TRUE)
  # composition_schools
  cat(glue("Preprocessing `{preproc_conf$composition_schools}`"), "\n")
  df_main %>% preprocess_composition_schools(data_conf = data_conf) %T>%
    glimpse() %>%
    DBI::dbWriteTable(conn = conn, name = preproc_conf$composition_schools,
                      value = ., overwrite = TRUE)
  # NOTE: the gains from preprocessing time-series summarisation are
  #       very minimal, and we do not preproc them ATM.
}

main()
