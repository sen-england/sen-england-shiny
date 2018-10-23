# Prepares various auxiliary preprocessed datasets from the main dataset
library("tidyverse")
library("magrittr")
library("glue")
library("here")
library("sf")
options(stringsAsFactors = FALSE)

preprocess_stats_sen <- function(df_main, data_conf) {
  df_main %>%
    filter(Year == data_conf$sen_db$periods$last) %>%
    select(TypeGeneral, Phase,
           TotalPupils, SEN_Support, Statement_EHC_Plan) %>%
    collect() %>%
    mutate_at(vars(SEN_Support, Statement_EHC_Plan, TotalPupils),
              coalesce, 0L) %>%
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
    filter(Year == data_conf$sen_db$periods$last) %>%
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
    filter(Year == data_conf$sen_db$periods$last) %>%
    count(TypeGeneral, Phase, TypeAcademy) %>%
    collect()
}

preprocess_composition_sen <- function(df_main, data_conf) {
  df_main %>%
    filter(Year == data_conf$sen_db$periods$last) %>%
    select(TypeGeneral, Phase, TypeAcademy,
           SEN_Support, Statement_EHC_Plan) %>%
    collect() %>%
    gather(TypeSEN, NumPupils, SEN_Support, Statement_EHC_Plan)
}

main <- function() {
  # Params
  data_conf <- config::get("data")
  sen_db_conf <- data_conf$sen_db
  params <- config::get("params")

  # Main dataset
  sen_db_conn <- DBI::dbConnect(
    RSQLite::SQLite(),
    dbname = here(sen_db_conf$db))
  df_main <- sen_db_conn %>%
    tbl(sen_db_conf$tbl) %>%
    select(one_of(sen_db_conf$vars))

  # Auxiliary datasets
  la_tbl <- read_csv(here("data/data-dictionaries/region-info.csv"),
                     col_types = c("cccc"))
  parlcon_tbl <- read_csv(here("data/data-dictionaries/parlcon-info.csv"),
                          col_types = c("cc"))

  # ==== Preprocess ====
  preproc_conf <- data_conf$preprocess
  conn <- DBI::dbConnect(RSQLite::SQLite(),
                         dbname = here(preproc_conf$db))

  # ---- boundary file ----
  england_la_2011 <- read_sf(here("data", "England_ct_2011",
                                  "england_ct_2011.shp"))

  england_la_2011_simplified <- england_la_2011 %>%
    st_simplify(dTolerance = 10)
  england_la_2011_simplified %T>%
    {
      dir.create(here("output", "england_ct_2011"), showWarnings = FALSE)
    } %>%
    write_sf(here("output", "england_ct_2011",
                  "england_ct_2011.shp"))

  # ---- stats_SEN ----
  cat(glue("Preprocessing `{preproc_conf$stats_sen}`"), "\n")
  df_main %>% preprocess_stats_sen(data_conf = data_conf) %T>%
    glimpse() %>%
    DBI::dbWriteTable(conn = conn, name = preproc_conf$stats_sen,
                      value = ., overwrite = TRUE)
  # ---- stats_schools ----
  cat(glue("Preprocessing `{preproc_conf$stats_schools}`"), "\n")
  df_main %>% preprocess_stats_schools(data_conf = data_conf) %T>%
    glimpse() %>%
    DBI::dbWriteTable(conn = conn, name = preproc_conf$stats_schools,
                      value = ., overwrite = TRUE)
  # ---- composition_schools ----
  cat(glue("Preprocessing `{preproc_conf$composition_schools}`"), "\n")
  df_main %>% preprocess_composition_schools(data_conf = data_conf) %T>%
    glimpse() %>%
    DBI::dbWriteTable(conn = conn, name = preproc_conf$composition_schools,
                      value = ., overwrite = TRUE)
  # ---- composition_sen ----
  cat(glue("Preprocessing `{preproc_conf$composition_sen}`"), "\n")
  df_main %>% preprocess_composition_sen(data_conf = data_conf) %T>%
    glimpse() %>%
    DBI::dbWriteTable(conn = conn, name = preproc_conf$composition_sen,
                      value = ., overwrite = TRUE)
  # NOTE: the gains from preprocessing time-series summarisation are
  #       very minimal, and we do not preproc them ATM.
  # ---- geographical information ----
  cat(glue("Preprocessing `{preproc_conf$cand_region}`"), "\n")
  la_tbl %>% select(RegionName, RegionCode) %>% distinct() %>%
    DBI::dbWriteTable(conn = conn, name = preproc_conf$cand_region,
                      value = ., overwrite = TRUE)
  cat(glue("Preprocessing `{preproc_conf$cand_la}`"), "\n")
  la_tbl %>% select(LAName, LACode) %>% distinct() %>%
    DBI::dbWriteTable(conn = conn, name = preproc_conf$cand_la,
                      value = ., overwrite = TRUE)
  cat(glue("Preprocessing `{preproc_conf$cand_parlcon}`"), "\n")
  parlcon_tbl %>% distinct() %>%
    DBI::dbWriteTable(conn = conn, name = preproc_conf$cand_parlcon,
                      value = ., overwrite = TRUE)
}

main()
