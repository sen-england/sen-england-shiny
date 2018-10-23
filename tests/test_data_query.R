suppressMessages(suppressWarnings({
  library("testthat")
  library("tidyverse")
  library("magrittr")
  library("here")
}))

source(here("libs/common.R"), local = TRUE)

data_conf <- config::get("data")
sen_db_conf <- data_conf$sen_db
preproc_conf <- data_conf$preprocess
params <- config::get("params")
candidates <- params$candidates

sen_db_conn <- DBI::dbConnect(
  RSQLite::SQLite(),
  dbname = here(sen_db_conf$db))
preproc_db_conn <- DBI::dbConnect(
  RSQLite::SQLite(),
  dbname = here(preproc_conf$db))
df_main_table <- sen_db_conn %>%
  tbl(sen_db_conf$tbl) %>%
  select(one_of(sen_db_conf$vars))

dsb_id_primary <- "primary"
dsb_id_tseries <- "tseries"
dsb_id_maps <- "maps"
cand_year <- candidates$year
cand_type <- candidates$type$value %>% set_names(candidates$type$name)
cand_phase <- candidates$phase$value %>% set_names(candidates$phase$name)
cand_type_sen <- candidates$type_sen$value %>% set_names(candidates$type_sen$name)
cand_type_school <- candidates$type_school$value %>%
  set_names(candidates$type_school$name)
cand_la_tbl <- read_csv(here("data/data-dictionaries/region-info.csv"),
                        col_types = c("cccc"))
cand_la <- cand_la_tbl %>%
  select(LAName, LACode) %>% distinct() %>% deframe()
cand_region <- cand_la_tbl %>%
  select(RegionName, RegionCode) %>% distinct() %>% deframe()
cand_parlcon <- read_csv(here("data/data-dictionaries/parlcon-info.csv"),
                         col_types = c("cc")) %>% deframe()

context("Combination of candidates")

combine_candidates <- function(candidates) {
  # For a vector of candidates, generate a combination of themselves.
  # i.e. for c("A", "B", "C"), get
  # - "A"; "B"; "C"; "A", "B"; "A", "C"; "B", "C"; "A", "B", "C"
  1:length(candidates) %>%
    map(function(m) partial(combn, m = m, simplify = FALSE,
                            .lazy = FALSE)) %>%
    invoke_map(x = candidates) %>%
    purrr::flatten()
}

test_that("`combine_candidates`", {
  outcome <- c("A", "B", "C") %>% combine_candidates()
  expected_outcome <- list(c("A"), c("B"), c("C"),
                           c("A", "B"), c("A", "C"), c("B", "C"),
                           c("A", "B", "C"))
  expect_equal(outcome, expected_outcome)
})

context(paste0("Global arguments: reactive loadings of dataframes ",
               "under all argument candidates"))

args <- cross(list(
  Phase = cand_phase %>% combine_candidates(),
  TypeGeneral = cand_type_school %>% combine_candidates()
))

test_that("`df_main`", {
  expect_silent({
    args %>%
      map(function(arg) {
        df_main <- df_main_table %>%
          filter(Phase %in% arg$Phase,
                 TypeGeneral %in% arg$TypeGeneral) %>%
          head() %>% collect()
        df_main
      })
  })
})

test_that("`df_preproc_stats_sen`", {
  expect_silent({
    args %>%
      map(function(arg) {
        df_preproc_stats_sen <- preproc_db_conn %>%
          tbl(preproc_conf$stats_sen) %>%
          filter(Phase %in% arg$Phase,
                 TypeGeneral %in% arg$TypeGeneral) %>%
          head() %>% collect()
        df_preproc_stats_sen
      })
  })
})

test_that("`df_preproc_stats_schools`", {
  expect_silent({
    args %>%
      map(function(arg) {
        df_preproc_stats_schools <- preproc_db_conn %>%
          tbl(preproc_conf$stats_schools) %>%
          filter(Phase %in% arg$Phase,
                 TypeGeneral %in% arg$TypeGeneral) %>%
          head() %>% collect()
        df_preproc_stats_schools
      })
  })
})

test_that("`df_preproc_composition_sen`", {
  expect_silent({
    args %>%
      map(function(arg) {
        df_preproc_composition_sen <- preproc_db_conn %>%
          tbl(preproc_conf$composition_sen) %>%
          filter(Phase %in% arg$Phase,
                 TypeGeneral %in% arg$TypeGeneral) %>%
          head() %>% collect()
        df_preproc_composition_sen
      })
  })
})

test_that("`df_preproc_composition_schools`", {
  expect_silent({
    args %>%
      map(function(arg) {
        df_preproc_composition_schools <- preproc_db_conn %>%
          tbl(preproc_conf$composition_schools) %>%
          filter(Phase %in% arg$Phase,
                 TypeGeneral %in% arg$TypeGeneral) %>%
          head() %>% collect()
        df_preproc_composition_schools
      })
  })
})
