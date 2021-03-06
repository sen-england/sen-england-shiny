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

df_main <- df_main_table %>%
  filter(Phase %in% cand_phase) %>%
  filter(TypeGeneral %in% cand_type_school) %>%
  collect()

context("libs/common.R: `summarise_academ`")

test_that("`summarise_academ`: structure", {
  expected_names <- c("Year", "Phase", "Academies")
  names <- df_main %>%
    summarise_academ(by = c("Year", "Phase"),
                     by_academisation_route = FALSE) %>%
    names()
  expect_equal(names, expected_names)
})

test_that("`summarise_academ`: structure with `by_academisation_route`", {
  expected_names <- c("Year", "RegionCode", "TypeAcademy", "Academies")
  names <- df_main %>%
    summarise_academ(by = c("Year", "RegionCode"),
                     by_academisation_route = TRUE) %>%
    names()
  expect_equal(names, expected_names)
})

test_that("`summarise_academ`: multiplier", {
  prop_value <- df_main %>%
    summarise_academ(by = c("RegionCode"),
                     multiplier = FALSE) %$%
    Academies
  pct_value <- df_main %>%
    summarise_academ(by = c("RegionCode"),
                     multiplier = TRUE) %$%
    Academies
  expect_equal(prop_value * 100, pct_value)
})

test_that("`summarise_academ`: summarisation", {
  df <- tribble(
    ~ID, ~IsAcademy,
    "A", 1,
    "A", 0,
    "A", 1,
    "B", 0,
    "B", 1,
    "B", 0,
    "C", 1,
    "C", 1,
    "C", 1
  )
  df_outcome <- df %>%
    summarise_academ(by = "ID", multiplier = FALSE)
  df_expected <- tribble(
    ~ID, ~Academies,
    "A", 2/3,
    "B", 1/3,
    "C", 1
  )
  expect_true(all.equal(df_outcome, df_expected))
})

test_that("`summarise_academ`: summarisation with `by_academisation_route`", {
  df <- tribble(
    ~ID, ~TypeAcademy,        ~IsAcademy,
    "A", "converter academy", 1,
    "A", "sponsored academy", 0,
    "A", "foobar",            1,
    "A", "converter academy", 0,
    "A", "converter academy", 1,
    "B", "sponsored academy", 1,
    "B", "sponsored academy", 1,
    "B", "converter academy", 1,
    "B", "converter academy", 1,
    "B", "foobar",            1
  )
  df_outcome <- df %>%
    summarise_academ(by = "ID", multiplier = FALSE,
                     by_academisation_route = TRUE)
  df_expected <- tribble(
    ~ID, ~TypeAcademy,        ~Academies,
    "A", "Sponsored Academy", 0,
    "B", "Sponsored Academy", 0.4,
    "A", "Converter Academy", 0.4,
    "B", "Converter Academy", 0.4
  )
  expect_true(all.equal(df_outcome, df_expected))
})

context("libs/common.R: `summarise_sen`")

test_that("`summarise_sen`: structure", {
  expected_names <- c("Year", "Phase", "SEN")
  names <- df_main %>%
    summarise_sen(by = c("Year", "Phase"),
                  by_sen_type = FALSE) %>%
    names()
  expect_equal(names, expected_names)
})

test_that("`summarise_sen`: structure with `by_sen_type`", {
  expected_names <- c("Year", "Phase", "TypeSEN", "SEN")
  names <- df_main %>%
    summarise_sen(by = c("Year", "Phase"),
                  by_sen_type = TRUE) %>%
    names()
  expect_equal(names, expected_names)
})

test_that("`summarise_sen`: multiplier", {
  prop_value <- df_main %>%
    summarise_sen(by = c("RegionCode"),
                  multiplier = FALSE) %$%
    SEN
  pct_value <- df_main %>%
    summarise_sen(by = c("RegionCode"),
                  multiplier = TRUE) %$%
    SEN
  expect_equal(prop_value * 100, pct_value)
})

test_that("`summarise_sen`: summarisation", {
  df <- tribble(
    ~ID, ~Group, ~TotalPupils, ~SEN_Support, ~Statement_EHC_Plan,
    1,   1,      100,          20,           5,
    2,   2,      30,           5,            2,
    3,   1,      150,          20,           10,
    4,   2,      120,          15,           5
  )
  test_that("sen_type: SEN_Support and Statement_EHC_Plan", {
    df_outcome <- df %>%
      summarise_sen(sen_type = c("SEN_Support", "Statement_EHC_Plan"),
                    by = "Group", by_sen_type = FALSE,
                    multiplier = FALSE)
    df_expected <- tribble(
      ~Group, ~SEN,
      1,      55/250,
      2,      27/150
    )
    expect_true(all.equal(df_outcome, df_expected))
  })
  test_that("sen_type: SEN_Support", {
    df_outcome <- df %>%
      summarise_sen(sen_type = c("SEN_Support"),
                    by = "Group", by_sen_type = FALSE,
                    multiplier = FALSE)
    df_expected <- tribble(
      ~Group, ~SEN,
      1,      40/250,
      2,      20/150
    )
    expect_true(all.equal(df_outcome, df_expected))
  })
  test_that("sen_type: EHC", {
    df_outcome <- df %>%
      summarise_sen(sen_type = c("Statement_EHC_Plan"),
                    by = "Group", by_sen_type = FALSE,
                    multiplier = FALSE)
    df_expected <- tribble(
      ~Group, ~SEN,
      1,      15/250,
      2,      7/150
    )
    expect_true(all.equal(df_outcome, df_expected))
  })
})

# TODO: break down routines in render_value_box.R
#       and render_primary.R to summarisation routines and rendering routines
