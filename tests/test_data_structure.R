suppressMessages(suppressWarnings({
  library("testthat")
  library("tidyverse")
  library("magrittr")
  library("here")
}))

data_conf <- config::get("data")
sen_db_conf <- data_conf$sen_db
preproc_conf <- data_conf$preprocess
params <- config::get("params")

context("Test dataset loading")

test_that("DB exists", {
  expect_true(file.exists(here(sen_db_conf$db)))
  expect_true(file.exists(here(preproc_conf$db)))
})

test_that("Table exists", {
  sen_db_conn <- DBI::dbConnect(
    RSQLite::SQLite(),
    dbname = here(sen_db_conf$db))
  preproc_db_conn <- DBI::dbConnect(
    RSQLite::SQLite(),
    dbname = here(preproc_conf$db))
  expect_true(sen_db_conf$tbl %in% DBI::dbListTables(sen_db_conn))
  expect_true(preproc_conf$stats_sen %in%
                DBI::dbListTables(preproc_db_conn))
  expect_true(preproc_conf$stats_schools %in%
                DBI::dbListTables(preproc_db_conn))
})

context("Test dataset structure")

sen_db_conn <- DBI::dbConnect(
  RSQLite::SQLite(),
  dbname = here(sen_db_conf$db))
preproc_db_conn <- DBI::dbConnect(
  RSQLite::SQLite(),
  dbname = here(preproc_conf$db))
df_main_table <- sen_db_conn %>%
  tbl(sen_db_conf$tbl) %>%
  select(one_of(sen_db_conf$vars))
df_preproc_stats_sen <- preproc_db_conn %>%
  tbl(preproc_conf$stats_sen)
df_preproc_stats_schools <- preproc_db_conn %>%
  tbl(preproc_conf$stats_schools)

test_that("Structure of `df_main_table`", {
  df <- df_main_table %>% head() %>% collect()
  df_struct <- df %>% summarise_all(class) %>% gather()
  struct <- tribble(
    ~key,                 ~value,
    "Year",               "integer",
    "URN",                "integer",
    "SchoolName",         "character",
    "LACode",             "character",
    "LAName",             "character",
    "RegionCode",         "character",
    "RegionName",         "character",
    "ParlConCode",        "character",
    "ParlConName",        "character",
    "IsAcademy",          "integer",
    "TotalPupils",        "integer",
    "SEN_Support",        "integer",
    "Statement_EHC_Plan", "integer",
    "TypeGeneral",        "character",
    "TypeAcademy",        "character",
    "Phase",              "character")
  expect_equal(df_struct, struct)
})

test_that("Structure of `df_preproc_stats_sen`", {
  df <- df_preproc_stats_sen %>% head() %>% collect()
  df_struct <- df %>% summarise_all(class) %>% gather()
  struct <- tribble(
    ~key,             ~value,
    "TypeGeneral",    "character",
    "Phase",          "character",
    "TypeSEN",        "character",
    "TotalPupils",    "integer",
    "TotalNumberSEN", "integer")
  expect_equal(df_struct, struct)
})

test_that("Structure of `df_preproc_stats_sen`", {
  df <- df_preproc_stats_schools %>% head() %>% collect()
  df_struct <- df %>% summarise_all(class) %>% gather()
  struct <- tribble(
    ~key,                 ~value,
    "TypeGeneral",        "character",
    "Phase",              "character",
    "TotalNumberSchools", "integer",
    "TotalNumberCA",      "integer",
    "TotalNumberSA",      "integer")
  expect_equal(df_struct, struct)
})
