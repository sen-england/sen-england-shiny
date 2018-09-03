suppressMessages(suppressWarnings({
  library("testthat")
  library("tidyverse")
  library("magrittr")
  library("here")
}))

data_conf <- config::get("data")
send_db_conf <- data_conf$send_db
preproc_conf <- data_conf$preprocess
params <- config::get("params")

context("Test dataset loading")

test_that("DB exists", {
  expect_true(file.exists(here(send_db_conf$db)))
  expect_true(file.exists(here(preproc_conf$db)))
})

test_that("Table exists", {
  send_db_conn <- DBI::dbConnect(
    RSQLite::SQLite(),
    dbname = here(send_db_conf$db))
  preproc_db_conn <- DBI::dbConnect(
    RSQLite::SQLite(),
    dbname = here(preproc_conf$db))
  expect_true(send_db_conf$tbl %in% DBI::dbListTables(send_db_conn))
  expect_true(preproc_conf$stats_sen %in%
                DBI::dbListTables(preproc_db_conn))
  expect_true(preproc_conf$stats_schools %in%
                DBI::dbListTables(preproc_db_conn))
})

context("Test dataset structure")

send_db_conn <- DBI::dbConnect(
  RSQLite::SQLite(),
  dbname = here(send_db_conf$db))
preproc_db_conn <- DBI::dbConnect(
  RSQLite::SQLite(),
  dbname = here(preproc_conf$db))
df_main_table <- send_db_conn %>%
  tbl(send_db_conf$tbl) %>%
  select(one_of(send_db_conf$vars))
df_preproc_stats_sen <- preproc_db_conn %>%
  tbl(preproc_conf$stats_sen)
df_preproc_stats_schools <- preproc_db_conn %>%
  tbl(preproc_conf$stats_schools)

test_that("Structure of `df_main_table`", {
  df <- df_main_table %>% head() %>% collect()
  df_struct <- df %>% summarise_all(class) %>% gather()
  struct <- tribble(
    ~key,                 ~value,
    "ID",                 "character",
    "Year",               "integer",
    "URN",                "character",
    "SchoolName",         "character",
    "LACode",             "character",
    "LAName",             "character",
    "RegionCode",         "character",
    "RegionName",         "character",
    "ParlConCode",        "character",
    "ParlConName",        "character",
    "IsAcademy",          "integer",
    "TotalPupils",        "numeric",
    "SEN_Support",        "numeric",
    "Statement_EHC_Plan", "numeric",
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
    "TotalPupils",    "numeric",
    "TotalNumberSEN", "numeric")
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
