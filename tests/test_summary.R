suppressMessages(suppressWarnings({
  library("testthat")
  library("tidyverse")
  library("magrittr")
  library("here")
}))

source(here("libs/common.R"), local = TRUE)

data_conf <- config::get("data")
send_db_conf <- data_conf$send_db
preproc_conf <- data_conf$preprocess
params <- config::get("params")

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

dsb_id_primary <- "primary"
dsb_id_tseries <- "tseries"
dsb_id_maps <- "maps"
cand_years <- 2011L:2017L
cand_types <- c("% academised schools" = "Academisation",
                "% pupils with SEN" = "SEN")
cand_phases <- c("Primary schools" = "primary",
                 "Secondary schools" = "secondary",
                 "Others (e.g. nursery, 16 Plus, etc)" = "others")
cand_type_sen <- c("SEN_Support", "Statement_EHC_Plan") %>%
  set_names(str_replace_all(., "_", " "))
cand_type_schools <- c("Mainstream School" = "mainstream school",
                       "Pupil Referral Unit" = "pupil referral unit",
                       "Special School" = "special school",
                       "Others (e.g. independent school)" = "others")
cand_la_tbl <- read_csv(here("data/region-info/region-info.csv"),
                        col_types = c("cccc"))
cand_la <- cand_la_tbl %>%
  select(LAName, LACode) %>% distinct() %>% deframe()
cand_region <- cand_la_tbl %>%
  select(RegionName, RegionCode) %>% distinct() %>% deframe()
cand_parlcon <- read_csv(here("data/region-info/parlcon-info.csv"),
                         col_types = c("cc")) %>% deframe()

df_main <- df_main_table %>%
  filter(Phase %in% cand_phases) %>%
  filter(TypeGeneral %in% cand_type_schools) %>%
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
  df <- tibble(ID = c("A", "B", "C") %>% rep(each = 3),
               IsAcademy = c(c(1, 0, 1),
                             c(0, 1, 0),
                             c(1, 1, 1)))
  df_outcome <- df %>%
    summarise_academ(by = "ID", multiplier = FALSE)
  df_expected <- tibble(ID = c("A", "B", "C"),
                        Academies = c(2/3, 1/3, 1))
  expect_true(all.equal(df_outcome, df_expected))
})

test_that("`summarise_academ`: summarisation with `by_academisation_route`", {
  df <- tibble(
    ID = c("A", "B") %>% rep(each = 5),
    TypeAcademy_code = c(
      c("ca", "sa", "foobar", "ca", "ca"),
      c("sa", "sa", "ca", "ca", "foobar")),
    TypeAcademy = if_else(TypeAcademy_code == "ca", "converter academy",
                          if_else(TypeAcademy_code == "sa", "sponsored academy",
                                  TypeAcademy_code)),
    IsAcademy = c(c(1, 0, 1, 0, 1),
                  c(1, 1, 1, 1, 1)))
  df_outcome <- df %>%
    summarise_academ(by = "ID", multiplier = FALSE,
                     by_academisation_route = TRUE)
  df_expected <- tribble(
    ~ID, ~TypeAcademy,        ~Academies,
    "A", "Sponsored Academy", 0,
    "B", "Sponsored Academy", 0.4,
    "A", "Converter Academy", 0.4,
    "B", "Converter Academy", 0.4)
  expect_true(all.equal(df_outcome, df_expected))
})

# WIP
context("libs/common.R: `summarise_sen`")

# TODO: break down routines in render_value_box.R
#       and render_primary.R to summarisation routines and rendering routines
