library("tidyverse")
library("magrittr")
library("glue")
library("here")

clean_sen_common <- function(df, Year = 2017L, replace_x = 1L) {
  #' Common cleaning routines for each cross-sectional data set.
  #' Some nessary steps need to be taken before invoking `clean_sen_common`,
  #' and the following variables need to be present in `df`:
  #' - URN
  #' - TotalPupils
  #' - SEN_Support
  #' - Statement_EHC_Plan

  df <- df %>%
    mutate(Year = Year) %>%
    mutate_if(is.character, clean_encoding) %>%
    mutate_all(clean_dot_to_na) %>%
    # count variables: replace "x" as 1L
    mutate_at(
      vars(TotalPupils, SEN_Support, Statement_EHC_Plan),
      clean_count, replace = replace_x) %>%
    select(Year, URN,
           TotalPupils, SEN_Support, Statement_EHC_Plan)

  return(df)
}

clean_encoding <- function(series) {
  #' Resolve encoding problems

  # single quote
  series <- series %>% str_replace_all("\x92", "'")
  return(series)
}

clean_dot_to_na <- function(series) {
  # replace ".." to NA in `series`
  ifelse(series == "..", NA, series)
}

clean_count <- function(series, replace = 1L) {
  # clean count variable
  # - replace "x" with `replace`
  # - replace "." with 0
  # - then convert them to integer
  ifelse(series == "x", replace,
         ifelse(series == ".", 0,
                as.integer(series)))
}

add_edubase_types <- function(df, df_edubase, cls_file) {
  add_cls <- function(labels, cls_groups, others_label) {
    new_labels <- rep(others_label, length(labels))
    for (group in cls_groups)
      new_labels[labels %in% group$candidates] = group$label
    return(new_labels)
  }
  cls_rules <- yaml::yaml.load_file(cls_file)
  df <- df %>%
    left_join(df_edubase %>%
                select(URN,
                       TypeSpecific = `TypeOfEstablishment (name)`,
                       Phase = `PhaseOfEducation (name)`),
              by = "URN") %>%
    mutate(TypeGeneral = TypeSpecific %>%
             add_cls(cls_rules$TypeGeneral$Classifications,
                     cls_rules$TypeGeneral$OthersLabel),
           TypeAcademy = TypeSpecific %>%
             add_cls(cls_rules$TypeAcademy$Classifications,
                     cls_rules$TypeAcademy$OthersLabel))
  df
}

add_school_info <- function(df, df_edubase, cls_file,
                            region_dict, la_lookup) {
  df %>%
    left_join(
      df_edubase %>%
        select(URN,
               SchoolName = EstablishmentName,
               LAName_sen = `LA (name)`,
               ParlConCode = `ParliamentaryConstituency (code)`,
               ParlConName = `ParliamentaryConstituency (name)`),
      by = c("URN")) %>%
    add_edubase_types(df_edubase, cls_file) %>%
    mutate(IsAcademy = as.integer(TypeAcademy %in% c("Converter Academy",
                                                     "Sponsored Academy"))) %>%
    mutate(Phase = if_else(Phase %in% c("Primary", "Middile deemed primary"),
                           "Primary",
                           if_else(Phase %in% c("Secondary",
                                                "Middled deemed secondary"),
                                   "Secondary",
                                   "Others"))) %>%
    left_join(la_lookup %>% read_csv() %>%
                select(LACode = Code, LAName = Name, LAName_sen = NameEdubase),
              by = c("LAName_sen")) %>%
    left_join(region_dict %>% read_csv() %>%
                select(RegionCode, LACode, RegionName, LAName),
              by = c("LAName", "LACode")) %>%
    select(-LAName_sen)
}

legacy_compatible <- function(df) {
  df %>% mutate_at(vars(TypeGeneral, TypeAcademy,
                        Phase),
                   str_to_lower)
}

main <- function() {
  df_edubase <- read_csv(here("data/edubase/",
                              "edubasealldata20181022.csv")) %T>%
    glimpse()

  sen_path <- here("data/send_data/SEN")
  na_pat <- c("", "NA", "..", "NULL")
  df_sen_2018 <- glue("{sen_path}-2018/SEN_2018_Underlying_Data.csv") %>%
    read_csv(na = na_pat) %T>% glimpse() %>%
    select(URN,
           TotalPupils = `Total pupils`,
           SEN_Support = `SEN support`,
           Statement_EHC_Plan = `Statement / EHC plan`) %>%
    clean_sen_common(Year = 2018L) %T>% glimpse()
  df_sen_2017 <- glue("{sen_path}-2017/SFR37_2017_UD.csv") %>%
    read_csv(na = na_pat) %T>% glimpse() %>%
    select(URN,
           TotalPupils = `Total pupils`,
           SEN_Support = `SEN support`,
           Statement_EHC_Plan = `Statement / EHC plan`) %>%
    clean_sen_common(Year = 2017L) %T>% glimpse()
  df_sen_2016 <- glue("{sen_path}-2016/SFR29_2016_UD.csv") %>%
    read_csv(na = na_pat) %T>% glimpse() %>%
    select(URN,
           TotalPupils = `Total Pupils`,
           SEN_Support = `SEN support`,
           Statement_EHC_Plan = `Statement/ EHC plan`) %>%
    clean_sen_common(Year = 2016L) %T>% glimpse()
  df_sen_2015 <- glue("{sen_path}-2015/SFR25_2015_UD.csv") %>%
    read_csv(na = na_pat) %T>% glimpse() %>%
    select(URN,
           TotalPupils = `Total Pupils`,
           SEN_Support = `SEN support`,
           Statement_EHC_Plan = `Statement/ EHC plan`) %>%
    clean_sen_common(Year = 2015L) %T>% glimpse()
  df_sen_2014 <- glue("{sen_path}-2014/SFR26_2014_UD.csv") %>%
    read_csv(na = na_pat) %T>% glimpse() %>%
    select(URN,
           TotalPupils = `Total Pupils`,
           SEN_Support = `No_Statement`,
           Statement_EHC_Plan = `Statement`) %>%
    clean_sen_common(Year = 2014L) %T>% glimpse()
  df_sen_2013 <- glue("{sen_path}-2013/SFR30-2013_UD.csv") %>%
    read_csv(na = na_pat) %T>% glimpse() %>%
    select(URN,
           TotalPupils = `Total Pupils`,
           SEN_Support = `No_Statement`,
           Statement_EHC_Plan = `Statement`) %>%
    clean_sen_common(Year = 2013L) %T>% glimpse()
  df_sen_2012 <- glue("{sen_path}-2012/SFR14_2012_UD.csv") %>%
    read_csv(na = na_pat) %T>% glimpse() %>%
    select(URN,
           TotalPupils = `Pupils`,
           SEN_Support = `No_statements`,
           Statement_EHC_Plan = `Statements`) %>%
    clean_sen_common(Year = 2012L) %T>% glimpse()
  df_sen_2011 <- glue("{sen_path}-2011/SFR14_2011_UD.csv") %>%
    read_csv(na = c(na_pat, ".")) %T>% glimpse() %>%
    mutate_at(vars(NoStatements, SchoolActionPlus, SchoolAction),
              clean_count) %>%
    mutate(NoStatements = (function(NoStatements,
                                    SchoolActionPlus, SchoolAction) {
      res <- NoStatements
      idx_na <- is.na(res) %>% which()
      res[idx_na] <- SchoolActionPlus[idx_na] + SchoolAction[idx_na]
      return(res)
    })(NoStatements, SchoolActionPlus, SchoolAction)) %>%
    select(URN,
           TotalPupils = `Pupils`,
           SEN_Support = `NoStatements`,
           Statement_EHC_Plan = `Statements`) %>%
    clean_sen_common(Year = 2011L) %T>% glimpse()
  df_sen <- list(df_sen_2011, df_sen_2012, df_sen_2013, df_sen_2014,
                 df_sen_2015, df_sen_2016, df_sen_2017, df_sen_2018) %>%
    reduce(bind_rows) %>%
    add_school_info(df_edubase,
                    cls_file = here("data/data-dictionaries/",
                                    "institution-classifications-edubase.yml"),
                    region_dict = here("data/data-dictionaries/",
                                       "region-info.csv"),
                    la_lookup = here("data/data-dictionaries/",
                                     "la-2011-lookup.csv")) %>%
    legacy_compatible() %T>% glimpse()

  data_conf <- config::get("data")
  sen_db_conf <- data_conf$sen_db
  sen_db_conn <- DBI::dbConnect(
    RSQLite::SQLite(),
    dbname = here(sen_db_conf$db))
  df_sen %>% DBI::dbWriteTable(conn = sen_db_conn,
                               name = sen_db_conf$tbl,
                               overwrite = TRUE)
}

main()
