#### user defined variables ####
data_path <- "../data/ltloggerdatacapturesummary.csv"

#### PACKAGES ####
packages_used <- 
  c("tidyverse", 
    "janitor",
    "readxl",
    "lubridate",
    "rstudioapi",
    "purrr")

packages_to_install <- 
  packages_used[!packages_used %in% installed.packages()[,1]]

if (length(packages_to_install) > 0) {
  install.packages(
    packages_to_install, 
    Ncpus = 1
    # Ncpus = Sys.getenv("NUMBER_OF_PROCESSORS") - 1
  )
}

lapply(packages_used, 
       require, 
       character.only = TRUE)

#### SetWD ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### READ IN DATA ####

data <-
  read_csv(data_path) %>%
  clean_names() %>%
  select(-starts_with("x")) %>%
  mutate(
    # https://chatgpt.com/share/8c1d97f4-8564-42e1-ae35-d11c47f0f473
    coordx =
      case_when(
        is.na(coordx) & !is.na(coordy) ~
          type more here,
        TRUE ~
          coordx
      )
  )
