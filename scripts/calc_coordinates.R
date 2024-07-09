#### user defined variables ####
data_path <- "../data/ltloggerdatacapturesummary_l.csv"

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
  read_csv(
    data_path,
    na = c(
      "na",
      "NA",
      "",
      "?"
    )
  ) %>%
  clean_names() %>%
  select(-starts_with("x")) %>%
  mutate(
    angle_radians = angle_degrees * pi / 180,
    # https://chatgpt.com/share/8c1d97f4-8564-42e1-ae35-d11c47f0f473
    coord_x =
      case_when(
        is.na(coord_x) ~
          -(reference_coord_x + distance_m * cos(angle_radians)),
        TRUE ~
          coord_x
      ),
    coord_y =
      case_when(
        is.na(coord_y) ~
          -(reference_coord_y + distance_m * sin(angle_radians)),
        TRUE ~
          coord_y
      )
  )
