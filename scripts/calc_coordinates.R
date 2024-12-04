#### user defined variables ####
data_coordinates_path <- "../data/light_pole_metadata.xlsx"

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

data_coordinates <-
  read_excel(
    data_coordinates_path,
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
    # distance_m = distance_from_disturbance_ft*0.3048,
    date = ymd(date),
    point_name = str_to_upper(point_name),
    angle_radians = angle_degrees * pi / 180,
    # https://chatgpt.com/share/8c1d97f4-8564-42e1-ae35-d11c47f0f473
    coord_y =
      case_when(
        is.na(coord_y) ~
          (reference_coord_y + distance_from_disturbance_ft * sin(angle_radians)),
        is.na(coord_x) ~
          (reference_coord_y + distance_from_disturbance_ft * sin(angle_radians)),
        TRUE ~
          coord_y
      ),
    coord_x =
      case_when(
        is.na(coord_x) ~
          # -(reference_coord_x + distance_from_disturbance_ft * cos(angle_radians)),
          reference_coord_x - (distance_from_disturbance_ft * cos(angle_radians)),
        TRUE ~
          coord_x
      )
  )

#### Save Coordinates to RData file ####
save(data_coordinates, file = "../data/data_coordinates.RData")


