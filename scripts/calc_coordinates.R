#### user defined variables ####
data_path_l <- "../data/ltloggerdatacapturesummary_l_ceb.xlsx"
data_path <- "../data/ltloggerdatacapturesummary_l_ceb.xlsx"

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
  read_excel(
    # data_path,
    data_path_l,
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
          -(reference_coord_x + distance_from_disturbance_ft * cos(angle_radians)),
        TRUE ~
          coord_x
      )
  )

#### Plot Coordinates ####

data %>%
  filter(date == "2022-06-03") %>%
  # filter(date == "2022-07-14") %>%
  # filter(date == "2022-07-18") %>%
  # filter(date == "2022-08-27") %>%
  # filter(date == "2022-09-10") %>%
  # filter(date == "2022-09-17") %>%
  # filter(date == "2022-09-25") %>%
  # filter(date == "2022-10-01") %>%
  # filter(date == "2022-10-05") %>%
  # filter(date == "2022-10-15") %>%
  # filter(date == "2022-11-02") %>%
  # filter(date == "2022-11-09") %>%
  ggplot() +
  aes(
    x = -coord_x,
    y = coord_y,
    color = distance_from_disturbance_ft,
    group = line_id
  ) +
  annotate("segment", x = -300, xend = 0, y = 0, yend = 0, linetype = "dashed", color = "grey") +
  annotate("segment", x = -300, xend = 0, y = 50, yend = 50, linetype = "dashed", color = "grey") +
  annotate("segment", x = -300, xend = 0, y = 100, yend = 100, linetype = "dashed", color = "grey") +
  annotate("segment", x = -300, xend = 0, y = 200, yend = 200, linetype = "dashed", color = "grey") +
  annotate("segment", x = 0, xend = 0, y = 0, yend = 200, linetype = "dashed", color = "grey") +
  annotate("segment", x = -100, xend = -100, y = 0, yend = 200, linetype = "dashed", color = "grey") +
  annotate("segment", x = -200, xend = -200, y = 0, yend = 200, linetype = "dashed", color = "grey") +
  annotate("segment", x = -300, xend = -300, y = 0, yend = 200, linetype = "dashed", color = "grey") +
  geom_line() +
  geom_point() +
  geom_text(
    aes(label = point_name), 
    vjust = -0.5, 
    hjust = 0.5, 
    size = 5
    ) +
  scale_color_gradient2(
    low = "lightblue",
    mid = "brown",
    high = "lightblue",
    midpoint = 0
  ) +
  theme_classic() +
  # facet_wrap(. ~ date + disturbance_id)
  facet_grid(date ~ disturbance_id)

