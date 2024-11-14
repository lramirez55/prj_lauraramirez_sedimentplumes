#### user defined variables ####
data_coordinates_path <- "../data/ltloggerdatacapturesummary_l_ceb.xlsx"

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
    # data_path,
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
save(data_coordinates, file = "data_coordinates.RData")

#### Plot Coordinates ####

plot_light_data_for_dates <- 
  function(
    data, 
    dates
  ) {
    # Determine the number of rows and columns to plot (from number of dates)
    n <- length(dates)
    ncol <- ceiling(sqrt(n))
    nrow <- ceiling(n / ncol)
    
    # Set up grid for plotting 
    par(mfrow = c(nrow, ncol), mar = c(4, 4, 2, 1))
    
    for (target_date in dates) {
      plot_data <- 
        data %>%
        filter(date == ymd(target_date))
      
      p <- ggplot(plot_data) +
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
          vjust = -0.25, 
          hjust = 1, 
          size = 5
        ) +
        scale_color_gradient2(
          low = "lightblue",
          mid = "brown",
          high = "lightblue",
          midpoint = 0
        ) +
        theme_classic() +
        ggtitle(paste("Date:", date)) +
        facet_grid(date ~ disturbance_id)
      
      print(p)
    }
    
    # Reset the plotting area
    par(mfrow = c(1, 1))
  }

dates <- c("2022-06-03", "2022-07-14", "2022-07-18", "2022-08-27")
plot_light_data_for_dates(data_coordinates, dates)

data_coordinates %>%
  # filter(date == "2022-06-03") %>%
  # filter(date == "2022-07-14") %>%
  # filter(date == "2022-07-18") %>%
  # filter(date == "2022-08-27") %>%
  
  # filter(date == "2022-09-10") %>%
  # filter(date == "2022-09-17") %>% 
  # filter(date == "2022-09-25") %>% 
  # filter(date == "2022-10-01") %>%
  # filter(date == "2022-10-05") %>%
  #filter(date == "2022-10-15") %>% # missing x and a
  #filter(date == "2022-11-02") %>% # missing x and a
  filter(date == "2022-11-09") %>% # missing x and needs dist to dist redone
  
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
    vjust = -0.25, 
    hjust = 1, 
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
# 1

