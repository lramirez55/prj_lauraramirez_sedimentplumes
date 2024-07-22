#### user defined variables ####
data_poles_path <- "../data/pole_legend.xlsx"

#### PACKAGES ####
packages_used <- 
  c("tidyverse", 
    "janitor",
    "readxl",
    "lubridate",
    "rstudioapi",
    "purrr",
    "furrr")

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

#### FUNCTION TO EXTRACT DATE TIME ####
extract_date_times <- 
  function(file_path) {
  data <- 
    read_csv(file_path, skip = 1) %>%
    clean_names()
  
  first_date_time <- data$date_time_gmt_05_00[1]
  last_date_time <- data$date_time_gmt_05_00[nrow(data)]
  
  tibble(
    first_date_time = mdy_hms(first_date_time),
    last_date_time = mdy_hms(last_date_time)

  )
}

#### READ IN DATA ####

plan(multisession, workers = parallel::detectCores())

data_light_logger_deployments <-
  read_excel(
    # data_path,
    data_poles_path,
    sheet = "data_lightloggers",
    na = c(
      "na",
      "NA",
      "",
      "?"
    )
  ) %>%
  clean_names() %>%
  mutate(
    date = ymd(date),
    lightlogger_file_path = str_replace(
      lightlogger_file_path,
      "^",
      "../"
    )
  ) %>%
  mutate(
    lightlogger_data = future_map(lightlogger_file_path, extract_date_times)
  ) %>%
  unnest(cols = c(lightlogger_data)) %>%
  arrange(
    date,
    point_name,
    logger_number_per_pole,
    first_date_time
  )



data_disturbance_times <-
  read_excel(
    # data_path,
    data_poles_path,
    sheet = "disturbance_periods",
    na = c(
      "na",
      "NA",
      "",
      "?"
    )
  ) %>%
  clean_names() %>%
  mutate(
    date = ymd(date),
    start_time = 
      ymd_hms(
        str_c(
          date, 
          format(
            start_time, 
            "%H:%M:%S"
          )
        )
      ),
    end_time = 
      ymd_hms(
        paste(
          date, 
          format(
            end_time, 
            "%H:%M:%S"
          )
        )
      )
  )

data_light_loggers <-
  data_light_logger_deployments %>%
  left_join(
    data_disturbance_times
  )

#### FUNCTION TO READ LIGHT INTENSITY ####
read_light_intensity <- function(file_path) {
  if (is.na(file_path)) {
    return(NA)
  }
  
  data <- read_csv(file_path, skip = 2) %>%
    clean_names()
  
  # Extract the light intensity values from the column "intensity_lum_ft2"
  light_intensity <- data$intensity_lum_ft2
  
  # Return the mean light intensity as a representative value
  return(mean(light_intensity, na.rm = TRUE))
}

#### Pivot Data to Get Logger 1 and Logger 2 ####
data_light_loggers_pivot <- data_light_loggers %>%
  pivot_wider(
    names_from = logger_number_per_pole,
    values_from = lightlogger_file_path,
    names_prefix = "logger_"
  )

#### Extract Light Intensity Values ####
data_light_loggers_pivot <- data_light_loggers_pivot %>%
  mutate(
    logger_1_intensity = map_dbl(logger_1, read_light_intensity),
    logger_2_intensity = map_dbl(logger_2, read_light_intensity)
  )

#### Percent Light Attenuation Calculation ####
data_light_loggers_pivot <- data_light_loggers_pivot %>%
  mutate(
    light_attenuation = 100 * (logger_2_intensity - logger_1_intensity) / logger_1_intensity
  )


#### Percent Light Attenuation ####
# Equation:
# 100 * (logger 2 - logger 1)/logger 1

# Note: this code assumes 'logger_1' and 'logger_2' are the columns 
# representing light readings from two loggers
data_light_loggers <- data_light_loggers %>%
  mutate(
    light_attenuation = 100 * (logger_2 - logger_1) / logger_1
  )

#### Load Coordinate Data ####
# Load the coordinate data saved from calc_coordinates.R
load("scripts/data_coordinates.RData")

#### Merge Data with Coordinates ####
data_with_coordinates <- data_light_loggers %>%
  left_join(data_coordinates, by = "point_name")

#### Create Heatmap ####
heatmap_plot <- ggplot(data_with_coordinates, aes(x = -coord_x, y = coord_y, fill = light_attenuation)) +
  geom_tile() +
  scale_fill_gradient(low = "blue", high = "red", na.value = NA) +
  labs(
    title = "Heatmap of Light Attenuation",
    x = "X Coordinate",
    y = "Y Coordinate",
    fill = "% Light Attenuation"
  ) +
  theme_minimal()

# Print the heatmap
print(heatmap_plot)
