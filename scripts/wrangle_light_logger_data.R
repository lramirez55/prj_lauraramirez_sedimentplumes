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
read_light_intensity <- function(file_path, start_time, end_time) {
  if (is.na(file_path)) {
    return(NA)
  }
  
  data <- read_csv(file_path, skip = 1) %>%
    clean_names()
  
  data <- data %>%
    mutate(
      date_time = lubridate::mdy_hms(`date_time_gmt_05_00`, tz = "UTC")
    ) 
  
  # Filter data based on start_time and end_time
  filtered_data <- data %>%
    filter(date_time >= start_time & date_time < end_time)
  
  # Get light intensity values from the 4th column
  light_intensity <- as.numeric(filtered_data[[4]])
  
  # Return the mean light intensity as a representative value
  return(mean(light_intensity, na.rm = TRUE))
}

###################################################################################
# Run all data 

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
    logger_1_intensity = map2_dbl(logger_1, start_time, ~read_light_intensity(.x, .y, end_time)),
    logger_2_intensity = map2_dbl(logger_2, start_time, ~read_light_intensity(.x, .y, end_time))
  )

# Separate data for logger_1 and logger_2
data_logger_1 <- data_light_loggers_pivot %>%
  filter(!is.na(logger_1_intensity)) %>%
  select(point_name, start_time, logger_1, logger_1_intensity)

data_logger_2 <- data_light_loggers_pivot %>%
  filter(!is.na(logger_2_intensity)) %>%
  select(point_name, start_time, logger_2, logger_2_intensity)

# Join logger_1 and logger_2 data
paired_loggers <- data_logger_1 %>%
  full_join(data_logger_2, by = c("point_name", "start_time"))

#### Percent Light Attenuation Calculation ####
# Calculate light attenuation
paired_loggers <- paired_loggers %>%
  mutate(
    light_attenuation = 100 * (logger_2_intensity - logger_1_intensity) / logger_1_intensity
  )

#### Create date column for paired loggers #####
paired_loggers <- paired_loggers %>%
  mutate(
    date = as.Date(start_time)
  )

###################################################################################

#### Percent Light Attenuation ####
# Equation:
# 100 * (logger 2 - logger 1)/logger 1

#### Load Coordinate Data ####
# Load the coordinate data saved from calc_coordinates.R
#load("scripts/data_coordinates.RData")
# or you may need to go back up 1 directory depending on your working directory 
load("../scripts/data_coordinates.RData")

#### Merge Data with Coordinates ####
data_with_coordinates <- paired_loggers %>%
  left_join(data_coordinates, by = c("point_name", "date"))

###################################################################################
# All dates heatmap 

#### Create Heatmap ####
all_dates_heatmap <- ggplot(data_with_coordinates, aes(x = -coord_x, y = coord_y, color = light_attenuation)) +
  geom_point(size = 2) + 
  scale_color_gradient(low = "brown", high = "lightblue", na.value = NA) +
  scale_x_continuous(limits = c(-300, 0)) +
  labs(
    title = "Heatmap of Light Attenuation: All Dates",
    x = "X Coordinate",
    y = "Y Coordinate",
    fill = "% Light Attenuation"
  ) +
  theme_minimal()

# Print the heatmap
print(all_dates_heatmap)

###################################################################################
# Just a single date heatmap 

# Function to subset data by date and create heatmap
create_heatmap_for_date <- function(paired_loggers, data_coordinates, specific_date) {
  # Subset the dataframe to the specific date
  subset_data <- paired_loggers %>%
    filter(date == as.Date(specific_date))
  
  # Merge the subset with the coordinates data
  data_with_coordinates <- subset_data %>%
    left_join(data_coordinates, by = c("point_name", "date"))
  
  # Create heatmap
  one_date_heatmap <- ggplot(data_with_coordinates, aes(x = -coord_x, y = coord_y, color = light_attenuation)) +
    geom_point(size = 3) +    # change the number for "size" to change size of data points 
    geom_text(aes(label = point_name), vjust = 0.5, hjust = 2.0, size = 5) +
    scale_color_gradient(low = "brown", high = "lightblue", na.value = NA) +
    scale_x_continuous(limits = c(-300, 0)) +
    labs(
      title = paste("Heatmap of Light Attenuation for", specific_date),
      x = "X Coordinate",
      y = "Y Coordinate",
      fill = "% Light Attenuation"
    ) +
    theme_minimal()
  
  # Print the heatmap
  print(one_date_heatmap)
}

# Get all unique dates for heatmap 
date_0603 <- unique(paired_loggers$date)[1]
date_0714 <- unique(paired_loggers$date)[2]
date_0718 <- unique(paired_loggers$date)[3]
date_0827 <- unique(paired_loggers$date)[4]
date_0910 <- unique(paired_loggers$date)[5]
date_0917 <- unique(paired_loggers$date)[6]
date_0925 <- unique(paired_loggers$date)[7]
date_1001 <- unique(paired_loggers$date)[8]
date_1005 <- unique(paired_loggers$date)[9]
date_1015 <- unique(paired_loggers$date)[10]
date_1102 <- unique(paired_loggers$date)[11]

create_heatmap_for_date(paired_loggers, data_coordinates, date_1015)

