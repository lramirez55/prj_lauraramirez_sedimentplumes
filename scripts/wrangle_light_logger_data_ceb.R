#### user defined variables ####
data_poles_path <- "../data/pole_legend.xlsx"
light_logger_data_vis_path <- "../output/light_logger_plots.pdf"

#### PACKAGES ####
packages_used <- 
  c("tidyverse", 
    "janitor",
    "readxl",
    "lubridate",
    "rstudioapi",
    "purrr",
    "furrr",
    "zoo")

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

#### FUNCTION TO READ LIGHT INTENSITY ####
read_light_intensity <- 
  function(
    file_path, 
    start_time, 
    end_time
  ) {
    if (is.na(file_path)) {
      return(NA)
    }
    
    data <- 
      read_csv(
        file_path, 
        skip = 1
      ) %>%
      clean_names() %>%
      # data <- data %>%
      mutate(
        date_time = lubridate::mdy_hms(`date_time_gmt_05_00`, tz = "UTC")
      ) %>%
      # Filter data based on start_time and end_time
      # filtered_data <- data %>%
      filter(date_time >= start_time & date_time < end_time)
    
    # Get light intensity values from the 4th column
    light_intensity <- as.numeric(filtered_data[[4]])
    
    # Return the mean light intensity as a representative value
    return(mean(light_intensity, na.rm = TRUE))
  }

#### FUNCTION TO CALCULATE LIGHT ATTENUATION ####

get_light_data <-
  function(
    file_path,
    start_time,
    end_time
  ){
    read_csv(
      file_path, 
      skip = 1
    ) %>%
      clean_names() %>%
      mutate(
        date_time = lubridate::mdy_hms(`date_time_gmt_05_00`, tz = "UTC")
      ) %>%
      filter(date_time >= lubridate::ymd_hms(start_time) & date_time < lubridate::ymd_hms(end_time)) 
    # dplyr::rename(
    #   temp_f_1 = starts_with("temp_f"),
    #   intensity_lum_ft2_1 = starts_with("intensity_lum_ft2")
    # )
  }

calc_light_attenuation <- 
  function(
    file_path_1,
    file_path_2,
    start_time, 
    end_time
  ) {
    
    if (is.na(file_path_1) || is.na(file_path_2)) {
      return(NA)
    }
    
    data_1 <- 
      get_light_data(
        file_path_1,
        start_time, 
        end_time
      ) %>%
      dplyr::rename(
        temp_f_1 = starts_with("temp_f"),
        intensity_lum_ft2_1 = starts_with("intensity_lum_ft2")
      )
    
    data_2 <- 
      get_light_data(
        file_path_2,
        start_time, 
        end_time
      ) %>%
      dplyr::rename(
        temp_f_2 = starts_with("temp_f"),
        intensity_lum_ft2_2 = starts_with("intensity_lum_ft2")
      )
    
    inner_join( # CEB perhaps inner_join would be better here
      data_1, 
      data_2
    ) %>%
      select(
        -starts_with("stopped_"),
        -starts_with("end_")
      ) %>%
      mutate(
        light_attenuation_pct = 100 * (intensity_lum_ft2_2 - intensity_lum_ft2_1) / intensity_lum_ft2_1
      ) %>%
      pull(light_attenuation_pct) %>%
      mean( 
        na.rm = TRUE
      )
  }


#### READ IN DATA ####

plan(multisession, workers = parallel::detectCores())

# read in the metadata for the light loggers
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
  filter(
    !str_detect(
      lightlogger_file_path,
      "deprecated"
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

# read in the metadata for the timing of the disturbance trials
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


# join the logger and disturbance metadata
plan(multisession, workers = parallel::detectCores())

data_light_loggers <-
  data_light_logger_deployments %>%
  left_join(
    data_disturbance_times,
    relationship = "many-to-many"
  ) %>%
  # make each row 1 pole, rather than 1 logger
  pivot_wider(
    names_from = logger_number_per_pole,
    values_from = 
      c(
        lightlogger_file_path,
        lightlogger_file_name,
        notes
      )
  )  %>%
  mutate(
    across(
      lightlogger_file_path_1:notes_3, 
      ~ map(
        .x, 
        ~ if(is.null(.x)) NA else .x)
    )
  ) %>%
  # calculate the light attenuation values
  # head(106) %>%
  mutate(
    light_attenuation_mean = 
      future_pmap_dbl(
        list(
          lightlogger_file_path_1, 
          lightlogger_file_path_2, 
          start_time,
          end_time
        ),
        ~calc_light_attenuation(
          ..1, 
          ..2, 
          ..3,
          ..4
        )
      ),
  ) %>%
  mutate(
    across(
      lightlogger_file_path_1:notes_3, 
      ~ unlist(.x)
    )
  ) %>%
  filter(date != "2022-06-03")


#### FUNCTION TO VISUALIZE LIGHT LOGGER DATA ####

visualize_light_logger_data <- 
  function(
    file_path_1,
    file_path_2,
    start_time, 
    end_time,
    date,
    point_name,
    disturbance_number,
    offset_time = 10, # Offset in minutes
    window_width = 5,
    scaling_factor=100 # controls the scale of the secondary y axis relative to the x axis
  ) {
    
    if (is.na(file_path_1) || is.na(file_path_2)) {
      return(NA)
    }
    
    # Adjust start_time and end_time with the offset
    adjusted_start_time <- start_time - lubridate::minutes(offset_time)
    adjusted_end_time <- end_time + lubridate::minutes(offset_time)
    
    data_1 <- 
      get_light_data(
        file_path_1,
        adjusted_start_time, 
        adjusted_end_time
      ) %>%
      dplyr::rename(
        temp_f_1 = starts_with("temp_f"),
        intensity_lum_ft2_1 = starts_with("intensity_lum_ft2")
      )
    
    data_2 <- 
      get_light_data(
        file_path_2,
        adjusted_start_time, 
        adjusted_end_time
      ) %>%
      dplyr::rename(
        temp_f_2 = starts_with("temp_f"),
        intensity_lum_ft2_2 = starts_with("intensity_lum_ft2")
      )
    
    data_12 <-
      inner_join( # CEB perhaps inner_join would be better here
        data_1, 
        data_2
      ) %>%
      select(
        -starts_with("stopped_"),
        -starts_with("end_")
      ) %>%
      mutate(
        intensity_lum_ft2_rollmean_1 = zoo::rollmean(intensity_lum_ft2_1, k = window_width, fill = NA, align = "center"),
        intensity_lum_ft2_rollmean_2 = zoo::rollmean(intensity_lum_ft2_2, k = window_width, fill = NA, align = "center"),
        light_attenuation_pct = 100 * (intensity_lum_ft2_2 - intensity_lum_ft2_1) / intensity_lum_ft2_1,
        light_attenuation_rollmean_pct = 100 * (intensity_lum_ft2_rollmean_2 - intensity_lum_ft2_rollmean_1) / intensity_lum_ft2_rollmean_1
      )
    
    data_12 %>%
      pivot_longer(
        cols = c(
          temp_f_1, 
          temp_f_2, 
          intensity_lum_ft2_1, 
          intensity_lum_ft2_2,
          intensity_lum_ft2_rollmean_1,
          intensity_lum_ft2_rollmean_2
        ),
        names_to = 
          c(
            ".value", 
            "logger_id"
          ),
        names_pattern = "(.*)_(\\d+)"
      ) %>%
      ggplot() +
      aes(
        x=date_time,
        y=intensity_lum_ft2,
        color = logger_id
      ) +
      geom_line(alpha = 0.5) +
      geom_line(
        aes(y = intensity_lum_ft2_rollmean), 
        # linetype = "dotted"
      ) +
      geom_point(
        aes(y = light_attenuation_pct * scaling_factor), 
        color = "black",
        shape = 1
      ) +
      geom_point(
        aes(y = light_attenuation_rollmean_pct * scaling_factor), 
        color = "black"
      ) +
      geom_vline(
        xintercept = as.numeric(start_time), 
        linetype = "dashed", 
        color = "green4") +
      geom_vline(
        xintercept = as.numeric(end_time), 
        linetype = "dashed", 
        color = "red4"
      ) +
      annotate("text", x = start_time - lubridate::minutes(1), y = 0, label = "Begin", hjust = 1, color = "green4") +
      annotate("text", x = end_time + lubridate::minutes(1), y = 0, label = "End", hjust = 0, color = "red4") +
      scale_y_continuous(
        name = "Intensity (lum/ft2)",
        sec.axis = sec_axis(~ . / scaling_factor, name = "Light Attenuation (%)")
      ) +
      theme_bw() +
      theme(
        axis.title.y.right = element_text(color = "black")
      ) +
      labs(
        title = str_c(
          date,
          ", Pole ",
          point_name,
          ", Disturbance ",
          disturbance_number,
          sep = ""
        )
      )
    
    # data_12 %>%
    #   ggplot() +
    #   aes(
    #     x=date_time,
    #     y=light_attenuation_pct
    #   ) +
    #   geom_point() +
    #   geom_smooth()
    
  }

#### Visualize Light Logger Data ####


plan(multisession, workers = parallel::detectCores())

plots_light_loggers <-
  data_light_loggers %>%
  # head(20) %>%
  select(
    lightlogger_file_path_1, 
    lightlogger_file_path_2, 
    start_time,
    end_time,
    date,
    point_name,
    disturbance_number
  ) %>%
  future_pmap(
    ~visualize_light_logger_data(
      file_path_1 = ..1, 
      file_path_2 = ..2, 
      start_time = ..3,
      end_time = ..4,
      date = ..5,
      point_name = ..6,
      disturbance_number = ..7,
      offset_time = 20,
      window_width = 5
    )
  )

plots_light_loggers[1]

pdf(light_logger_data_vis_path)
walk(plots_light_loggers, ~ if (!is.null(.x)) print(.x))
dev.off()

#### CEB STOPPED HERE ####

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
  #geom_tile() + 
  scale_color_gradient(low = "brown", high = "lightblue", na.value = NA, limits = c(-80, 100)) +
  #scale_fill_gradient(low = "brown", high = "lightblue", na.value = NA, limits = c(-80, 100)) +
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
  one_date_heatmap <- ggplot(data_with_coordinates, aes(x = -coord_x, y = coord_y, fill = light_attenuation)) +
    #geom_point(size = 3) +    # change the number for "size" to change size of data points 
    geom_tile() +
    geom_text(aes(label = point_name), vjust = 0.5, hjust = 2.0, size = 5) +
    #scale_color_gradient(low = "brown", high = "lightblue", na.value = NA, limits = c(-80, 100)) +
    scale_fill_gradient(low = "brown", high = "lightblue", na.value = NA, limits = c(-80, 100)) +
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

create_heatmap_for_date(paired_loggers, data_coordinates, date_0603)

create_heatmap_for_date(paired_loggers, data_coordinates, date_0714)

create_heatmap_for_date(paired_loggers, data_coordinates, date_0718)

create_heatmap_for_date(paired_loggers, data_coordinates, date_0827)

create_heatmap_for_date(paired_loggers, data_coordinates, date_0910)

create_heatmap_for_date(paired_loggers, data_coordinates, date_0917)

create_heatmap_for_date(paired_loggers, data_coordinates, date_0925)

create_heatmap_for_date(paired_loggers, data_coordinates, date_1001)

create_heatmap_for_date(paired_loggers, data_coordinates, date_1005)

create_heatmap_for_date(paired_loggers, data_coordinates, date_1015)

create_heatmap_for_date(paired_loggers, data_coordinates, date_1102)

