#### user defined variables ####
data_poles_path <- "../data/light_logger_metadata.xlsx"
# light_logger_data_vis_path <- "../output/light_logger_plots.pdf"
sliding_window_interval_seconds = 300

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
    
    
    
    
    # 
    # for(file_path in file_paths){
    
    
    
    data <- 
      read_csv(file_path, skip = 1) %>%
      clean_names() %>%
      filter(!if_any(everything(), ~ grepl("Logged", .))) %>%
      select(where(~ !all(is.na(.)))) %>%
      filter(!if_any(everything(), ~ is.na(.))) %>%
      slice(-n())
    
    first_date_time <- data$date_time_gmt_05_00[1]
    last_date_time <- data$date_time_gmt_05_00[nrow(data)]
    
    # Calculate the total time difference in seconds
    total_seconds <- 
      as.numeric(
        difftime(
          mdy_hms(last_date_time), 
          mdy_hms(first_date_time), 
          units = "secs"
        )
      )
    
    # Calculate the number of seconds between measurements
    # measurement_interval <- total_seconds / (nrow(data) - 1)
    
    tibble(
      first_date_time = mdy_hms(first_date_time),
      last_date_time = mdy_hms(last_date_time),
      measurement_interval = total_seconds / (nrow(data) - 1)
    )
    
    # }
    
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
        light_attenuation_pct = 100 * (intensity_lum_ft2_1 - intensity_lum_ft2_2) / intensity_lum_ft2_1
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
  filter(!is.na(lightlogger_file_path)) %>%
  #remove light logger data that has an issue
  filter(is.na(data_issue)) %>%
  select(-data_issue) %>%
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
  #filter(date == "2022-10-05") %>%
  left_join(
    data_disturbance_times,
    relationship = "many-to-many"
  ) %>% 
  # fix date_time that don't match, and minor measurment interval variations
  group_by(point_name, date) %>%
  mutate(last_date_time = min(last_date_time)) %>%
  mutate(first_date_time = max(first_date_time)) %>%
  mutate(measurement_interval = min(measurement_interval)) %>%
  ungroup() %>% 
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
    ),
    sliding_window_width = (sliding_window_interval_seconds/measurement_interval) + 1
  ) %>%
  # filter(date != "2022-06-03")
  filter(!str_detect(date, "2022\\-06")) #LR


