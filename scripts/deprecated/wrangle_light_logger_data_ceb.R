#### user defined variables ####
data_poles_path <- "../data/pole_legend.xlsx"
light_logger_data_vis_path <- "../output/light_logger_plots.pdf"
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
        light_attenuation_pct = 100 * (intensity_lum_ft2_1 - intensity_lum_ft2_2) / intensity_lum_ft2_1,
        light_attenuation_rollmean_pct = 100 * (intensity_lum_ft2_rollmean_1 - intensity_lum_ft2_rollmean_2) / intensity_lum_ft2_rollmean_1
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
      ylim(-2000,NA) +
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
    disturbance_number,
    sliding_window_width #LR
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
      # window_width = 5 
      window_width = ..8 #LR
    )
  )

plots_light_loggers[1]

pdf(
  str_c(
    dirname(light_logger_data_vis_path),
    "/",
    sliding_window_interval_seconds,
    "s_",
    basename(light_logger_data_vis_path),
    sep = ""
  )
)
walk(plots_light_loggers, ~ if (!is.null(.x)) print(.x))
dev.off()


#### Process Data for Disturbance Locations by Zone and Track ####

# CEB note that there is a coordinate for every logger, but data_light_loggers has 1 row per pole per disturbance
source("../scripts/calc_coordinates.R")
#load("../data/data_coordinates.RData")

# get disturbances

disturbance_locations <-
  data_coordinates %>% 
  select(
    coord_x, 
    coord_y, 
    point_name, 
    date, 
    disturbance_number = disturbance_id
  ) %>%
  filter(point_name == "X") %>%
  filter(!str_detect(date, "2022\\-06")) %>%
  filter(!str_detect(date, "2022\\-11\\-09")) %>%
  mutate(
    track =
      case_when(
        coord_x < 0 ~ "U",
        coord_x >= 0 & coord_x < 100 ~ "3",
        coord_x >= 100 & coord_x < 200 ~ "2",
        coord_x >=200 & coord_x <= 300 ~ "1"
      )  %>%
      factor(.,levels = c("1",
                          "2",
                          "3",
                          'U')),
    zone =
      case_when(
        coord_y < 0 ~ "U",
        coord_y >= 0 & coord_y < 50 ~ "A",
        coord_y >= 50 & coord_y < 100 ~ "B",
        coord_y >= 100 & coord_y <= 200 ~ "C"
      ) %>%
      factor(.,levels = c("U",
                          "A",
                          "B",
                          "C"))
  ) %>%
  # define the begin and end coordinate for each disturbance (row)
  mutate(
    x = 
      case_when(
        coord_x == 0 ~ 3.5,
        coord_x == 100 ~ 2.5,
        coord_x == 50 ~ 2.5,
        coord_x == 150 ~ 1.5,
        coord_x == 250 ~ 0.5,
        TRUE ~ NA_real_
      ),
    xend = 
      case_when(
        coord_x == 0 ~ 3.5,
        coord_x == 100 ~ 2.5,
        coord_x == 50 ~ 3.5,
        coord_x == 150 ~ 2.5,
        coord_x == 250 ~ 1.5,
        TRUE ~ NA_real_
      ),
    y = 
      case_when(
        coord_y == 0 ~ 1.5,
        coord_y == 100 ~ 3.5,
        coord_y == 25 ~ 1.5,
        coord_y == 75 ~ 2.5,
        coord_y == 150 ~ 3.5,
        TRUE ~ NA_real_
      ),
    yend = 
      case_when(
        coord_y == 0 ~ 1.5,
        coord_y == 100 ~ 3.5,
        coord_y == 25 ~ 2.5,
        coord_y == 75 ~ 3.5,
        coord_y == 150 ~ 4.5,
        TRUE ~ NA_real_
      )
  )

#### Join the Light Logger Attenuation data With their Spatial Locations ####

# and convert to the zone and track categorization

data_combined <- 
  data_light_loggers %>%
  # group_by(
  #   point_name, 
  #   date, 
  #   disturbance_number
  # ) %>%
  # summarise(
  #   light_attenuation_mean_mean = 
  #     mean(
  #       light_attenuation_mean, 
  #       na.rm = T
  #     )
  # ) %>%
  left_join(
    data_coordinates %>% 
      select(
        coord_x, 
        coord_y, 
        point_name, 
        date, 
        disturbance_number = disturbance_id
      )
  ) %>%
  ungroup() %>%
  distinct() %>%
  filter(!is.na(coord_x)) %>%
  filter(!is.na(coord_y)) %>%
  filter(!is.na(light_attenuation_mean)) %>%
  
  # each pole that is placed on a track or zone border should be duplicated and
  # the x or y coordinate should be incremented up in copy and down in the other
  # so that it will contribute to two or more cells
  
  # Duplicate rows where coord_x is 0, 100, 200, or 300
  mutate(
    #duplicate_flag = coord_x %in% c(0, 100, 200, 300)
    duplicate_flag =
      case_when(
        coord_x > -1 & coord_x < 1 ~ TRUE,
        coord_x > 99 & coord_x < 101 ~ TRUE,
        coord_x > 199 & coord_x < 201 ~ TRUE,
        coord_x > 299 & coord_x < 301 ~ TRUE,  #CEB problem?
        TRUE ~ FALSE
      )
  ) %>%
  bind_rows(
    filter(., duplicate_flag) %>%
      mutate(coord_x = coord_x + 2,
             duplicate_flag = FALSE)
  ) %>%
  mutate(
    coord_x = if_else(duplicate_flag, coord_x - 2, coord_x)
  ) %>%
  select(-duplicate_flag) %>%
  
  # Duplicate rows where coord_y is 0, 50, 100, or 200
  mutate(
    #duplicate_flag = coord_x %in% c(0, 100, 200, 300)
    duplicate_flag =
      case_when(
        coord_y > -1 & coord_y < 1 ~ TRUE,
        coord_y > 49 & coord_y < 51 ~ TRUE,
        coord_y > 99 & coord_y < 101 ~ TRUE,
        coord_y > 199 & coord_y < 201 ~ TRUE,  #CEB Problem?
        TRUE ~ FALSE
      )
  ) %>%
  bind_rows(
    filter(., duplicate_flag) %>%
      mutate(coord_y = coord_y + 2,
             duplicate_flag = FALSE)
  ) %>%
  mutate(
    coord_y = if_else(duplicate_flag, coord_y - 2, coord_y)
  ) %>%
  select(-duplicate_flag) %>%
  
  #remove duplicated poles outside of study area
  filter(coord_y <=200,
         coord_x <= 300) %>% 
  
  # Classify the poles by track and zone
  mutate(
    track =
      case_when(
        coord_x < 0 ~ "U",
        coord_x >= 0 & coord_x < 100 ~ "3",
        coord_x >= 100 & coord_x < 200 ~ "2",
        coord_x >=200 & coord_x <= 300 ~ "1"
      )  %>%
      factor(.,levels = c("1",
                          "2",
                          "3",
                          'U')),
    zone =
      case_when(
        coord_y < 0 ~ "U",
        coord_y >= 0 & coord_y < 50 ~ "A",
        coord_y >= 50 & coord_y < 100 ~ "B",
        coord_y >= 100 & coord_y <= 200 ~ "C"
      ) %>%
      factor(.,levels = c("U",
                          "A",
                          "B",
                          "C"))
  ) %>%
  group_by(
    date,
    disturbance_number,
    track,
    zone
  ) %>%
  summarize(
    mean_light_attenuation = mean(light_attenuation_mean, na.rm=TRUE),
    sd_light_attenuation = sd(light_attenuation_mean, na.rm=TRUE),
    n = n()
  ) %>%
  filter(mean_light_attenuation >= 0)

#### Store Track and Zone Boundaries into a File for Plotting ####
# Create a complete grid of all track and zone combinations
grid_data <- 
  expand.grid(
    track = levels(data_combined$track),
    zone = levels(data_combined$zone)
  ) %>%
  filter(track != "U") %>%
  filter(zone != "U")



#### Make attenuation heatmap for all disturbances on all days ####

heatmap_all <-
  data_combined %>%
  ggplot() +
  aes(
    x = track,
    y=zone, 
    fill = mean_light_attenuation
  ) +
  geom_tile() +
  theme_classic() +
  scale_fill_gradientn(
    colors = c("yellow", "white", "black"),
    # values = scales::rescale(c(-100, 0, 100)),
    values = c(0, 0.5, 1),  # Relative positions for -100, 0, 100
    limits = c(-100, 100),  # Explicit limits for the color scale
    name = "Mean Light\nAttenuation"
  ) +
  # # add number of logger poles to the plot
  # geom_text(
  #   aes(label = n), 
  #   color = "black", 
  #   size = 3
  #   ) +  # Add n value at the center of each tile
  # add mean attenuation to the plot
  geom_text(
    aes(label = 
          round(mean_light_attenuation)
    ), 
    color = "black", 
    size = 3
  ) +  # Add n value at the center of each tile
  facet_grid(disturbance_number ~ date)+
  labs(
    title = "Mean Light Attenuation by Day & Disturbance",
    x = "Track",
    y = "Zone"
  ) +
  geom_rect(
    data = grid_data,  # Use the full grid
    aes(
      xmin = as.numeric(track) - 0.5,
      xmax = as.numeric(track) + 0.5,
      ymin = as.numeric(zone) - 0.5,
      ymax = as.numeric(zone) + 0.5
    ),
    inherit.aes = FALSE,  # Prevent inheriting fill and other aesthetics
    fill = NA,
    color = "black",
    linetype = "dashed"
  ) +
  geom_segment(
    data = disturbance_locations,  # Use the full grid
    aes(
      #x = as.numeric(track) - 0.5,
      #y = as.numeric(zone) + 0.5,
      #xend = as.numeric(track) + 0.5,
      #yend = as.numeric(zone) + 0.5
      x = x,
      y = y,
      xend = xend,
      yend = yend
    ),
    inherit.aes = FALSE,  # Prevent inheriting fill and other aesthetics
    color = "brown",
    size = 2
    # linetype = "dashed"
  )
#geom_text(aes(label=cnt), color='red') +

heatmap_all

#### heatmap, avg across disturbances ####
# heat map by track and zone and day

heatmap_day <-
  data_combined %>%
  ungroup() %>%
  group_by(
    date,
    track,
    zone
  ) %>%
  summarize(
    mean_light_attenuation = mean(mean_light_attenuation, na.rm=TRUE),
    sd_light_attenuation = sd(sd_light_attenuation, na.rm=TRUE),
    n = n()
  ) %>%
  ggplot() +
  aes(
    x = track,
    y=zone, 
    fill = mean_light_attenuation
  ) +
  geom_tile() +
  theme_bw() +
  scale_fill_gradientn(
    colors = c("yellow", "white", "black"),
    # values = scales::rescale(c(-100, 0, 100)),
    values = c(0, 0.5, 1),  # Relative positions for -100, 0, 100
    limits = c(-100, 100),  # Explicit limits for the color scale
    name = "Mean Light\nAttenuation"
  ) +
  facet_wrap(. ~ date) +
  labs(
    title = "Mean Light Attenuation by Day"
  ) +
  geom_rect(
    data = grid_data,  # Use the full grid
    aes(
      xmin = as.numeric(track) - 0.5,
      xmax = as.numeric(track) + 0.5,
      ymin = as.numeric(zone) - 0.5,
      ymax = as.numeric(zone) + 0.5
    ),
    inherit.aes = FALSE,  # Prevent inheriting fill and other aesthetics
    fill = NA,
    color = "black",
    linetype = "dashed"
  ) +
  geom_segment(
    data = disturbance_locations,  # Use the full grid
    aes(
      #x = as.numeric(track) - 0.5,
      #y = as.numeric(zone) + 0.5,
      #xend = as.numeric(track) + 0.5,
      #yend = as.numeric(zone) + 0.5
      x = x,
      y = y,
      xend = xend,
      yend = yend
    ),
    inherit.aes = FALSE,  # Prevent inheriting fill and other aesthetics
    color = "brown",
    size = 2
    # linetype = "dashed"
  )

heatmap_day

#### Heatmap Avg across days ####
# heat map by track and zone 
heatmap_avg <-
data_combined %>%
  group_by(
    date,
    track,
    zone
  ) %>%
  summarize(
    mean_light_attenuation = mean(mean_light_attenuation, na.rm=TRUE),
    sd_light_attenuation = sd(sd_light_attenuation, na.rm=TRUE)
  ) %>%
  group_by(
    track,
    zone
  ) %>%
  summarize(
    mean_light_attenuation = mean(mean_light_attenuation, na.rm=TRUE),
    sd_light_attenuation = sd(sd_light_attenuation, na.rm=TRUE)
  ) %>%
  ggplot() +
  aes(
    x = track,
    y=zone, 
    fill = mean_light_attenuation
  ) +
  geom_tile() +
  theme_bw() +
  scale_fill_gradientn(
    colors = c("yellow", "white", "black"),
    # values = scales::rescale(c(-100, 0, 100)),
    values = c(0, 0.5, 1),  # Relative positions for -100, 0, 100
    limits = c(-100, 100),  # Explicit limits for the color scale
    name = "Mean Light\nAttenuation"
  ) +
  labs(
    title = "Mean Light Attenuation Across Whole Experiment"
  ) +
  geom_rect(
    data = grid_data,  # Use the full grid
    aes(
      xmin = as.numeric(track) - 0.5,
      xmax = as.numeric(track) + 0.5,
      ymin = as.numeric(zone) - 0.5,
      ymax = as.numeric(zone) + 0.5
    ),
    inherit.aes = FALSE,  # Prevent inheriting fill and other aesthetics
    fill = NA,
    color = "black",
    linetype = "dashed",
    size = 1
  ) +
  geom_segment(
    data = disturbance_locations,  # Use the full grid
    aes(
      #x = as.numeric(track) - 0.5,
      #y = as.numeric(zone) + 0.5,
      #xend = as.numeric(track) + 0.5,
      #yend = as.numeric(zone) + 0.5
      x = x,
      y = y,
      xend = xend,
      yend = yend
    ),
    inherit.aes = FALSE,  # Prevent inheriting fill and other aesthetics
    color = "brown",
    size = 2
    # linetype = "dashed"
  )

heatmap_avg

#### save heatmaps to pdf ####

pdf("../output/heatmaps.pdf")

heatmap_all
heatmap_day
heatmap_avg

dev.off()



#### ceb stopped here #####
data_combined %>%
  filter(date == '2022-10-01', disturbance_number ==1) %>%
  ggplot(aes(x=coord_x, y=coord_y, fill=light_attenuation_mean_mean)) + geom_tile() +
  theme_light() +
  scale_x_reverse()


data_combined %>%
  ungroup() %>%
  summarise(min(light_attenuation_mean_mean, na.rm = T), max(light_attenuation_mean_mean, na.rm = T))

data_combined %>%
  drop_na(light_attenuation_mean_mean) %>% # drop nan attenuation
  ggplot(aes(x=coord_x, y=coord_y, color=light_attenuation_mean_mean)) +  
  annotate('rect',xmin=0, xmax=100, ymin=0, ymax=200, alpha=0,color='black' ) +
  annotate('rect',xmin=200, xmax=300, ymin=0, ymax=200, alpha=0,color='black' ) +
  annotate('rect',xmin=0, xmax=300, ymin=0, ymax=50, alpha=0,color='black' ) +
  annotate('rect',xmin=0, xmax=300, ymin=100, ymax=200, alpha=0,color='black' ) +
  annotate("text", x = 320, y = 25, label = "A", size = 3, color = "black") +
  annotate("text", x = 50, y = 215, label = "1", size = 3, color = "black") +
  
  annotate("text", x = 320, y = 75, label = "B", size = 3, color = "black") +
  annotate("text", x = 150, y = 215, label = "2", size = 3, color = "black") +
  
  annotate("text", x = 320, y = 150, label = "C", size = 3, color = "black") +
  annotate("text", x = 250, y = 215, label = "3", size = 3, color = "black") +
  
  geom_point(size=3, pch=15) +
  facet_grid(cols = vars(date), rows= vars(disturbance_number )) +
  scale_colour_distiller(direction =1, palette = 'BrBG') +
  xlab('Width (ft)') + ylab('Length (ft)') + theme_light() +
  scale_y_continuous(breaks=c(0, 50, 100, 200)) +
  scale_x_reverse(limits=c(335,0)) + # reverse x-axis
  theme(
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank()   # Remove minor gridlines
  )


data_combined %>%
  ggplot(aes(x=coord_x, y=coord_y, color=light_attenuation_mean_mean)) + geom_tile() + 
  facet_grid(cols = vars(date), rows= vars(disturbance_number ), scales='free')


data_combined %>%
  mutate(coord_x = round(coord_x/100)*100, 
         coord_y = round(coord_y/100)*100) %>%
  count(coord_x, coord_y) %>%
  ggplot(aes(x=coord_x, y=coord_y, fill=n)) + geom_tile() +
  geom_text(aes(label=n)) +
  theme_light() + 
  scale_x_reverse() # reverse x-axis


x_vals <- c(0, 100, 200, 300)
y_vals <- c(0, 50, 100, 200)
res <- data.frame()
for (i in 1:(length(x_vals)-1)){
  for (j in 1:(length(y_vals)-1)){
    res <- res %>%
      bind_rows(data.frame(x = i, y= j, cnt = sum(between(data_combined$coord_x, x_vals[i], x_vals[i+1]) & 
                                                    between(data_combined$coord_y, y_vals[j], y_vals[j+1]) , na.rm = T)))
  }
}

res %>%
  ggplot(aes(x=x, y=y, fill=cnt)) + geom_tile() + 
  geom_text(aes(label=cnt), color='red') +
  theme_light() + 
  scale_x_reverse(breaks=c(1,2,3), labels=c(50, 150, 250)) +
  scale_y_continuous(breaks=c(1,2,3), labels=c(25, 75, 150)) + #edit to have 
  xlab('Width (ft)') + ylab('Length (ft)') + 
  ggtitle('Percent Light Attenuation Heatmap') +
  theme(plot.title = element_text(hjust=0.5)) +
  scale_fill_gradient(low = "lightblue", high = "darkblue")

#### CEB STOPPED HERE ####

load("data_coordinates.RData")

######################################################################
######################################################################
load("data_coordinates.RData")

data_combined <- 
  data_light_loggers %>%
  group_by(
    point_name, 
    date, 
    disturbance_number
  ) %>%
  summarise(
    light_attenuation_mean_mean = mean(
      light_attenuation_mean, 
      na.rm = T
    )
  ) %>%
  left_join(data_coordinates %>% 
              select(coord_x, coord_y, point_name, date, disturbance_number = disturbance_id)) %>%
  ungroup()

data_combined %>%
  filter(date == '2022-10-01', disturbance_number ==1) %>%
  ggplot(aes(x=coord_x, y=coord_y, fill=light_attenuation_mean_mean)) + geom_tile() +
  theme_light() +
  scale_x_reverse()


data_combined %>%
  ungroup() %>%
  summarise(min(light_attenuation_mean_mean, na.rm = T), max(light_attenuation_mean_mean, na.rm = T))

data_combined %>%
  drop_na(light_attenuation_mean_mean) %>% # drop nan attenuation
  ggplot(aes(x=coord_x, y=coord_y, color=light_attenuation_mean_mean)) +  
  annotate('rect',xmin=0, xmax=100, ymin=0, ymax=200, alpha=0,color='black' ) +
  annotate('rect',xmin=200, xmax=300, ymin=0, ymax=200, alpha=0,color='black' ) +
  annotate('rect',xmin=0, xmax=300, ymin=0, ymax=50, alpha=0,color='black' ) +
  annotate('rect',xmin=0, xmax=300, ymin=100, ymax=200, alpha=0,color='black' ) +
  annotate("text", x = 320, y = 25, label = "A", size = 3, color = "black") +
  annotate("text", x = 50, y = 215, label = "1", size = 3, color = "black") +
  
  annotate("text", x = 320, y = 75, label = "B", size = 3, color = "black") +
  annotate("text", x = 150, y = 215, label = "2", size = 3, color = "black") +
  
  annotate("text", x = 320, y = 150, label = "C", size = 3, color = "black") +
  annotate("text", x = 250, y = 215, label = "3", size = 3, color = "black") +
  
  geom_point(size=3, pch=15) +
  facet_grid(cols = vars(date), rows= vars(disturbance_number )) +
  scale_colour_distiller(direction =1, palette = 'BrBG') +
  xlab('Width (ft)') + ylab('Length (ft)') + theme_light() +
  scale_y_continuous(breaks=c(0, 50, 100, 200)) +
  scale_x_reverse(limits=c(335,0)) + # reverse x-axis
  theme(
    panel.grid.major = element_blank(),  # Remove major gridlines
    panel.grid.minor = element_blank()   # Remove minor gridlines
  )


data_combined %>%
  ggplot(aes(x=coord_x, y=coord_y, color=light_attenuation_mean_mean)) + geom_tile() + 
  facet_grid(cols = vars(date), rows= vars(disturbance_number ), scales='free')


data_combined %>%
  mutate(coord_x = round(coord_x/100)*100, 
         coord_y = round(coord_y/100)*100) %>%
  count(coord_x, coord_y) %>%
  ggplot(aes(x=coord_x, y=coord_y, fill=n)) + geom_tile() +
  geom_text(aes(label=n)) +
  theme_light() + 
  scale_x_reverse() # reverse x-axis


x_vals <- c(0, 100, 200, 300)
y_vals <- c(0, 50, 100, 200)
res <- data.frame()
for (i in 1:(length(x_vals)-1)){
  for (j in 1:(length(y_vals)-1)){
    res <- res %>%
      bind_rows(data.frame(x = i, y= j, cnt = sum(between(data_combined$coord_x, x_vals[i], x_vals[i+1]) & 
                                                    between(data_combined$coord_y, y_vals[j], y_vals[j+1]) , na.rm = T)))
  }
}

res %>%
  ggplot(aes(x=x, y=y, fill=cnt)) + geom_tile() + 
  geom_text(aes(label=cnt), color='red') +
  theme_light() + 
  scale_x_reverse(breaks=c(1,2,3), labels=c(50, 150, 250)) +
  scale_y_continuous(breaks=c(1,2,3), labels=c(25, 75, 150)) + #edit to have 
  xlab('Width (ft)') + ylab('Length (ft)') + 
  ggtitle('Percent Light Attenuation Heatmap') +
  theme(plot.title = element_text(hjust=0.5)) +
  scale_fill_gradient(low = "lightblue", high = "darkblue")





######################################################################
load("data_coordinates.RData")

combined <- data_light_loggers %>%
  group_by(point_name, date, disturbance_number) %>%
  summarise(light_attenuation_mean = mean(light_attenuation_mean, na.rm = T)) %>%
  left_join(data_coordinates %>% 
              select(coord_x, coord_y, point_name, date, disturbance_number = disturbance_id)) %>%
  ungroup()

combined %>%
  filter(date == '2022-10-01', disturbance_number ==1) %>%
  ggplot(aes(x=coord_x, y=coord_y, fill=light_attenuation_mean)) + geom_tile() +
  theme_light()


combined %>%
  ungroup() %>%
  summarise(min(light_attenuation_mean, na.rm = T), max(light_attenuation_mean, na.rm = T))

combined %>%
  drop_na(light_attenuation_mean) %>% # drop nan attenuation
  ggplot(aes(x=coord_x, y=coord_y, color=light_attenuation_mean)) +  
  annotate('rect',xmin=0, xmax=100, ymin=-Inf, ymax=Inf, alpha=0.4,fill='gray' ) +
  annotate('rect',xmin=200, xmax=300, ymin=-Inf, ymax=Inf, alpha=0.4,fill='gray' ) +
  geom_point(size=3, pch=15) +
  facet_grid(cols = vars(date), rows= vars(disturbance_number )) +
  scale_colour_distiller(direction =1, palette = 'YlOrBr') +
  xlab('Width') + ylab('Height') + theme_light() +
  scale_y_continuous(breaks=c(0, 50, 100, 200)) +
  scale_x_reverse() # reverse x-axis


combined %>%
  ggplot(aes(x=coord_x, y=coord_y, color=light_attenuation_mean)) + geom_tile() + 
  facet_grid(cols = vars(date), rows= vars(disturbance_number ), scales='free')


combined %>%
  mutate(coord_x = round(coord_x/100)*100, 
         coord_y = round(coord_y/100)*100) %>%
  count(coord_x, coord_y) %>%
  ggplot(aes(x=coord_x, y=coord_y, fill=n)) + geom_tile() +
  geom_text(aes(label=n)) +
  theme_light() + 
  scale_x_reverse() # reverse x-axis


x_vals <- c(0, 100, 200, 300)
y_vals <- c(0, 50, 100, 200)
res <- data.frame()
for (i in 1:(length(x_vals)-1)){
  for (j in 1:(length(y_vals)-1)){
    res <- res %>%
      bind_rows(data.frame(x = i, y= j, cnt = sum(between(combined$coord_x, x_vals[i], x_vals[i+1]) & 
                                                    between(combined$coord_y, y_vals[j], y_vals[j+1]) , na.rm = T)))
  }
}

res %>%
  ggplot(aes(x=x, y=y, fill=cnt)) + geom_tile() +
  geom_text(aes(label=cnt)) +
  theme_light() + 
  scale_x_reverse() # reverse x-axis


#######################################################

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

