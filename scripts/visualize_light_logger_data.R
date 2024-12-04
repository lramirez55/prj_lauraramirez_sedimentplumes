#### user defined variables ####
# data_poles_path <- "../data/pole_legend.xlsx"
light_logger_data_vis_path <- "../output/light_logger_plots.pdf"
# sliding_window_interval_seconds = 300

#### SetWD ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### Get Data ####
source("wrangle_light_logger_data.R")

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

#### Load Coordinates Data ####

# CEB note that there is a coordinate for every logger, but data_light_loggers has 1 row per pole per disturbance
source("../scripts/calc_coordinates.R")
#load("../data/data_coordinates.RData")

#### Visualize Pole Coordinates ####

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
      
      print(target_date)
      
      p <- 
        ggplot(plot_data) +
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
        ggtitle(paste("Date:", target_date)) +
        facet_grid(date ~ disturbance_id)
      
      print(p)
    }
    
    # Reset the plotting area
    par(mfrow = c(1, 1))
  }

dates <- 
  c(
    "2022-07-14", 
    "2022-07-18", 
    "2022-08-27",
    "2022-09-10",
    "2022-09-17",
    "2022-09-25",
    "2022-10-01",
    "2022-10-05",
    "2022-10-15",
    "2022-11-02",
    "2022-11-09"
  )

pdf("../output/pole_coordinate_maps.pdf")
  plot_light_data_for_dates(data_coordinates, dates)
dev.off()

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


#### Process Data for Disturbance Locations by Zone and Track ####

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



