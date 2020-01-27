df_wq %>%
  dplyr::mutate(date = lubridate::date(datetimestamp),
                month = lubridate::month(date),
                day = lubridate::day(date)) %>%
  dplyr::group_by(station_name, date) %>%
  dplyr::summarise_all(list(min, max))



#   ggplot(aes(x = datetimestamp, y = sal)
#          
# # summaries
# df_wq %>%
#   dplyr::mutate(date = lubridate::date(datetimestamp),
#                 month = lubridate::month(date),
#                 day = lubridate::day(date)) %>%
#   dplyr::group_by(station_name, month)
#   # dplyr::summarise_all(list(~min(.), ~max(.))) %>%
#   
#   
# df_wq %>%
#   filter(station_name == "gtmsswq" & year == 2018) %>%
#   ggplot(aes(x = datetimestamp, y = sal)) +
#   geom_point()

df_wq %>%
  filter(year != 2019) %>%
  select(-datetimestamp, -month, -hour, -day) %>%
  group_by(station_name, month) %>%
  summarise_all(~mean(., na.rm = TRUE)) %>%
  mutate(`2005-2018` = "2005-2018")



# minimum temperatures from Fort Matanzas station

mintemp_anomaly <- function(station) {
  
  yr_mins <- df_wq %>%
    filter(station_name == station) %>%
    select(-datetimestamp, -hour, -day, year, temp) %>%
    group_by(year) %>%
    mutate(tempF = convert_c_to_f(temp)) %>%
    summarise(min_temp_yr = min(tempF, na.rm = TRUE)) %>%
    mutate(timeseries_min_avg = mean(min_temp_yr),
           anomaly = min_temp_yr - timeseries_min_avg,
           dev = ifelse(anomaly >= 0, "Warmer", "Colder"))
  
  ggbarplot(yr_mins, x = "year", y = "anomaly",
            fill = "dev",
            color = "dev",
            palette = c("#00AFBB", "#E7B800"),
            sort.by.groups = TRUE,
            group = "year",
            ylab = "Temperature difference from average minimum"
  ) +
    geom_hline(yintercept = 0, linetype = 2, color = "lightgray") +
    labs(caption = paste("data from ", station))
}

maxtemp_anomaly <- function(station) {
  
  yr_max <- df_wq %>%
    filter(station_name == station) %>%
    select(-datetimestamp, -hour, -day, year, temp) %>%
    group_by(year) %>%
    mutate(tempF = convert_c_to_f(temp)) %>%
    summarise(max_temp_yr = max(tempF, na.rm = TRUE)) %>%
    mutate(timeseries_max_avg = mean(max_temp_yr),
           anomaly = max_temp_yr - timeseries_max_avg,
           dev = ifelse(anomaly >= 0, "Warmer", "Colder")) 
  
  ggbarplot(yr_max, x = "year", y = "anomaly",
            fill = "dev",
            color = "dev",
            palette = c("#00AFBB", "#E7B800"),
            sort.by.groups = TRUE,
            group = "year",
            ylab = "Temperature difference from average maximum"
  ) +
    geom_hline(yintercept = 0, linetype = 2, color = "lightgray") +
    labs(caption = paste("data from ", station))
}


mintemp_anomaly("gtmfmwq")
maxtemp_anomaly("gtmfmwq")

yr_mins <- df_wq %>%
  filter(station_name == "gtmfmwq") %>%
  select(-datetimestamp, -hour, -day, year, temp) %>%
  group_by(year) %>%
  mutate(tempF = convert_c_to_f(temp)) %>%
  summarise(min_temp_yr = min(tempF, na.rm = TRUE)) %>%
  mutate(timeseries_min_avg = mean(min_temp_yr),
         anomaly = min_temp_yr - timeseries_min_avg,
         dev = ifelse(anomaly >= 0, "Warmer", "Colder")) 

# lollipop
ggdotchart(data = yr_mins, x = "year", y = "anomaly",
           color = "dev",
           palette = c("#00AFBB", "#E7B800"),
           add = "segments",
           add.params = list(color = "lightgray", size = 2.5),
           group = "year",
           dot.size = 6,
           ggtheme = theme_pubr(),
           ylab = "Temperature difference from average minimum") +
  geom_hline(yintercept = 0, linetype = 2, color = "lightgray") 

# barplot  
ggbarplot(yr_mins, x = "year", y = "anomaly",
          fill = "dev",
          color = "dev",
          palette = c("#00AFBB", "#E7B800"),
          sort.by.groups = TRUE,
          group = "year",
          ylab = "Temperature difference from average minimum"
) +
  geom_hline(yintercept = 0, linetype = 2, color = "lightgray")  

# minimum temperatures from Fort Matanzas station

yr_max <- df_wq %>%
  filter(station_name == "gtmfmwq") %>%
  select(-datetimestamp, -hour, -day, year, temp) %>%
  group_by(year) %>%
  mutate(tempF = convert_c_to_f(temp)) %>%
  summarise(max_temp_yr = max(tempF, na.rm = TRUE)) %>%
  mutate(timeseries_max_avg = mean(max_temp_yr),
         anomaly = max_temp_yr - timeseries_max_avg,
         dev = ifelse(anomaly >= 0, "Warmer", "Colder")) 

# lollipop
ggdotchart(data = yr_max, x = "year", y = "anomaly",
           color = "dev",
           palette = c("#00AFBB", "#E7B800"),
           add = "segments",
           add.params = list(color = "lightgray", size = 2.5),
           group = "year",
           dot.size = 6,
           ggtheme = theme_pubr(),
           ylab = "Temperature difference from average maximum") +
  geom_hline(yintercept = 0, linetype = 2, color = "lightgray") 

# barplot  
ggbarplot(yr_max, x = "year", y = "anomaly",
          fill = "dev",
          color = "dev",
          palette = c("#00AFBB", "#E7B800"),
          sort.by.groups = TRUE,
          group = "year",
          ylab = "Temperature difference from average maximum"
) +
  geom_hline(yintercept = 0, linetype = 2, color = "lightgray")  