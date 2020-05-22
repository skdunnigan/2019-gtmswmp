# temperature anomalies

# temperature
convert_c_to_f <- function(x) {
  fahrenheit <- ((x * (9/5)) + 32)
  return(fahrenheit)
}

# temp anomaly function
# station is SWMP station, in parentheses
# type should equal min or max in parentheses

temp_anomaly <- function(station, type) {
  
  if (type == "min") {
    timeseries_min_avg <- df_wq %>%
      dplyr::filter(station_name == station) %>%
      dplyr::select(-datetimestamp, -hour, -day, year, temp) %>%
      dplyr::group_by(year) %>%
      dplyr::mutate(tempF = convert_c_to_f(temp)) %>%
      dplyr::summarise(min_temp_yr = min(tempF, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::summarise(timeseries_min_avg = mean(min_temp_yr))
    
    timeseries_min_avg <- timeseries_min_avg[[1,1]]
    
    yr_mins <- df_wq %>%
    dplyr::filter(station_name == station) %>%
    dplyr::select(-datetimestamp, -hour, -day, year, temp) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(tempF = convert_c_to_f(temp)) %>%
    dplyr::summarise(min_temp_yr = min(tempF, na.rm = TRUE)) %>%
    dplyr::mutate(timeseries_min_avg = mean(min_temp_yr),
                  anomaly = min_temp_yr - timeseries_min_avg,
                  dev = ifelse(anomaly > 0, "Warmer", "Colder"))
  
  ggpubr::ggbarplot(yr_mins, x = "year", y = "anomaly",
            fill = "dev",
            color = "dev",
            palette = c("#00AFBB", "#E7B800"),
            sort.by.groups = TRUE,
            group = "year",
            label = TRUE,
            lab.nb.digits = 1,
            lab.size = 5) +
    geom_hline(yintercept = 0, linetype = 2, color = "lightgray") +
    labs(x = '',
         y = 'Temperature ('~degree*F*') difference',
         title = "Temperature anomaly from average minimum",
         caption = paste("Data from", station, ': average minimum temperature 2005-2019 was', round(timeseries_min_avg, 1), '°F')) +
    theme(legend.position = "none")
  
  } else if (type == "max") {
    
    yr_max <- df_wq %>%
      dplyr::filter(station_name == station) %>%
      dplyr::select(-datetimestamp, -hour, -day, year, temp) %>%
      dplyr::group_by(year) %>%
      dplyr::mutate(tempF = convert_c_to_f(temp)) %>%
      dplyr::summarise(max_temp_yr = max(tempF, na.rm = TRUE)) %>%
      dplyr::mutate(timeseries_max_avg = mean(max_temp_yr),
                    anomaly = max_temp_yr - timeseries_max_avg,
                    dev = ifelse(anomaly > 0, "Warmer", "Colder"))
    
    timeseries_max_avg <- df_wq %>%
      dplyr::filter(station_name == station) %>%
      dplyr::select(-datetimestamp, -hour, -day, year, temp) %>%
      dplyr::group_by(year) %>%
      dplyr::mutate(tempF = convert_c_to_f(temp)) %>%
      dplyr::summarise(max_temp_yr = max(tempF, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::summarise(timeseries_max_avg = mean(max_temp_yr))
    
    timeseries_max_avg <- timeseries_max_avg[[1,1]]
    
    ggpubr::ggbarplot(yr_max, x = "year", y = "anomaly",
              fill = "dev",
              color = "dev",
              palette = c("#00AFBB", "#E7B800"),
              sort.by.groups = TRUE,
              group = "year",
              label = TRUE,
              lab.nb.digits = 1,
              lab.size = 5) +
      geom_hline(yintercept = 0, linetype = 2, color = "lightgray") +
      labs(x = '',
           y = 'Temperature ('~degree*F*') difference',
           title = "Temperature anomaly from average maximum",
           caption = paste("Data from", station,': average maximum temperature 2005-2019 was', round(timeseries_max_avg, 1), '°F')) +
      theme(legend.position = "none")
    
  }

}

temp_anomaly(station = "gtmpiwq", type = "max")
temp_anomaly(station = "gtmsswq", type = "max")
temp_anomaly(station = "gtmfmwq", type = "max")
temp_anomaly(station = "gtmpcwq", type = "max")

# -------------------all sites together
temp_anomaly_GTM <- function(type) {
  
if (type == "min") {
timeseries_min_avg <- df_wq %>%
  dplyr::select(-datetimestamp, -hour, -day, year, temp) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(tempF = convert_c_to_f(temp)) %>%
  dplyr::summarise(min_temp_yr = min(tempF, na.rm = TRUE)) %>%
  dplyr::ungroup() %>%
  dplyr::summarise(timeseries_min_avg = mean(min_temp_yr))

timeseries_min_avg <- timeseries_min_avg[[1,1]]

yr_mins <- df_wq %>%
  dplyr::select(-datetimestamp, -hour, -day, year, temp) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(tempF = convert_c_to_f(temp)) %>%
  dplyr::summarise(min_temp_yr = min(tempF, na.rm = TRUE)) %>%
  dplyr::mutate(timeseries_min_avg = mean(min_temp_yr),
                anomaly = min_temp_yr - timeseries_min_avg,
                dev = ifelse(anomaly > 0, "Warmer", "Colder"))

ggpubr::ggbarplot(yr_mins, x = "year", y = "anomaly",
                  fill = "dev",
                  color = "dev",
                  palette = c("#00AFBB", "#E7B800"),
                  sort.by.groups = TRUE,
                  group = "year",
                  label = TRUE,
                  lab.nb.digits = 1,
                  lab.size = 5) +
  geom_hline(yintercept = 0, linetype = 2, color = "lightgray") +
  labs(x = '',
       y = 'Temperature ('~degree*F*') difference',
       title = "Temperature anomaly from average minimum",
       caption = paste('Data from all stations: average minimum temperature 2005-2019 was', round(timeseries_min_avg, 1), '°F')) +
  theme(legend.position = "none")
} else if (type == "max") {
  
  yr_max <- df_wq %>%
    dplyr::select(-datetimestamp, -hour, -day, year, temp) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(tempF = convert_c_to_f(temp)) %>%
    dplyr::summarise(max_temp_yr = max(tempF, na.rm = TRUE)) %>%
    dplyr::mutate(timeseries_max_avg = mean(max_temp_yr),
                  anomaly = max_temp_yr - timeseries_max_avg,
                  dev = ifelse(anomaly > 0, "Warmer", "Colder"))
  
  timeseries_max_avg <- df_wq %>%
    dplyr::select(-datetimestamp, -hour, -day, year, temp) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(tempF = convert_c_to_f(temp)) %>%
    dplyr::summarise(max_temp_yr = max(tempF, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::summarise(timeseries_max_avg = mean(max_temp_yr))
  
  timeseries_max_avg <- timeseries_max_avg[[1,1]]
  
  ggpubr::ggbarplot(yr_max, x = "year", y = "anomaly",
                    fill = "dev",
                    color = "dev",
                    palette = c("#00AFBB", "#E7B800"),
                    sort.by.groups = TRUE,
                    group = "year",
                    label = TRUE,
                    lab.nb.digits = 1,
                    lab.size = 5) +
    geom_hline(yintercept = 0, linetype = 2, color = "lightgray") +
    labs(x = '',
         y = 'Temperature ('~degree*F*') difference',
         title = "Temperature anomaly from average maximum",
         caption = paste('Data from all stations: average maximum temperature 2005-2019 was', round(timeseries_max_avg, 1), '°F')) +
    theme(legend.position = "none")
}
}

temp_anomaly_GTM(type = "min")
temp_anomaly_GTM(type = "max")