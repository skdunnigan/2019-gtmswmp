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
            ylab = "Temperature difference from average minimum") +
    geom_hline(yintercept = 0, linetype = 2, color = "lightgray") +
    labs(x = '',
         caption = paste("data from", station))
  
  } else if (type == "max") {
    
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
      labs(x = '',
           caption = paste("data from", station))
    
  }

}

a <- temp_anomaly(station = "gtmfmwq", type = "min")
b <- temp_anomaly(station = "gtmfmwq", type = "max")

plot_grid(a,b, ncol = 1)