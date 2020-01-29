# temperature
convert_c_to_f <- function(x) {
  fahrenheit <- ((x * (9/5)) + 32)
  return(fahrenheit)
}

temp_anomaly_GTM_met <- function(type) {
  
  df_adjust <- df_met %>%
    dplyr::filter(year >= 2007) %>%
    dplyr::select(year, atemp) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(tempF = convert_c_to_f(atemp))
  
  if (type == "min") {
    timeseries_min_avg <- df_adjust %>%
      dplyr::summarise(min_temp_yr = min(tempF, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::summarise(timeseries_min_avg = mean(min_temp_yr))
    
    timeseries_min_avg <- timeseries_min_avg[[1,1]]
    
    yr_mins <- df_adjust %>%
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
           caption = paste('Data from gtmpcmet 2007-2019: average minimum temperature was', round(timeseries_min_avg, 1), '°F')) +
      theme_cowplot() +
      theme(legend.position = "none")
    
  } else if (type == "max") {
    
    yr_max <- df_adjust %>%
      dplyr::summarise(max_temp_yr = max(tempF, na.rm = TRUE)) %>%
      dplyr::mutate(timeseries_max_avg = mean(max_temp_yr),
                    anomaly = max_temp_yr - timeseries_max_avg,
                    dev = ifelse(anomaly > 0, "Warmer", "Colder"))
    
    timeseries_max_avg <- df_adjust %>%
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
           caption = paste('Data from gtmpcmet 2007-2019: averagee maximum temperature was', round(timeseries_max_avg, 1), '°F')) +
      theme_cowplot() +
      theme(legend.position = "none")
  }
}