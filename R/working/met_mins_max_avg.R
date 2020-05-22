convert_c_to_f <- function(x) {
  fahrenheit <- ((x * (9/5)) + 32)
  return(fahrenheit)
}

# calculate timeseries avg temperature
  timeseries_avg <- df_met %>%
    select(atemp) %>%
    mutate(tempF = convert_c_to_f(atemp)) %>%
    summarise(timeseries_avg = mean(tempF, na.rm = TRUE))
  
  ts_avg <- timeseries_avg[[1,1]]

temp_met <- df_met %>%
  select(year, atemp) %>%
  mutate(tempF = convert_c_to_f(atemp))

  yr_min_max <- temp_met %>% 
    group_by(year) %>%
    summarise(min_temp_yr = min(tempF, na.rm = TRUE),
              max_temp_yr = max(tempF, na.rm = TRUE)) %>%
    ungroup() %>%
    summarise(timeseries_min_avg = mean(min_temp_yr),
              timeseries_max_avg = mean(max_temp_yr))
  
  ts_min_avg <- yr_min_max[[1,1]]
  ts_max_avg <- yr_min_max[[1,2]]
  
  yr_mins_maxs_means <- temp_met %>%
    group_by(year) %>%
    summarise(min_temp_yr = min(tempF, na.rm = TRUE),
              max_temp_yr = max(tempF, na.rm = TRUE),
              mean_temp_yr = mean(tempF, na.rm = TRUE)) %>%
    ungroup()
  
  
  
  a <- yr_mins_maxs_means %>%
    ggplot(aes(x = year)) +
    geom_point(aes(y = min_temp_yr, color = "minimum"), size= 2) +
    geom_hline(yintercept = ts_min_avg, linetype = 2, color = "lightgray") +
    geom_point(aes(y = max_temp_yr, color = "maximum"), size= 2) +
    geom_hline(yintercept = ts_max_avg, linetype = 2, color = "lightgray") +
    geom_point(aes(y = mean_temp_yr, color = "average"), size= 2) +
    geom_hline(yintercept = ts_avg, linetype = 2, color = "darkgray") +
    theme_cowplot() +
    scale_color_manual(name = NULL, values = c("minimum" = "#00AFBB", 
                                               "maximum" = "#FC4E07",
                                               "average" = "#E7B800")) +
    scale_x_continuous(breaks = c(2005:2019)) +
    labs(x = '',
         y = expression('Temperature ('~degree*F*')'),
         caption = paste('Data from gtmpcmet from 2005-2019: average of', 
                         round(ts_avg, 1), '°F and an average minimum of', 
                         round(ts_min_avg,1), '°F and maximum of', 
                         round(ts_max_avg,1), '°F'))
  
  min_under <- temp_met %>%
    group_by(year) %>%
    filter(tempF < ts_min_avg) %>%
    count(name = "no_under_min") %>%
    ungroup()
  
  max_over <- temp_met %>%
    group_by(year) %>%
    filter(tempF > ts_max_avg) %>%
    count(name = "no_over_max") %>%
    ungroup()
  
  print(yr_min_max)
  print(yr_mins_maxs_means)
  print(full_join(min_under, max_over) %>% arrange(desc(year)))
  print(a)
  
rm(max_over, min_under, temp_met, timeseries_avg, 
   yr_min_max, ts_avg, ts_max_avg, ts_min_avg,
   a, yr_mins_maxs_means,
   convert_c_to_f)