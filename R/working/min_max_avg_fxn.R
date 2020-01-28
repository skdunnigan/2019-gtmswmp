convert_c_to_f <- function(x) {
  fahrenheit <- ((x * (9/5)) + 32)
  return(fahrenheit)
}


mins_maxs <- function(x) {

  # calculate timeseries avg temperature
timeseries_avg <- df_wq %>%
  filter(station_name == x) %>%
  select(temp) %>%
  mutate(tempF = convert_c_to_f(temp)) %>%
  summarise(timeseries_avg = mean(tempF, na.rm = TRUE))

ts_avg <- timeseries_avg[[1,1]]

yr_min_max <- df_wq %>%
  filter(station_name == x) %>%
  select(year, temp) %>%
  mutate(tempF = convert_c_to_f(temp)) %>%
  group_by(year) %>%
  summarise(min_temp_yr = min(tempF, na.rm = TRUE),
            max_temp_yr = max(tempF, na.rm = TRUE)) %>%
  ungroup() %>%
  summarise(timeseries_min_avg = mean(min_temp_yr),
            timeseries_max_avg = mean(max_temp_yr))

ts_min_avg <- yr_min_max[[1,1]]
ts_max_avg <- yr_min_max[[1,2]]

yr_mins_maxs_means <- df_wq %>%
  filter(station_name == x) %>%
  select(year, temp) %>%
  group_by(year) %>%
  mutate(tempF = convert_c_to_f(temp)) %>%
  summarise(min_temp_yr = min(tempF, na.rm = TRUE),
            max_temp_yr = max(tempF, na.rm = TRUE),
            mean_temp_yr = mean(tempF, na.rm = TRUE)) 

print(yr_mins_maxs_means)

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
       caption = paste('Data from', x, 'from 2005-2019: average of', 
                       round(ts_avg, 1), 'and an average minimum of', 
                       round(ts_min_avg,1), 'and maximum of', 
                       round(ts_max_avg,1)))
print(a)

min_under <- df_wq %>%
  filter(station_name == x) %>%
  select(year, temp) %>%
  mutate(tempF = convert_c_to_f(temp)) %>%
  group_by(year) %>%
  filter(tempF < ts_min_avg) %>%
  count(name = "no_under_min")

max_over <- df_wq %>%
  filter(station_name == x) %>%
  select(year, temp) %>%
  mutate(tempF = convert_c_to_f(temp)) %>%
  group_by(year) %>%
  filter(tempF > ts_max_avg) %>%
  count(name = "no_over_max") 

print(full_join(min_under, max_over) %>% arrange(desc(year)))

}

mins_maxs("gtmfmwq")
mins_maxs("gtmpcwq")

# -------------------------------all sites

  # calculate timeseries avg temperature
  timeseries_avg <- df_wq %>%
    select(temp) %>%
    mutate(tempF = convert_c_to_f(temp)) %>%
    summarise(timeseries_avg = mean(tempF, na.rm = TRUE))
  
  ts_avg <- timeseries_avg[[1,1]]
  
  yr_min_max <- df_wq %>%
    select(year, temp) %>%
    mutate(tempF = convert_c_to_f(temp)) %>%
    group_by(year) %>%
    summarise(min_temp_yr = min(tempF, na.rm = TRUE),
              max_temp_yr = max(tempF, na.rm = TRUE)) %>%
    ungroup() %>%
    summarise(timeseries_min_avg = mean(min_temp_yr),
              timeseries_max_avg = mean(max_temp_yr))
  
  ts_min_avg <- yr_min_max[[1,1]]
  ts_max_avg <- yr_min_max[[1,2]]
  
  yr_mins_maxs_means <- df_wq %>%
    select(year, temp) %>%
    group_by(year) %>%
    mutate(tempF = convert_c_to_f(temp)) %>%
    summarise(min_temp_yr = min(tempF, na.rm = TRUE),
              max_temp_yr = max(tempF, na.rm = TRUE),
              mean_temp_yr = mean(tempF, na.rm = TRUE)) 
  
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
         caption = paste('Data from all stations 2005-2019: average of', 
                         round(ts_avg, 1), 'and an average minimum of', 
                         round(ts_min_avg,1), 'and maximum of', 
                         round(ts_max_avg,1)))
  print(a)
  
  min_under <- df_wq %>%
    select(year, temp) %>%
    mutate(tempF = convert_c_to_f(temp)) %>%
    group_by(year) %>%
    filter(tempF < ts_min_avg) %>%
    count(name = "no_under_min")
  
  max_over <- df_wq %>%
    select(year, temp) %>%
    mutate(tempF = convert_c_to_f(temp)) %>%
    group_by(year) %>%
    filter(tempF > ts_max_avg) %>%
    count(name = "no_over_max") 
  
  print(full_join(min_under, max_over) %>% arrange(desc(year)))
  print(yr_mins_maxs_means)