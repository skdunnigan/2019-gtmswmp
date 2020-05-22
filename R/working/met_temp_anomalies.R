# temperature
convert_c_to_f <- function(x) {
  fahrenheit <- ((x * (9/5)) + 32)
  return(fahrenheit)
}

temp_anomaly_GTM_met <- function(type) {
  
  df_adjust <- df_met %>%
    # dplyr::filter(year >= 2007) %>%
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
    
a <-  ggpubr::ggbarplot(yr_mins, x = "year", y = "anomaly",
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
    
print(yr_mins)
print(a)
    
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
    
a <- ggpubr::ggbarplot(yr_max, x = "year", y = "anomaly",
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
           caption = paste('Data from gtmpcmet 2007-2019: average maximum temperature was', round(timeseries_max_avg, 1), '°F')) +
      theme_cowplot() +
      theme(legend.position = "none")
    
print(yr_max)
print(a)
  }
}

####################################################
# redo of SotR article figure

df_adjust <- df_met %>%
  # dplyr::filter(year >= 2007) %>%
  dplyr::select(year, atemp) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(tempF = convert_c_to_f(atemp))


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

 
  ggplot(data = yr_mins, aes(x = year, y = anomaly)) +
    geom_col(aes(fill = dev), 
             show.legend = TRUE) +
    # geom_text(aes(label = round(anomaly, 1)), vjust = -1, size = 5) +
    geom_smooth(method = "lm", se = FALSE, color = "black", linetype = 2) +
    stat_regline_equation() +
    stat_cor(
      aes(label = paste(..rr.label.., ..p.label.., sep = "~`, `~")), label.y = 5.5) +
    geom_hline(yintercept = 0, linetype = 2, color = "lightgray") +
    labs(x = '',
         y = 'Temperature ('~degree*F*') difference') +
    theme_cowplot() +
    theme(legend.position = "none") +
    scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
    scale_x_continuous(breaks = c(2005:2019)) +
    ylim(-5,6.5) +
    ggsave(here::here('output', 'visuals', '2020-02-10_mintempdiff_line.png'),
         height = 5,
         width = 8,
         units = "in",
         dpi = 300)
    
  
  
  
  
  
   
# ggsave(here::here('output', 'visuals', '2020-01-31_mintempdiff.png'),
#        height = 5,
#        width = 8,
#        units = "in",
#        dpi = 300)