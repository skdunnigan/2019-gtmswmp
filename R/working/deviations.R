convert_c_to_f <- function(x) {
  fahrenheit <- ((x * (9/5)) + 32)
  return(fahrenheit)
}

# formula for overall monthly anomalies

monthly_temp_deviations <- function(x) {

# calculate average monthly temperatures for the entire time period for station x
monthly_ts_avg <- df_wq %>%
  filter(station_name == x) %>%
  select(year, month, day, temp) %>%
  mutate(tempF = convert_c_to_f(temp)) %>%
  group_by(month) %>%
  summarise(monthly_timeseries_avg = mean(tempF, na.rm = TRUE))

# view table
print(monthly_ts_avg %>%
       rename(monthly_avg_tempF_2005_2019 = monthly_timeseries_avg))

monthly_avgs <- df_wq %>%
  filter(station_name == x) %>%
  select(year, month, day, temp) %>%
  mutate(tempF = convert_c_to_f(temp)) %>%
  group_by(year, month) %>%
  summarise(monthly_avg = mean(tempF, na.rm = TRUE)) %>%
  left_join(monthly_ts_avg, by = "month") %>%
  mutate(dev_month = monthly_avg - monthly_timeseries_avg,
         day = 1,
         date = ymd(paste(year, month, day)),
         dev_color = ifelse(dev_month > 0, "Greater", "Lower")) %>%
  ungroup()

# view table
print(monthly_avgs %>%
       select(-day, -date, -dev_color) %>%
       rename(monthly_avg_tempF = monthly_avg,
              monthly_avg_tempF_2005_2019 = monthly_timeseries_avg,
              anomaly = dev_month))

a <- monthly_avgs %>%
  ggplot() +
  geom_bar(aes(x = date, y = dev_month, fill = dev_color),
           stat = "identity") +
  geom_hline(yintercept = 0, linetype = 2, color = "black") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_pubr() +
  theme(legend.position = "none") +
  labs(x = '',
       y = 'Monthly Temperature Anomalies') +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"), name = NULL)

b <- monthly_avgs %>%
  ggplot(aes(x = date, y = dev_month)) +
  geom_point(aes(color = dev_color),
           size = 2) +
  geom_smooth(method = lm, se = FALSE, 
              color = "black", linetype = 2) +
  stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~`, `~")),
                        label.y.npc = 1) +
  geom_hline(yintercept = 0, linetype = 2, color = "lightgray") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_pubr() +
  labs(x = '',
       y = 'Monthly Temperature Anomalies') +
  scale_color_manual(values = c("#00AFBB", "#E7B800"), name = NULL)

print(a)
}

monthly_temp_deviations("gtmsswq")

# -----------------------------------------------------------all sites
monthly_ts_avg <- df_wq %>%
  select(year, month, day, temp) %>%
  mutate(tempF = convert_c_to_f(temp)) %>%
  group_by(month) %>%
  summarise(monthly_timeseries_avg = mean(tempF, na.rm = TRUE))

# view table
print(monthly_ts_avg %>%
        rename(monthly_avg_tempF_2005_2019 = monthly_timeseries_avg))

monthly_avgs <- df_wq %>%
  select(year, month, day, temp) %>%
  mutate(tempF = convert_c_to_f(temp)) %>%
  group_by(year, month) %>%
  summarise(monthly_avg = mean(tempF, na.rm = TRUE)) %>%
  left_join(monthly_ts_avg, by = "month") %>%
  mutate(dev_month = monthly_avg - monthly_timeseries_avg,
         day = 1,
         date = ymd(paste(year, month, day)),
         dev_color = ifelse(dev_month > 0, "Greater", "Lower")) %>%
  ungroup()

# view table
print(monthly_avgs %>%
        select(-day, -date, -dev_color) %>%
        rename(monthly_avg_tempF = monthly_avg,
               monthly_avg_tempF_2005_2019 = monthly_timeseries_avg,
               anomaly = dev_month))

a <- monthly_avgs %>%
  ggplot() +
  geom_bar(aes(x = date, y = dev_month, fill = dev_color),
           stat = "identity") +
  geom_hline(yintercept = 0, linetype = 2, color = "black") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_pubr() +
  theme(legend.position = "none") +
  labs(x = '',
       y = 'Monthly Temperature Anomalies') +
  scale_fill_manual(values = c("#00AFBB", "#E7B800"), name = NULL) +
  

b <- monthly_avgs %>%
  ggplot(aes(x = date, y = dev_month)) +
  geom_point(aes(color = dev_color),
             size = 2) +
  geom_smooth(method = lm, se = FALSE, 
              color = "black", linetype = 2) +
  stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~`, `~")),
                        label.y.npc = 1) +
  geom_hline(yintercept = 0, linetype = 2, color = "lightgray") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_pubr() +
  labs(x = '',
       y = 'Monthly Temperature Anomalies') +
  scale_color_manual(values = c("#00AFBB", "#E7B800"), name = NULL)

a
b
