convert_c_to_f <- function(x) {
  fahrenheit <- ((x * (9/5)) + 32)
  return(fahrenheit)
}

# calculate timeseries avg temperature
timeseries_avg <- df_wq %>%
  filter(station_name == "gtmfmwq") %>%
  select(temp) %>%
  mutate(tempF = convert_c_to_f(temp)) %>%
  summarise(timeseries_avg = mean(tempF, na.rm = TRUE))

ts_avg <- timeseries_avg[[1,1]]

monthly_ts_avg <- df_wq %>%
  filter(station_name == "gtmfmwq") %>%
  select(year, month, day, temp) %>%
  mutate(tempF = convert_c_to_f(temp)) %>%
  group_by(month) %>%
  summarise(monthly_timeseries_avg = mean(tempF, na.rm = TRUE))

monthly_avgs <- df_wq %>%
  filter(station_name == "gtmfmwq") %>%
  select(year, month, day, temp) %>%
  mutate(tempF = convert_c_to_f(temp)) %>%
  group_by(year, month) %>%
  summarise(monthly_avg = mean(tempF, na.rm = TRUE)) %>%
  left_join(monthly_ts_avg, by = "month") %>%
  mutate(dev_month = monthly_avg - monthly_timeseries_avg,
         day = 1,
         date = ymd(paste(year, month, day)),
         dev_color = ifelse(dev_month > 0, "Greater", "Lower"))

monthly_avgs %>%
  ggplot() +
  geom_bar(aes(x = date, y = dev_month, fill = dev_color),
           stat = "identity") +
  geom_hline(yintercept = 0, linetype = 2, color = "black") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  theme_pubr()
  