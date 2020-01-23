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
  select(-datetimestamp, -hour, -day) %>%
  group_by(station_name, month) %>%
  summarise_all(~mean(., na.rm = TRUE)) %>%
  mutate(`2005-2018` = "2005-2018")
