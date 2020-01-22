df_wq %>%
  dplyr::mutate(date = lubridate::date(datetimestamp),
                month = lubridate::month(date),
                day = lubridate::day(date)) %>%
  dplyr::group_by(station_name, date) %>%
  dplyr::summarise_all(list(min, max))



  ggplot(aes(x = datetimestamp, y = sal)
         
# summaries
df_wq %>%
  dplyr::mutate(date = lubridate::date(datetimestamp),
                month = lubridate::month(date),
                day = lubridate::day(date)) %>%
  dplyr::group_by(station_name, month) %>%
  # dplyr::summarise_all(list(~min(.), ~max(.))) %>%