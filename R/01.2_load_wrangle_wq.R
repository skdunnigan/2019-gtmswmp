# ----02 LOAD water quality data ------------------------------------------------------
df_wq_pi <- SWMPr::import_local(here::here('data', '13394.zip'), 'gtmpiwq')
df_wq_ss <- SWMPr::import_local(here::here('data', '13394.zip'), 'gtmsswq')
df_wq_fm <- SWMPr::import_local(here::here('data', '13394.zip'), 'gtmfmwq')
df_wq_pc <- SWMPr::import_local(here::here('data', '13394.zip'), 'gtmpcwq')

# qaqc and remove bad values
# add column that has station name in each df
df_wq_pi <- df_wq_pi %>%
  SWMPr::qaqc(qaqc_keep = c('0', '1', '4', '5')) %>%
  dplyr::mutate(station_name = 'gtmpiwq')
df_wq_ss <- df_wq_ss %>%
  SWMPr::qaqc(qaqc_keep = c('0', '1', '4', '5')) %>%
  dplyr::mutate(station_name = 'gtmsswq')
df_wq_fm <- df_wq_fm %>%
  SWMPr::qaqc(qaqc_keep = c('0', '1', '4', '5')) %>%
  dplyr::mutate(station_name = 'gtmfmwq')
df_wq_pc <- df_wq_pc %>%
  SWMPr::qaqc(qaqc_keep = c('0', '1', '4', '5')) %>%
  dplyr::mutate(station_name = 'gtmpcwq')

# merge all the files into one dataframe
# make sure station_name is ordered
# pull out year, month, day, hour 
df_wq <- dplyr::bind_rows(df_wq_pi, 
                          df_wq_ss, 
                          df_wq_fm, 
                          df_wq_pc) %>%
  dplyr::mutate(station_name = factor(station_name,
                                      levels = c("gtmpiwq",
                                                 "gtmsswq",
                                                 "gtmfmwq",
                                                 "gtmpcwq")),
                year = lubridate::year(as.Date(datetimestamp)),
                month = lubridate::month(as.Date(datetimestamp)),
                day = lubridate::day(as.Date(datetimestamp)),
                hour = lubridate::hour(datetimestamp)) %>%
  dplyr::select(-level, -clevel)

# remove individual stations df ---- cleanup
rm(df_wq_pi, df_wq_ss, df_wq_fm, df_wq_pc)

beep() # sound alert
