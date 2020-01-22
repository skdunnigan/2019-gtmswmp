# ----02 LOAD water quality data ------------------------------------------------------
df_wq_pi <- SWMPr::import_local(here::here('data', '837846.zip'), 'gtmpiwq')
df_wq_ss <- SWMPr::import_local(here::here('data', '837846.zip'), 'gtmsswq')
df_wq_fm <- SWMPr::import_local(here::here('data', '837846.zip'), 'gtmfmwq')
df_wq_pc <- SWMPr::import_local(here::here('data', '837846.zip'), 'gtmpcwq')

# qaqc and remove bad values
# add column that has station name in each df
df_wq_pi <- df_wq_pi %>%
  SWMPr::qaqc(qaqc_keep = c('0', '1', '5')) %>%
  dplyr::mutate(station_name = 'gtmpiwq')
df_wq_ss <- df_wq_ss %>%
  SWMPr::qaqc(qaqc_keep = c('0', '1', '5')) %>%
  dplyr::mutate(station_name = 'gtmsswq')
df_wq_fm <- df_wq_fm %>%
  SWMPr::qaqc(qaqc_keep = c('0', '1', '5')) %>%
  dplyr::mutate(station_name = 'gtmfmwq')
df_wq_pc <- df_wq_pc %>%
  SWMPr::qaqc(qaqc_keep = c('0', '1', '5')) %>%
  dplyr::mutate(station_name = 'gtmpcwq')

# merge all the files into one dataframe
df_wq <- dplyr::bind_rows(df_wq_pi, df_wq_ss, df_wq_fm, df_wq_pc)

# remove individual stations df
rm(df_wq_pi, df_wq_ss, df_wq_fm, df_wq_pc)

# make sure station_name is ordered
df_wq$station_name <- as.factor(df_wq$station_name)
df_wq$station_name <- factor(df_wq$station_name, 
                             levels = c("gtmpiwq",
                                        "gtmsswq",
                                        "gtmfmwq",
                                        "gtmpcwq")
)
