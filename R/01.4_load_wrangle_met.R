# ----04 LOAD met data ------------------------------------------------------
df_met <- SWMPr::import_local(here::here('data', '13394.zip'), 'gtmpcmet')

# qaqc and remove bad values
df2_met <- df_met %>%
  SWMPr::qaqc(qaqc_keep = c('0', '4', '5')) 


# merge all the files into one dataframe
# make sure station_name is ordered
# pull out year, month, day, hour 
df2_met <- df2_met %>%
  dplyr::mutate(year = lubridate::year(datetimestamp),
                month = lubridate::month(datetimestamp),
                day = lubridate::day(datetimestamp),
                hour = lubridate::hour(datetimestamp)) 

# make the df easy again
df_met <- df2_met
rm(df2_met)

beep() # sound alert for finish!
