# ----03 LOAD biomonitoring vegetation data--------------------------------------------
# ---------------------------read in 2019 data 
df_veg <- readxl::read_xlsx(here::here('data', '2019VEG_raw.xlsx')) %>% 
  janitor::clean_names()

# rename canopy height columns
# add calculated percent cover column (blank)
# reformat date, and add year column
# add in season
# make subplot a character

df2_veg <- df_veg %>%
  dplyr::rename(canopy_height_cm = canopy_height_15,
                canopy_height_m = canopy_height_16) %>%
  dplyr::mutate(percent_cover_cal = NA,
                date = lubridate::date(date),
                year = lubridate::year(date),
                month = lubridate::month(date),
                season = ifelse(month >4, "Fall", "Spring"),
                season = factor(season, levels = c("Spring", "Fall")),
                site_id = factor(site_id, levels = c("40", 
                                                     "00", 
                                                     "22", 
                                                     "06", 
                                                     "46", 
                                                     "01")),
                subplot = as.character(subplot))

# ----------------------------read in previous data 2012-2018
df_vegold <- read.csv(here::here('data', '2012-2018_VEGMaster.csv'), 
                      stringsAsFactors = FALSE) %>%
  janitor::clean_names()

# rename the percent cover columns
# create new columns for canopy height in cm, reserve, month, and type, 
#  that are in 2019
# reconfigure columns (date, year, season)
# make subplot a character

df2_vegold <- df_vegold %>%
  dplyr::rename(percent_cover = x_cover,
                percent_cover_cal = calc_cover) %>%
  dplyr::mutate(canopy_height_cm = canopy_height_m*100,
                reserve = "GTM",
                type = "E",
                date = as.Date(date, "%m/%e/%Y"),
                year = lubridate::year(date),
                month = lubridate::month(date),
                season = factor(season, levels = c("Spring", "Fall")),
                site_id = factor(site_id, levels = c("40", 
                                                     "00", 
                                                     "22", 
                                                     "06", 
                                                     "46", 
                                                     "01")),
                subplot = as.character(subplot))

# -----------re-class multiple columns in both data frames to numeric

cols.num <- c("percent_cover", "percent_cover_cal", "elevation", "plot_id",
              "canopy_height_cm", "canopy_height_m", "density_raw", "density_adj",
              "lg_burrow_raw", "lg_burrow_adj", "sm_burrow_raw", 
              "sm_burrow_adj", "diameter", "height")
df2_veg[cols.num] <- sapply(df2_veg[cols.num], as.numeric)
df2_vegold[cols.num] <- sapply(df2_vegold[cols.num], as.numeric)


# ---------------------------------------------combine data into one
df_veg_all <- dplyr::bind_rows(df2_vegold, df2_veg)


# remove entries for 'Distichlis spicata' because we don't have it in plots
# remove unnecessary columns

df2_veg_all <- df_veg_all %>%
  dplyr::filter(plot_id <= 5 & species != "Distichlis spicata") %>%
  dplyr::filter(species != "Distichlis spicata ") 

# ---------for comparing with SWMP water quality data and for grouping as such
# create column that groups sites based on proximity to SWMP WQ stations
# first make vectors for each site
gtmpiwq <- c("40", "00")
gtmsswq <- c("22", NA)
gtmfmwq <- c("01", "06")
gtmpcwq <- c("46", NA)

# bind the vectors into a data frame
swmp_wq <- bind_cols("gtmpiwq" = gtmpiwq, 
                     "gtmsswq" = gtmsswq,
                     "gtmfmwq" = gtmfmwq,
                     "gtmpcwq" = gtmpcwq) %>%
  gather(key = "station_name", value = "site_id")

# remove the vectors, we don't need them
rm(gtmpiwq, gtmsswq, gtmfmwq, gtmpcwq)

# merge site names with dataframe
df3_veg_all <- merge(df2_veg_all, swmp_wq, by="site_id", all.x=TRUE) %>%
  dplyr::mutate(station_name <- factor(station_name, 
                                       levels = c("gtmpiwq", 
                                                  "gtmsswq", 
                                                  "gtmfmwq", 
                                                  "gtmpcwq")))

# rename dataframe
df_veg <- df3_veg_all
# -----------------remove clutter, part 1
rm(cols.num, df_vegold, df2_vegold, df2_veg)
# --------------------------------remove clutter, part 2
rm(swmp_wq, df_veg_all, df2_veg_all, df3_veg_all)