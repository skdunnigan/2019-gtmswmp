
# ----01 LOAD nutrient data----

# load GTMNERR SWMP nutrient dataset
# load file with CDMO SWMP names

df_nut <- readxl::read_xlsx(here::here('data', '2019_Nutrients_12.xlsx'), 
                         sheet = "Chemistry") %>% 
  janitor::clean_names()

names <- readr::read_csv(here::here('data', 'componentnames.csv')) %>% 
  janitor::clean_names()

# inspect the data
glimpse(df_nut)


# remove component_short
# force names to lowercase 
# remove any spaces in station_code
# merge cdmo names file with dataframe to convert
# set cdmo_name to factor
# separate station_code into station_code, monitoringprogram, and replicate
# clean data to only columns of interest

df2_nut <- df_nut %>%
  dplyr::select(-component_short) %>%
  dplyr::mutate(station_code = tolower(station_code),
                station_code = gsub(" ","", station_code), # removes any spaces in the names
                site = tolower(site),
                component_long = tolower(component_long)) %>%
  dplyr::left_join(names, by = "component_long") %>%
  dplyr::mutate(cdmo_name = forcats::as_factor(cdmo_name)) %>%
  tidyr::separate(station_code, 
                  into = c("station_code", "num"), 
                  sep = "(?<=[A-Za-z])(?=[0-9])") %>%
  tidyr::separate(num,
                  into = c("monitoringprogram", "replicate"),
                  sep = "[.]") %>%
  dplyr::select(site, station_code, monitoringprogram, replicate, 
                date_sampled, component_long, cdmo_name, result)
# rewrite data 
df_nut <- df2_nut

# remove data frames
rm(names, df2_nut)

#----02 LOAD water quality data ----
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


# ----03 LOAD biomonitoring vegetation data----
df_veg <- readxl::read_xlsx(here::here('data', '2019VEG_raw.xlsx')) %>% 
                              janitor::clean_names()

# edit column names
# remove unnecessary columns
df2_veg <- df_veg %>%
  dplyr::rename(canopy_height_cm = canopy_height_15,
                canopy_height_m = canopy_height_16) %>%
  dplyr::select(date, site_id, transect_id, plot_id, distance, elevation,
                species, percent_cover, canopy_height_m, density_adj)
