
# ----01 LOAD nutrient data-------------------------------------------------------

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

# make sure station_name is ordered
df_nut$station_code <- as.factor(df_nut$station_code)
df_nut$station_code <- factor(df_nut$station_code, 
                              levels = c("gtmpinut",
                                         "gtmssnut",
                                         "gtmfmnut",
                                         "gtmpcnut")
)



