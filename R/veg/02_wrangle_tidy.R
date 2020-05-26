# script for wrangling and tidying the vegetation dataframe

# ----01 species names continuity----
# this is especially important for Sarcocornia since it has been renamed a few times
# see this reference https://plants.sc.egov.usda.gov/core/profile?symbol=SAPE11
# and this reference https://itis.gov/servlet/SingleRpt/SingleRpt?search_topic=TSN&search_value=520732
species_names <- tibble(
  species = c("Spartina alterniflora", "Batis maritima", 
              "Sarcocornia ambigua", "Sarcocornia perennis", 
              "Avicennia germinans", "Juncus roemerianus", 
              "Distichlis spicata", "Serenoa repens", 
              "Morella cerifera", "Unvegetated", "Unidentified"),
  species_names = c("Spartina alterniflora", "Batis maritima", 
                    "Sarcocornia perennis", "Sarcocornia perennis", 
                    "Avicennia germinans", "Juncus roemerianus", 
                    "Distichlis spicata", "Serenoa repens", 
                    "Morella cerifera", "Unvegetated", "Unidentified")
)

dat_veg <- merge(dat_veg, species_names, by = "species", all.x = TRUE) %>%
  dplyr::select(-species) %>%
  dplyr::rename(species = species_names) %>%
  dplyr::filter(!is.na(species))

rm(species_names)

# ---- 02 add site names instead of the numbered site names, also set their levels----
sites <- tibble(
  site_id = c("0", "00", "1", "01", "6", "06", "22", "40", "46"),
  site_name = c("HI", "HI", "WO", "WO", "MC", "MC", "EC", "PI", "PC")
)

dat_veg <- merge(dat_veg, sites, by = "site_id", all.x = TRUE) %>%
  mutate(site_name = factor(site_name, levels = c("PI", 
                                                  "HI", 
                                                  "EC", 
                                                  "MC", 
                                                  "PC", 
                                                  "WO")))

rm(sites)

# ----03 dates and times----
dat_veg <- dat_veg %>%
  dplyr::mutate(date = lubridate::date(date),
                year = lubridate::year(date),
                month = lubridate::month(date),
                season = ifelse(month > 4, "Fall", "Spring"),
                season = factor(season, levels = c("Spring", "Fall")))

# ----04 create dataframe for veg data of only the platform plots (1-5)----

dat_veg_all <- dat_veg %>%
  dplyr::filter(plot_id <= 5)

# ----05 create dataframe for veg data of only the moses long transect plots----

dat_veg_moses <- dat_veg %>%
  dplyr::filter(site_name == "MC", year %in% c(2012, 2015, 2019))