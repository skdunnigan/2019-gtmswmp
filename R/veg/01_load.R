# ----01 read in all the years of veg data - stored as separate files ----

read_function <- function(x) {
  readxl::read_xlsx(here::here('data', 'VEG', paste('GTMVEG',x,'_m2.xlsx', sep = ""))) %>%
  janitor::clean_names() %>%
  dplyr::select(-reserve, -type, -diameter, -height, -subplot) 
}

dat2012 <- read_function(2012)
dat2013 <- read_function(2013)
dat2014 <- read_function(2014)
dat2015 <- read_function(2015)
dat2016 <- read_function(2016)
dat2017 <- read_function(2017)
dat2018 <- read_function(2018) %>%
  dplyr::select(-orthometric_height, -height_relative_to_mllw, -ssam_1)
dat2019 <- read_function(2019) %>%
  dplyr::select(-orthometric_height, -height_relative_to_mllw, -ssam_1) %>%
  dplyr::rename(percent_cover = cover)

# ----02 now combine them into one data file...in steps----

# for years 2012-2017 they are all in one particular format, and the files are really weird.
# we're going to just make sure the class of each column is standard in all the files
# lots of copy and paste here because I could not get for loops or a function to work properly (2020-05-21)

cols.num <- c("lat", "long", "plot_id", "distance",
              "elevation", "rep", "percent_cover", "density", "canopy_height")
cols.char <- c("site_id", "transect_id", "species")

dat2012[cols.num] <- sapply(dat2012[cols.num], as.numeric)
dat2012[cols.char] <- sapply(dat2012[cols.char], as.character)

dat2013[cols.num] <- sapply(dat2013[cols.num], as.numeric)
dat2013[cols.char] <- sapply(dat2013[cols.char], as.character)

dat2014[cols.num] <- sapply(dat2014[cols.num], as.numeric)
dat2014[cols.char] <- sapply(dat2014[cols.char], as.character)

dat2015[cols.num] <- sapply(dat2015[cols.num], as.numeric)
dat2015[cols.char] <- sapply(dat2015[cols.char], as.character)

dat2016[cols.num] <- sapply(dat2016[cols.num], as.numeric)
dat2016[cols.char] <- sapply(dat2016[cols.char], as.character)

dat2017[cols.num] <- sapply(dat2017[cols.num], as.numeric)
dat2017[cols.char] <- sapply(dat2017[cols.char], as.character)


  # merge 2012-2017 data 
  dat_veg_old <- dplyr::bind_rows(dat2012, dat2013, dat2014, dat2015, dat2016, dat2017) %>%
    dplyr::select(-elevation)

  # remove 2012-2017 individual dfs
  rm(dat2012, dat2013, dat2014, dat2015, dat2016, dat2017)


# now, we are also going to do the same class thing with the 2018-2019 files, but they don't have the elevation column
cols.num <- c("lat", "long", "plot_id", "distance",
              "rep", "percent_cover", "density", "canopy_height")
cols.char <- c("site_id", "transect_id", "species")

dat2018[cols.num] <- sapply(dat2018[cols.num], as.numeric)
dat2018[cols.char] <- sapply(dat2018[cols.char], as.character)

dat2019[cols.num] <- sapply(dat2019[cols.num], as.numeric)
dat2019[cols.char] <- sapply(dat2019[cols.char], as.character)

  # merge 2018-2019 data
  dat_veg_new <- dplyr::bind_rows(dat2018, dat2019)
  
  # remove 2018-2019 individual dfs
  rm(dat2018, dat2019)


# combine both data frames into one for vegetation
dat_veg <- dplyr::bind_rows(dat_veg_old, dat_veg_new)

# clean up environment 
rm(dat_veg_old, dat_veg_new, cols.num, cols.char, read_function)
