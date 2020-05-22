# starting analysis

# ----01 load in all data and packages----
source('R/00_loadpackages.R') #loads all packages
source('R/01.1_load_wrangle_nut.R') # reads in 2019 nutrient data
source('R/01.2_load_wrangle_wq.R') # reads in 2005-2019 water quality data
source('R/01.3_load_wrangle_veg.R') # reads in 2012-2018 and 2019 veg data and merges them into one
source('R/01.4_load_wrangle_met.R') # reads in 2005-2019 meteorological data

beepr::beep() # play sound when complete

# ----02 