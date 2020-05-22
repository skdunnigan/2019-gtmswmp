# moses transects

# stacked graphs with x = plot, y = percentcover & canopy height
# facet by year

se <- function(x) {
  sd(x)/sqrt(length(x))
}


dat_veg_moses %>%
  