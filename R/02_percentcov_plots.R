# resource (https://www.r-graph-gallery.com/piechart-ggplot2.html)
# veg data percent cover of species spring vs. fall
sitecolours <- c(
  "PI" = "",
  "HI" = "", 
  "EC" = "",
  "MC" = "",
  "PC" = "",
  "WO" = ""
)

speciescolours <- c(
  `Avicennia germinans` = "#5488af",
  `Batis maritima` = "#a9b645",
  `Juncus roemerianus` = "#a4a4a6",
  `Sarcocornia perennis` = "#f4c544",
  `Spartina alterniflora` = "#ce5c36",
  Unvegetated = "#325269"
)

# --------------------------------------only interest year data
pct_cov_seasonYRcomparison <- function(interestyear) {

# average all plots together
df_veg %>%
  dplyr::filter(year == interestyear) %>%
  dplyr::group_by(season, site_name, species) %>%
  dplyr::summarise(mean_pc = mean(percent_cover, na.rm = TRUE)) %>%
  ggplot() +
  geom_bar(aes(x = "", y = mean_pc, fill = species), 
           color = "white", 
           width = 1, 
           stat = "identity") +
  coord_polar("y", start = 0) +
  facet_grid(season ~ site_name) + # reverse these to flip it vertical
  scale_fill_manual(values = speciescolours) +
  theme_void() +
  labs(title = interestyear)+
  theme(legend.position = "top")
}

# plot 2019
pct_cov_seasonYRcomparison(2019)




# ------------------------------------------------this year compared to previous
pct_cov_2019vs <- function(season_int) {

df_veg %>%
  dplyr::mutate(interestYR = ifelse(year < 2019, "2012-2018", "2019")) %>%
  dplyr::filter(season == season_int) %>%
  dplyr::group_by(interestYR, site_name, species) %>%
  dplyr::summarise(mean_pc = mean(percent_cover, na.rm = TRUE)) %>%
  ggplot() +
  geom_bar(aes(x = "", y = mean_pc, fill = species), 
           color = "white", 
           width = 1, 
           stat = "identity") +
  coord_polar("y", start = 0) +
  facet_grid(interestYR ~ site_name) + 
  scale_fill_manual(values = speciescolours) +
  theme_void() +
  theme(legend.position = "top") +
  labs(title = paste(season_int, "Season"))

}

# make spring
pct_cov_2019vs(season_int = "Spring") 

# make fall
pct_cov_2019vs(season_int = "Fall")
 
