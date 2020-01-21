# veg data percent cover of species spring vs. fall
sitecolours <- c(
  "40" = "",
  "00" = "", 
  "22" = "",
  "06" = "",
  "46" = "",
  "01" = ""
)

speciescolours <- c(
  `Avicennia germinans` = "#5488af",
  `Batis maritima` = "#a9b645",
  `Juncus roemerianus` = "#a4a4a6",
  `Sarcocornia perennis` = "#f4c544",
  `Spartina alterniflora` = "#ce5c36",
  Unvegetated = "#325269"
)

# average all plots together
df2_veg %>%
  dplyr::mutate(month = lubridate::month(date),
                season = ifelse(month >4, "fall", "spring"),
                season = as.factor(season),
                season = factor(season, levels = c("spring", "fall"))) %>%
  dplyr::group_by(season, site_id, species) %>%
  dplyr::summarise(mean_pc = mean(percent_cover, na.rm = TRUE)) %>%
  ggplot() +
  geom_bar(aes(x = "", y = mean_pc, fill = species), color = "white", width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  facet_grid(site_id ~ season) + 
  scale_fill_manual(values = speciescolours) +
  theme_void()

# flip it horizontal
df2_veg %>%
  dplyr::mutate(month = lubridate::month(date),
                season = ifelse(month >4, "fall", "spring"),
                season = as.factor(season),
                season = factor(season, levels = c("spring", "fall"))) %>%
  dplyr::group_by(season, site_id, species) %>%
  dplyr::summarise(mean_pc = mean(percent_cover, na.rm = TRUE)) %>%
  ggplot() +
  geom_bar(aes(x = "", y = mean_pc, fill = species), color = "white", width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  facet_grid(season ~ site_id) + 
  scale_fill_manual(values = speciescolours) +
  theme_void() +
  theme(legend.position = "top")