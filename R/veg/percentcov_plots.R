# resource (https://www.r-graph-gallery.com/piechart-ggplot2.html)



# coverage for all
# average all plots together for all years and all seasons
dat_veg_all %>%
  dplyr::group_by(site_name, species) %>%
  dplyr::summarise(mean_pc = mean(percent_cover, na.rm = TRUE)) %>%
  ggplot() +
  geom_bar(aes(x = "", y = mean_pc, fill = species), 
           color = "white", 
           width = 1, 
           stat = "identity",
           position = "fill") + # by setting position = "fill" this will fill up any gaps caused by averaging percent cover measurements
  coord_polar("y", start = 0) +
  facet_grid(site_name ~.,) + # reverse these to flip it vertical
  scale_fill_manual(name = "Species",
                    values = speciescolours_all, 
                    labels = expression(italic('Avicennia germinans'), # this allows you to have italicized species names in legend, but not italicize Unvegetated
                                        italic('Batis maritima'),
                                        italic('Distichlis spicata'),
                                        italic('Juncus roemerianus'),
                                        italic('Salicornia ambigua'),
                                        italic('Spartina alterniflora'),
                                        'Unvegetated')
                    ) +
  theme_void() +
  theme(legend.position = "right",
        legend.text.align = 0) # align species names in legend to the left


# --------------------------------------only interest year data
pct_cov_seasonYRcomparison <- function(interestyear) {
  
  # average all plots together
  dat_veg_all %>%
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
dat_veg_all %>%
  dplyr::mutate(interestYR = ifelse(year < 2019, "2012-2018", "2019")) %>%
  dplyr::group_by(interestYR, site_name, species) %>%
  dplyr::summarise(mean_pc = mean(percent_cover, na.rm = TRUE)) %>%
  ggplot() +
  geom_bar(aes(x = "", y = mean_pc, fill = species), 
           color = "white", 
           width = 1, 
           stat = "identity",
           position = "fill") +
  coord_polar("y", start = 0) +
  facet_grid(site_name ~ interestYR) + 
  scale_fill_manual(name = "Species",
                    values = speciescolours_all, 
                    labels = expression(italic('Avicennia germinans'), # this allows you to have italicized species names in legend, but not italicize Unvegetated
                                        italic('Batis maritima'),
                                        italic('Distichlis spicata'),
                                        italic('Juncus roemerianus'),
                                        italic('Salicornia ambigua'),
                                        italic('Spartina alterniflora'),
                                        'Unvegetated')
  ) +
  theme_void() +
  theme(legend.position = "right",
        legend.text.align = 0) # align species names in legend to the left 


# ---- function for specific season ----

pct_cov_2019vs <- function(season_int) {

dat_veg_all %>%
  dplyr::mutate(interestYR = ifelse(year < 2019, "2012-2018", "2019")) %>%
  dplyr::filter(season == season_int) %>%
  dplyr::group_by(interestYR, site_name, species) %>%
  dplyr::summarise(mean_pc = mean(percent_cover, na.rm = TRUE)) %>%
  ggplot() +
  geom_bar(aes(x = "", y = mean_pc, fill = species), 
           color = "white", 
           width = 1, 
           stat = "identity",
           position = "fill") +
  coord_polar("y", start = 0) +
  facet_grid(interestYR ~ site_name) + 
    scale_fill_manual(name = "Species",
                      values = speciescolours_all, 
                      labels = expression(italic('Avicennia germinans'), # this allows you to have italicized species names in legend, but not italicize Unvegetated
                                          italic('Batis maritima'),
                                          italic('Distichlis spicata'),
                                          italic('Juncus roemerianus'),
                                          italic('Salicornia ambigua'),
                                          italic('Spartina alterniflora'),
                                          'Unvegetated')
    ) +
  theme_void() +
  theme(legend.position = "top",
        legend.text.align = 0) +
  labs(title = paste(season_int, "Season"))

}

# make spring
pct_cov_2019vs(season_int = "Spring") 

# make fall
pct_cov_2019vs(season_int = "Fall")
 
