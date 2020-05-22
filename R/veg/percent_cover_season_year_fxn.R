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