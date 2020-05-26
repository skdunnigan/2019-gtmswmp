# function to examine cover at a particular site

se <- function(x) {
  sd(x, na.rm = TRUE)/sqrt(length(x))
}
# parameters = percent_cover or canopy_height
site_plot <- function(site, parameter) {

  a <- dat_veg_all %>%
  dplyr::filter(site_name == site) %>%
  dplyr::group_by(year, season, species) %>%
  dplyr::summarise(mean_cover = mean(percent_cover, na.rm = TRUE),
                   se_cover = se(percent_cover),
                   mean_ht = mean(canopy_height, na.rm = TRUE),
                   se_ht = se(canopy_height)) %>%
  dplyr::mutate(year_season = paste(season, year),
                year_season = factor(year_season, levels = c('Fall 2012',
                                                             'Spring 2013', 
                                                             'Fall 2013', 
                                                             'Spring 2014', 
                                                             'Fall 2014', 
                                                             'Spring 2015', 
                                                             'Fall 2015', 
                                                             'Spring 2016', 
                                                             'Fall 2016', 
                                                             'Spring 2017', 
                                                             'Fall 2017', 
                                                             'Spring 2018', 
                                                             'Fall 2018', 
                                                             'Spring 2019', 
                                                             'Fall 2019'))) 

levels(a$year_season) <- gsub(" ", "\n", levels(a$year_season))  # force a break between the season and year for better axis label

  if (parameter == "percent_cover") {

    b <- ggplot(data = a, aes(x = year_season, y = mean_cover, group = species, color = species)) +
          geom_point(size = 3) +
          geom_line(size = 1) +
          geom_errorbar(aes(x = year_season, ymin = mean_cover-se_cover, ymax = mean_cover+se_cover, width = 0.3)) +
          scale_color_manual(name = "Species",
                             values = speciescolours_all,
                             labels = expression(italic('Avicennia germinans'), # this allows you to have italicized species names in legend, but not italicize Unvegetated
                                                 italic('Batis maritima'),
                                                 italic('Distichlis spicata'),
                                                 italic('Juncus roemerianus'),
                                                 italic('Sarcocornia perennis'),
                                                 italic('Spartina alterniflora'),
                                                 'Unvegetated')
          ) +
          theme_cowplot() +
          theme(legend.position = "top",
                legend.text.align = 0) + # align species names in legend to the left 
          labs(x = "",
               y = "Mean Cover (%)",
               title = paste("Site:",site))
        # print(a)
        print(b)


  } else if (parameter == "canopy_height") {
  
  
  b <- ggplot(data = a, aes(x = year_season, y = mean_ht, group = species, color = species)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    geom_errorbar(aes(x = year_season, ymin = mean_ht-se_ht, ymax = mean_ht+se_ht, width = 0.3)) +
    scale_color_manual(name = "Species",
                       values = speciescolours_all,
                       labels = expression(italic('Avicennia germinans'), # this allows you to have italicized species names in legend, but not italicize Unvegetated
                                           italic('Batis maritima'),
                                           italic('Distichlis spicata'),
                                           italic('Juncus roemerianus'),
                                           italic('Sarcocornia perennis'),
                                           italic('Spartina alterniflora'),
                                           'Unvegetated')
    ) +
    theme_cowplot() +
    theme(legend.position = "top",
          legend.text.align = 0) + # align species names in legend to the left 
    labs(x = "",
         y = "Mean Canopy Height (m)",
         title = paste("Site:",site))
  
  # print(a)
  print(b)
  
  }

}


# example 
# site_plot(site = "PI", parameter = "percent_cover")