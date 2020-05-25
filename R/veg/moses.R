# moses transects

# stacked graphs with x = plot, y = percentcover & canopy height
# facet by year

se <- function(x) {
  sd(x, na.rm = TRUE)/sqrt(length(x))
}

# all three transects averaged together ----

moses <- dat_veg_moses %>%
  dplyr::filter(!plot_id %in% c(4,5)) %>% # remove plots 4 & 5 since they are replicates of 2 & 3
  dplyr::filter(canopy_height <= 3) %>% # appears to be a typo in canopy height in the 2019 data for Juncus on plot 12, so I removed the super large value from the analysis.
  dplyr::group_by(year, species, plot_id) %>%
  dplyr::summarise(mean_cover = mean(percent_cover, na.rm = TRUE),
                   se_cover = se(percent_cover),
                   mean_ht = mean(canopy_height, na.rm = TRUE),
                   se_ht = se(canopy_height))
  
  
moses %>%
  ggplot(aes(x = plot_id, y = mean_cover, group = species, color = species)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymin = mean_cover-se_cover, ymax = mean_cover+se_cover, width = 0.3)) +
  facet_grid(year~., switch = "y") +
  scale_x_continuous(breaks = c(1:3, 6:20)) +
  scale_color_manual(name = "Species",
                     values = speciescolours_all,
                     labels = expression(italic('Avicennia germinans'), # this allows you to have italicized species names in legend, but not italicize Unvegetated
                                         italic('Batis maritima'),
                                         italic('Distichlis spicata'),
                                         italic('Juncus roemerianus'),
                                         italic('Salicornia ambigua'),
                                         italic('Serenoa repens'),
                                         italic('Spartina alterniflora'),
                                         'Unidentified',
                                         'Unvegetated')
  ) +
  theme_classic() +
  theme(strip.placement = "outside",
        legend.text.align = 0,
        strip.background = element_rect(fill = NULL, 
                                        color = "white"),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)) +
  labs(x = "Plot",
       y = "Mean Cover (%)")


moses %>%
  dplyr::filter(species != "Unvegetated") %>% # remove the unvegetated values (which were reported as zero in 2015)
  ggplot(aes(x = plot_id, y = mean_ht, group = species, color = species)) +
  geom_point(size = 3) +
  geom_line(size = 1) +
  geom_errorbar(aes(ymin = mean_ht-se_ht, ymax = mean_ht+se_ht, width = 0.3)) +
  facet_grid(year~., switch = "y") +
  scale_x_continuous(breaks = c(1:3, 6:20)) +
  scale_color_manual(name = "Species",
                     values = speciescolours_all,
                     labels = expression(italic('Avicennia germinans'), # this allows you to have italicized species names in legend, but not italicize Unvegetated
                                         italic('Batis maritima'),
                                         italic('Distichlis spicata'),
                                         italic('Juncus roemerianus'),
                                         italic('Salicornia ambigua'),
                                         italic('Serenoa repens'),
                                         italic('Spartina alterniflora'),
                                         'Unidentified')
  ) +
  theme_classic() +
  theme(strip.placement = "outside",
        legend.text.align = 0,
        strip.background = element_rect(fill = NULL, 
                                        color = "white"),
        strip.text = element_text(size = 16),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)) +
  labs(x = "Plot",
       y = "Mean Height (m)")  

# by individual transects (fxn)----
# keep in mind that the transects are  B - A - C from West to East.

moses_transect <- dat_veg_moses %>%
  dplyr::filter(!plot_id %in% c(4,5)) %>% # remove plots 4 & 5 since they are replicates of 2 & 3
  dplyr::group_by(year, species, plot_id, transect_id)

moses_transect <- function(transect, parameter) {
  
  moses_transect <- dat_veg_moses %>%
    dplyr::filter(!plot_id %in% c(4,5)) %>% # remove plots 4 & 5 since they are replicates of 2 & 3
    dplyr::group_by(year, species, plot_id, transect_id) %>%
    dplyr::filter(transect_id == transect)
  
  if (parameter == "percent_cover") {
    
    moses_transect %>%
      ggplot(aes(x = plot_id, y = percent_cover, group = species, color = species)) +
      geom_point(size = 3) +
      geom_line(size = 1) +
      facet_grid(year~., switch = "y") +
      scale_x_continuous(breaks = c(1:3, 6:20)) +
      scale_color_manual(name = "Species",
                         values = speciescolours_all,
                         labels = expression(italic('Avicennia germinans'), # this allows you to have italicized species names in legend, but not italicize Unvegetated
                                             italic('Batis maritima'),
                                             italic('Distichlis spicata'),
                                             italic('Juncus roemerianus'),
                                             italic('Salicornia ambigua'),
                                             italic('Serenoa repens'),
                                             italic('Spartina alterniflora'),
                                             'Unidentified',
                                             'Unvegetated')
      ) +
      theme_classic() +
      theme(strip.placement = "outside",
            legend.text.align = 0,
            strip.background = element_rect(fill = NULL, 
                                            color = "white"),
            strip.text = element_text(size = 16),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 14)) +
      labs(x = "Plot",
           y = "Mean Cover (%)",
           title = paste("Transect:",transect))
    
  } else if (parameter == "canopy_height") {
    
    moses_transect %>%
      dplyr::filter(canopy_height <= 3 & species != "Unvegetated") %>% # remove the unvegetated values (which were reported as zero in 2015)
      ggplot(aes(x = plot_id, y = canopy_height, group = species, color = species)) +
      geom_point(size = 3) +
      geom_line(size = 1) +
      facet_grid(year~., switch = "y") +
      scale_x_continuous(breaks = c(1:3, 6:20)) +
      scale_color_manual(name = "Species",
                         values = speciescolours_all,
                         labels = expression(italic('Avicennia germinans'), # this allows you to have italicized species names in legend, but not italicize Unvegetated
                                             italic('Batis maritima'),
                                             italic('Distichlis spicata'),
                                             italic('Juncus roemerianus'),
                                             italic('Salicornia ambigua'),
                                             italic('Serenoa repens'),
                                             italic('Spartina alterniflora'),
                                             'Unidentified')
      ) +
      theme_classic() +
      theme(strip.placement = "outside",
            legend.text.align = 0,
            strip.background = element_rect(fill = NULL, 
                                            color = "white"),
            strip.text = element_text(size = 16),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 14)) +
      labs(x = "Plot",
           y = "Mean Height (m)",
           title = paste("Transect:",transect))
    
  }
  
  
  
}

# transect B needs to be done on it's own. It is very different from A and C.

moses_transect(transect = "B", parameter = "canopy_height")
moses_transect(transect = "A", parameter = "canopy_height")
moses_transect(transect = "C", parameter = "canopy_height")