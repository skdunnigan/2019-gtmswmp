# to work up amount of plots that are sampleable

dat_plots <- readxl::read_xlsx(here::here('data', 'VEG', 'sampleable-plots.xlsx')) %>%
  janitor::clean_names()

dat_plots %>%
  ggplot(aes(x = year, y = prop_of_plots)) +
  geom_bar(aes(fill = sampled), 
           width = 0.5, 
           stat = "identity",
           position = "fill") +
  scale_fill_manual(name = "",
                    values = c('darkturquoise', 'darkorange')) +
  scale_x_continuous(breaks = c(2012, 
                                2013,
                                2014,
                                2015,
                                2016,
                                2017,
                                2018,
                                2019)) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  theme(legend.position = "top",
        text = element_text(size = 14, color = "black"),
        axis.text = element_text(color = "black")) +
  labs(x = "Year",
       y = "Percentage of Plots")

# ggsave(p, filename = here::here('output', 'sampleable-plots.png'), dpi = 120, bg = "transparent") # make sure to save with transparent background