# stamdard error function
se<- function(x) sd(x)/sqrt(length(x))

SET <- read_csv("Data Subsets/2013-19SET.csv")

SET <- readr::read_csv(here::here('data', '2013-19SET.csv')) %>% 
  janitor::clean_names()

PI <- "40"
HI <- "0"
EC <- "22"
MC <- "6"
PC <- "46"
WO <- "1"

# bind the vectors into a data frame
veg_names <- bind_cols("PI" = PI, 
                       "HI" = HI,
                       "EC" = EC,
                       "MC" = MC,
                       "PC" = PC,
                       "WO" = WO) %>%
  gather(key = "site_name", value = "site") %>%
  mutate(site = as.numeric(site))


# set site and station to factors
# set season to factor with levels
SET.1 <- SET %>%
  dplyr::left_join(veg_names, by = "site") %>%
  mutate(site = factor(site),
         station = factor(station),
         season = factor(season, levels = c('Spring 2013', 
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
                                            'Fall 2019')),
         site_name = factor(site_name, 
                             levels = c("PI",
                                        "HI",
                                        "EC",
                                        "MC",
                                        "PC",
                                        "WO"))) 

SET.2 <- SET %>%
  dplyr::left_join(veg_names, by = "site") %>%
  tidyr::separate(season, 
                  into = c("season", "year"), 
                  sep = "(?<=[A-Za-z]) (?=[0-9])") %>%
  mutate(year = as.numeric(year),
         site = factor(site),
         station = factor(station),
         season = factor(season, levels = c('Spring', 'Fall')),
         site_name = factor(site_name, 
                            levels = c("PI",
                                       "HI",
                                       "EC",
                                       "MC",
                                       "PC",
                                       "WO"))) 

levels(SET.1$season) <- gsub(" ", "\n", levels(SET.1$season))  # force a break between the season and year for better axis label

SET.1 %>%  
  group_by(season, site_name) %>%
  summarise(ht = mean(height_adj),
            se = se(height_adj)) %>%
  ggplot(aes(season, ht, group = site_name, colour = site_name)) + 
    geom_line(size = 1) +
    geom_point(size = 3) +
    ylim(150, 280) +
    geom_errorbar(mapping = aes(x = season, ymin = ht-se, ymax = ht+se, width = 0.3)) +
    geom_smooth(method = "lm", fill = NA, linetype = 3, size=1) +
    stat_regline_equation(
        aes(label = paste(..eq.label.., ..rr.label.., sep = "~`, `~")),
        label.y.npc = 1) +
    theme_cowplot() +
    scale_color_discrete(name = "Site") +
    # scale_color_brewer(name = "Site", palette = 'Set1') +
    labs(x = '',
         y = 'Mean Height (mm)') +
  theme(legend.position = "top") +
  guides(col = guide_legend(nrow = 1))
ggsave(here::here('output', 'SETall.png'), 
       units = "in", 
       height = 5, 
       width = 8, 
       dpi = 300)

# to just map sites, use function and select site: PI, HI, EC, MC, PC, WO
site_set <- function (x) {

  SET.1 %>%
  filter(site_name == x) %>%
  group_by(season, station) %>%
  summarise(ht = mean(height_adj),
            se = se(height_adj)) %>%
  ggplot(aes(season, ht, group = station, colour = station)) + 
  geom_line(size = 1) +
  geom_point() +
  # ylim(150, 350) +
  geom_errorbar(mapping = aes(x = season, ymin = ht-se, ymax = ht+se, width = 0.3)) +
  geom_smooth(method = "lm", fill = NA, linetype = 2, size=1) +
  stat_regline_equation(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~`, `~")),
    label.y.npc = 1) +
  #scale_color_manual() +
  theme_classic()
}

site_set("EC")
site_set("MC")