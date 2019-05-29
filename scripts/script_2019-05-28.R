library(ggpomological)
library(tidyverse)
library(ggridges)

#### Data ####

wine_ratings <- read_csv("data/data_2019-05-28.csv", col_types = "dcccddcccccccc")


#### Tables ####

best_french_wines <-
  wine_ratings %>%
  filter(country == "France") %>%
  select(country, variety, points) %>%
  group_by(variety) %>%
  summarise(mean = mean(points), n = n()) %>%
  filter(n > 50) %>%
  arrange(desc(mean), n) %>%
  head(9) %>%
  pull(variety)

df_best_french_wines <-
  wine_ratings %>%
  select(country, variety, points) %>%
  filter(country == "France", variety %in% best_french_wines) %>%
  mutate(variety = fct_reorder(variety, desc(points)))


#### Plot ####

ggplot(df_best_french_wines) +
  aes(x = points, y = variety) +
  geom_density_ridges(aes(fill = variety, color = variety), alpha = 0.9) +
  geom_text(aes(label = variety), x = 80, nudge_y = 0.3, 
            family = "Chopin Script", size = 7, hjust = 0) +
  scale_fill_pomological() +
  scale_color_pomological() +
  scale_x_continuous(limits = c(80, 100)) +
  expand_limits(y = c(NA, 10.7)) +
  labs(x = "Score", title = "Best French Wines", 
       caption = "Source: WineEnthusiast\n@_abichat for #TidyTuesday") +
  theme_pomological() +
  theme(text = element_text(family = "Chopin Script", size = 20), 
        plot.title = element_text(size = 30, hjust = 0.5), 
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_blank(), 
        axis.title.y = element_blank(), 
        legend.position = "none")

ggsave("plots/plot_2019-05-28.png", width = 29, height = 21, units = "cm", dpi = "retina")

