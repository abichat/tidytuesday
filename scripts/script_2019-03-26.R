library(tidyverse)
library(ggwordcloud)
library(patchwork)

#### Data ####

seattle_pets <- read_csv("data/data_2019-03-26.csv", col_types = "ccccccc")


#### Table ####

data_count <-
  seattle_pets %>%
  drop_na(animals_name) %>%
  count(species, animals_name, sort = TRUE) %>%
  filter(n > 5) %>%
  group_split(species)


#### Plots ####

p_cat <-
  ggplot(data_count[[1]]) +
  aes(label = animals_name, size = n, color = n) +
  geom_text_wordcloud_area(mask = png::readPNG("ressources/img_2019-03-26_cat.png"), 
                           rm_outside = TRUE) +
  theme_minimal() +
  scale_color_gradient(low = "darkblue", high = "blue")

p_dog <-
  ggplot(data_count[[2]]) +
  aes(label = animals_name, size = n, color = n) +
  geom_text_wordcloud_area(mask = png::readPNG("ressources/img_2019-03-26_dog.png"), 
                           rm_outside = TRUE) +
  theme_minimal() +
  scale_color_gradient(low = "darkred", high = "red")

p_all <-
  p_dog + p_cat + 
  plot_annotation(title = "Most used names for dogs and cats in Seattle",
                  caption = "Source: Seattle's open data portal\n@_abichat for #TidyTuesday",
                  theme = theme(text = element_text(size = 12, family = "Arial Rounded MT Bold"),
                                plot.title = element_text(hjust = 0.5, face = "bold", 
                                                          size = 20, lineheight = 0.1)))

set.seed(42)
ggsave(plot = p_all, "plots/plot_2019-03-26.png", width = 29, height = 12, units = "cm", dpi = "retina")

