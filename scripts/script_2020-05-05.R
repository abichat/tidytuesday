library(tidyverse)
library(ggimage)

#### Data ####

items <- read_csv("data/data_2020-05-05.csv", col_types = "dcccldcdccldcccc")


#### Tables ####

craftables <-
  items %>% 
  select(recipe, recipe_id) %>% 
  drop_na(recipe_id) %>% 
  count(recipe_id, wt = recipe, sort = TRUE) 

materials <- 
  items %>% 
  filter(id %in% craftables$recipe_id) %>% 
  select(id, sell_value, image_url) %>% 
  unique()

craftables_processed <-
  craftables %>% 
  filter(n > 9, ! str_detect(recipe_id, "egg")) %>% 
  left_join(materials, by = c("recipe_id"  = "id")) %>% 
  drop_na(image_url) %>% 
  mutate(recipe_id = str_replace_all(recipe_id, "-", " "),
         recipe_id = str_to_title(recipe_id),
         recipe_id = fct_reorder(recipe_id, -n),
         round_n = ceiling(n / 10) * 10,
         n_fill = map2(10, round_n, seq, by = 10)) %>% 
  unnest(n_fill) 


#### Plot ####

ggplot(craftables_processed) +
  aes(x = recipe_id, y = n_fill) +
  geom_image(aes(image = image_url)) +
  geom_text(x = 18.5, y = 260, size = 11, 
            family = "FinkHeavy",  color = "#729A7F", 
            label = "How many materials do you need") +
  geom_text(x = 17, y = 260, size = 11, 
            family = "FinkHeavy", color = "#729A7F", 
            label = "to craft all DIY recipes once?") +
  geom_image(x = 14.5, y = 260, size = 0.1, asp = 1.5,
             image = "ressources/img_2020-05-05.png") +
  annotate(geom = "text", x = -1, y = -80, family = "FinkHeavy", 
           color = "#729A7F", hjust = 0,
           label = "Only the most common materials are displayed, events excluded") +
  scale_y_continuous(breaks = seq(0, 400, by = 50)) +
  coord_flip(xlim = c(1, NA), ylim = c(10, NA), clip = "off") +
  labs(x = NULL, y = NULL,
       caption = "Source: villagerdb.com\n@_abichat for #TidyTuesday") +
  theme_minimal() +
  theme(text = element_text(family = "FinkHeavy", color = "#729A7F"), 
        plot.caption = element_text(size = 12),
        axis.text = element_text(color = "#729A7F"),
        axis.text.x = element_text(size = 15),
        axis.text.y = element_text(size = 12),
        plot.background = element_rect(fill = "#CCE2CF", color = NA), 
        panel.grid = element_line(color = "#FDE3B1", size = 0.3))

ggsave("plots/plot_2020-05-05.png", width = 29, height = 21, units = "cm", dpi = "retina")

